//! Handling of activities, that is, clang-provided categories of stuff it can
//! be doing at a point in time

pub mod argument;

use self::argument::{ActivityArgumentType, RawActivityArgument};
use super::ArgParseError;
use crate::ctf::{events::duration::DurationEvent, Duration, Timestamp, TraceEvent};
use phf::phf_map;
use serde_json as json;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use thiserror::Error;

/// Clang activity with timing information
#[derive(Clone, Debug, PartialEq)]
pub struct ActivityStat {
    /// What clang was doing
    activity: Activity,

    /// When it started doing it
    start: Timestamp,

    /// How long it did it
    duration: Duration,
}
//
impl ActivityStat {
    /// Create an ActivityStat from scratch
    #[cfg(test)]
    pub(crate) fn new(activity: Activity, start: Timestamp, duration: Timestamp) -> Self {
        Self {
            activity,
            start,
            duration,
        }
    }

    /// Decode a TraceEvent which is expected to contain a timed activity
    pub fn parse(t: TraceEvent) -> Result<Self, ActivityStatParseError> {
        match t {
            TraceEvent::X {
                duration_event:
                    DurationEvent {
                        pid,
                        tid,
                        ts,
                        name: Some(name),
                        cat: None,
                        tts: None,
                        args,
                        stack_trace: None,
                    },
                dur,
                tdur: None,
                end_stack_trace: None,
            } if (pid == 1 && tid == 0) || tid == pid => {
                let activity = Activity::parse(name, args)?;
                Ok(Self {
                    activity,
                    start: ts,
                    duration: dur,
                })
            }
            _ => Err(ActivityStatParseError::UnexpectedInput(t)),
        }
    }

    /// What clang was doing
    pub fn activity(&self) -> &Activity {
        &self.activity
    }

    /// When it started doing it
    pub fn start(&self) -> Timestamp {
        self.start
    }

    /// How long it did it
    pub fn duration(&self) -> Duration {
        self.duration
    }

    /// When it stopped doing it
    pub fn end(&self) -> Timestamp {
        self.start + self.duration
    }
}

/// What can go wrong while parsing an activity profile
#[derive(Error, Debug, PartialEq)]
pub enum ActivityStatParseError {
    /// Encountered unexpected input
    #[error("attempted to parse ActivityStat from unexpected {0:#?}")]
    UnexpectedInput(TraceEvent),

    /// Failed to parse inner activity
    #[error("failed to parse activity ({0})")]
    BadArguments(#[from] ActivityParseError),
}

/// Activity that Clang can engage in during the compilation process
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Activity {
    /// Machine identifier that can be translated back into a human identifier
    pub(crate) id: ActivityId,

    /// Supplementary data received as an argument, if any
    pub(crate) arg: RawActivityArgument,
}
//
impl Activity {
    /// Activity identifier that is cheap to compare or use as a hashmap key
    pub fn id(&self) -> ActivityId {
        self.id
    }

    /// Textual name of the activity, as featured in the JSON data
    pub fn name(&self) -> &'static str {
        self.id.into()
    }

    /// Get the un-parsed argument of the activity
    ///
    /// For performance reasons, activity arguments only undergo minimal parsing
    /// upon loading, and the bulk of the work is saved for the moment where it
    /// is established with reasonable confidence that an activity argument
    /// will be displayed to the user.
    ///
    /// You can use `RawActivityArgument::parse()` to finish the parsing and get
    /// a ParsedActivityArgument (which should probably be cached by
    /// ActivityTraceId), and then `ParsedActivityArgument::resolve()` to get a
    /// usable (but temporary) view of that argument where needed.
    ///
    pub fn raw_argument(&self) -> &RawActivityArgument {
        &self.arg
    }

    /// Parse from useful bits of Duration events
    fn parse(
        name: Box<str>,
        mut args: Option<HashMap<Box<str>, json::Value>>,
    ) -> Result<Self, ActivityParseError> {
        // Handling of activity name
        let (id, arg_type) = if let Some(activity) = ACTIVITIES.get(&name) {
            activity
        } else {
            return Err(ActivityParseError::UnknownActivity(
                name.clone(),
                args.take(),
            ));
        };
        let arg_type = arg_type.clone();

        // Interior mutability to allow multiple mutable borrows
        let args = RefCell::new(args);

        // Handling of activities' "detail" argument
        let detail_arg = || -> Result<RawActivityArgument, ArgParseError> {
            Self::parse_detail_arg(args.borrow_mut().take())
                .map(|detail| RawActivityArgument::new(arg_type, Some(detail)))
        };
        //
        let arg_result = match arg_type {
            ActivityArgumentType::Nothing => Self::parse_empty_args(args.borrow_mut().take())
                .map(|()| RawActivityArgument::new(arg_type, None)),

            ActivityArgumentType::String
            | ActivityArgumentType::FilePath
            | ActivityArgumentType::CppEntity
            | ActivityArgumentType::MangledSymbol
            | ActivityArgumentType::UnnamedLoop => detail_arg(),

            ActivityArgumentType::MangledSymbolOpt | ActivityArgumentType::UnnamedLoopOpt => {
                match detail_arg() {
                    Ok(detail_arg) => Ok(detail_arg),
                    Err(ArgParseError::MissingKey("detail")) => {
                        Ok(RawActivityArgument::new(arg_type, None))
                    }
                    Err(e) => Err(e),
                }
            }
        };
        match arg_result {
            Ok(arg) => Ok(Self {
                id: id.clone(),
                arg,
            }),
            Err(e) => Err(ActivityParseError::BadArguments(id.clone(), e)),
        }
    }

    /// Check for absence of arguments
    fn parse_empty_args(args: Option<HashMap<Box<str>, json::Value>>) -> Result<(), ArgParseError> {
        if let Some(args) = args {
            if args.is_empty() {
                Ok(())
            } else {
                Err(ArgParseError::UnexpectedKeys(args))
            }
        } else {
            Ok(())
        }
    }

    /// Parse a single "detail" string argument
    fn parse_detail_arg(
        args: Option<HashMap<Box<str>, json::Value>>,
    ) -> Result<Rc<str>, ArgParseError> {
        if let Some(args) = args {
            let mut args_iter = args.into_iter();
            let collect_bad_args = |args_iter, (k, v)| {
                let mut remainder = HashMap::from_iter(args_iter);
                remainder.insert(k, v);
                remainder.remove("detail");
                remainder
            };
            if let Some((k, v)) = args_iter.next() {
                if &*k == "detail" {
                    if let json::Value::String(s) = v {
                        let s = s.into();
                        if let Some(kv) = args_iter.next() {
                            Err(ArgParseError::UnexpectedKeys(collect_bad_args(
                                args_iter, kv,
                            )))
                        } else {
                            Ok(s)
                        }
                    } else {
                        Err(ArgParseError::UnexpectedValue("detail", v))
                    }
                } else {
                    Err(ArgParseError::UnexpectedKeys(collect_bad_args(
                        args_iter,
                        (k, v),
                    )))
                }
            } else {
                Err(ArgParseError::MissingKey("detail"))
            }
        } else {
            Err(ArgParseError::MissingKey("detail"))
        }
    }
}

/// Generate a PHF that maps from clang activity names (as seen in time-trace
/// files) to a unique identifier (ActivityId) and an ActivityArgumentParsing
/// that tells how the activity argument should be interpreted.
macro_rules! generate_activities {
    ($($string:literal => ($enum:ident, $arg:ident)),* $(,)?) => {
        /// Clang activity identifier
        #[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, strum::IntoStaticStr, strum::Display)]
        pub enum ActivityId {
            $(
                #[doc = $string]
                #[strum(to_string = $string)]
                $enum
            ),*
        }

        /// Map from a clang activity name to its identifier and argument type
        static ACTIVITIES: phf::Map<&'static str, (ActivityId, ActivityArgumentType)> = phf_map! {
            $(
                $string => (ActivityId::$enum, ActivityArgumentType::$arg)
            ),*
        };
    };
}
//
generate_activities! {
    "Source" => (Source, FilePath),
    "ParseClass" => (ParseClass, CppEntity),
    "InstantiateClass" => (InstantiateClass, CppEntity),
    "ParseTemplate" => (ParseTemplate, CppEntity),
    "InstantiateFunction" => (InstantiateFunction, CppEntity),
    "DebugType" => (DebugType, CppEntity),
    "DebugGlobalVariable" => (DebugGlobalVariable, CppEntity),
    "CodeGen Function" => (CodeGenFunction, CppEntity),
    "DebugFunction" => (DebugFunction, CppEntity),
    "PerformPendingInstantiations" => (PerformPendingInstantiations, Nothing),
    "Frontend" => (Frontend, Nothing),
    "RunPass" => (RunPass, String),
    "OptFunction" => (OptFunction, MangledSymbol),
    "PerFunctionPasses" => (PerFunctionPasses, Nothing),
    "RunLoopPass" => (RunLoopPass, String),
    "OptModule" => (OptModule, FilePath),
    "PerModulePasses" => (PerModulePasses, Nothing),
    "CodeGenPasses" => (CodeGenPasses, Nothing),
    "Backend" => (Backend, Nothing),
    "ExecuteCompiler" => (ExecuteCompiler, Nothing),
    "InferFunctionAttrsPass" => (InferFunctionAttrsPass, FilePath),
    "PassManager<llvm::Function>" => (FunctionPassManager, MangledSymbolOpt),
    "SROAPass" => (SROAPass, MangledSymbol),
    "ModuleToFunctionPassAdaptor" => (ModuleToFunctionPassAdaptor, FilePath),
    "IPSCCPPass" => (IPSCCPPass, FilePath),
    "CalledValuePropagationPass" => (CalledValuePropagationPass, FilePath),
    "GlobalOptPass" => (GlobalOptPass, FilePath),
    "PromotePass" => (PromotePass, MangledSymbol),
    "DeadArgumentEliminationPass" => (DeadArgumentEliminationPass, FilePath),
    "InstCombinePass" => (InstCombinePass, MangledSymbol),
    "RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>" => (ModuleGlobalsAAPass, FilePath),
    "CGSCCToFunctionPassAdaptor" => (CGSCCToFunctionPassAdaptor, Nothing),
    "DevirtSCCRepeatedPass" => (DevirtSCCRepeatedPass, Nothing),
    "FunctionToLoopPassAdaptor" => (FunctionToLoopPassAdaptor, MangledSymbol),
    "InlinerPass" => (InlinerPass, Nothing),
    "JumpThreadingPass" => (JumpThreadingPass, MangledSymbol),
    "LoopFullUnrollPass" => (LoopFullUnrollPass, UnnamedLoop),
    "PassManager<llvm::Loop, llvm::LoopAnalysisManager, llvm::LoopStandardAnalysisResults &, llvm::LPMUpdater &>" => (LoopAnalysisManager, Nothing),
    "GVNPass" => (GVNPass, MangledSymbol),
    "EarlyCSEPass" => (EarlyCSEPass, MangledSymbol),
    "MemCpyOptPass" => (MemCpyOptPass, MangledSymbol),
    "CorrelatedValuePropagationPass" => (CorrelatedValuePropagationPass, MangledSymbol),
    "SimplifyCFGPass" => (SimplifyCFGPass, MangledSymbol),
    "TailCallElimPass" => (TailCallElimPass, MangledSymbol),
    "ReassociatePass" => (ReassociatePass, MangledSymbol),
    "LoopRotatePass" => (LoopRotatePass, UnnamedLoop),
    "LICMPass" => (LICMPass, UnnamedLoopOpt),
    "SCCPPass" => (SCCPPass, MangledSymbol),
    "BDCEPass" => (BDCEPass, MangledSymbol),
    "ADCEPass" => (ADCEPass, MangledSymbol),
    "DSEPass" => (DSEPass, MangledSymbol),
    "IndVarSimplifyPass" => (IndVarSimplifyPass, UnnamedLoop),
    "PostOrderFunctionAttrsPass" => (PostOrderFunctionAttrsPass, Nothing),
    "LoopSimplifyPass" => (LoopSimplifyPass, MangledSymbol),
    "LoopInstSimplifyPass" => (LoopInstSimplifyPass, UnnamedLoop),
    "ModuleToPostOrderCGSCCPassAdaptor" => (ModuleToPostOrderCGSCCPassAdaptor, FilePath),
    "ModuleInlinerWrapperPass" => (ModuleInlinerWrapperPass, FilePath),
    "GlobalDCEPass" => (GlobalDCEPass, FilePath),
    "EliminateAvailableExternallyPass" => (EliminateAvailableExternallyPass, FilePath),
    "ReversePostOrderFunctionAttrsPass" => (ReversePostOrderFunctionAttrsPass, FilePath),
    "SLPVectorizerPass" => (SLPVectorizerPass, MangledSymbol),
    "Float2IntPass" => (Float2IntPass, MangledSymbol),
    "LoopVectorizePass" => (LoopVectorizePass, MangledSymbol),
    "LowerConstantIntrinsicsPass" => (LowerConstantIntrinsicsPass, MangledSymbol),
    "LoopUnrollPass" => (LoopUnrollPass, MangledSymbol),
    "LoopSinkPass" => (LoopSinkPass, MangledSymbol),
    "InstSimplifyPass" => (InstSimplifyPass, MangledSymbol),
    "LoopLoadEliminationPass" => (LoopLoadEliminationPass, MangledSymbol),
    "InjectTLIMappings" => (InjectTLIMappings, MangledSymbol),
    "VectorCombinePass" => (VectorCombinePass, MangledSymbol),
    "CGProfilePass" => (CGProfilePass, FilePath),
    "ConstantMergePass" => (ConstantMergePass, FilePath),
    "RelLookupTableConverterPass" => (RelLookupTableConverterPass, FilePath),
    "Optimizer" => (Optimizer, Nothing),
    "LibCallsShrinkWrapPass" => (LibCallsShrinkWrapPass, MangledSymbol),
    "LCSSAPass" => (LCSSAPass, MangledSymbol),
    "LoopDeletionPass" => (LoopDeletionPass, UnnamedLoop),
}

/// What can go wrong while parsing an Activity
#[derive(Error, Debug, PartialEq)]
pub enum ActivityParseError {
    /// Encountered an unexpected activity name
    #[error("encountered unknown activity {0:?} with arguments {1:?}")]
    UnknownActivity(Box<str>, Option<HashMap<Box<str>, json::Value>>),

    /// Failed to parse activity arguments
    #[error("failed to process arguments of activity {0} ({1})")]
    BadArguments(ActivityId, ArgParseError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ctf::{
        stack::{EndStackTrace, StackFrameId, StackTrace},
        EventCategories,
    };

    #[test]
    fn activity_stat_accessors() {
        let stat = ActivityStat {
            activity: Activity {
                id: ActivityId::ExecuteCompiler,
                arg: RawActivityArgument::new(ActivityArgumentType::Nothing, None),
            },
            start: 12.3,
            duration: 45.6,
        };
        assert_eq!(stat.activity(), &stat.activity);
        assert_eq!(stat.start(), stat.start);
        assert_eq!(stat.duration(), stat.duration);
        assert_eq!(stat.end(), stat.start + stat.duration);
    }

    fn test_valid_activity(args: Option<HashMap<Box<str>, json::Value>>, expected: &Activity) {
        // Check direct Activity parsing
        let name = Box::<str>::from(expected.name());
        assert_eq!(
            Activity::parse(name.clone(), args.clone()),
            Ok(expected.clone())
        );

        // Preparate a generator of ActivityStat inputs that are valid from
        // the Activity point of view but not from the ActivityStat point of view
        let start = 4.2;
        let duration = 1234.5;
        let make_event = |good_type, pid, tid, cat, tts, stack_trace, tdur, end_stack_trace| {
            let duration_event = DurationEvent {
                pid,
                tid,
                ts: start,
                name: Some(name.clone()),
                cat,
                tts,
                args: args.clone(),
                stack_trace,
            };
            if good_type {
                TraceEvent::X {
                    duration_event,
                    dur: duration,
                    tdur,
                    end_stack_trace,
                }
            } else {
                TraceEvent::B(duration_event)
            }
        };

        // Valid ActivityStat input
        assert_eq!(
            ActivityStat::parse(make_event(true, 1, 0, None, None, None, None, None),),
            Ok(ActivityStat {
                activity: expected.clone(),
                start,
                duration,
            })
        );
        assert_eq!(
            ActivityStat::parse(make_event(true, 42, 42, None, None, None, None, None),),
            Ok(ActivityStat {
                activity: expected.clone(),
                start,
                duration,
            })
        );

        // Invalid inputs
        let test_bad_input = |input: TraceEvent| {
            assert_eq!(
                ActivityStat::parse(input.clone()),
                Err(ActivityStatParseError::UnexpectedInput(input))
            )
        };
        test_bad_input(make_event(false, 1, 0, None, None, None, None, None));
        test_bad_input(make_event(true, 123, 456, None, None, None, None, None));
        test_bad_input(make_event(
            true,
            1,
            0,
            Some(EventCategories::default()),
            None,
            None,
            None,
            None,
        ));
        test_bad_input(make_event(true, 1, 0, None, Some(start), None, None, None));
        test_bad_input(make_event(
            true,
            1,
            0,
            None,
            None,
            Some(StackTrace::sf(StackFrameId::default())),
            None,
            None,
        ));
        test_bad_input(make_event(
            true,
            1,
            0,
            None,
            None,
            None,
            Some(duration),
            None,
        ));
        test_bad_input(make_event(
            true,
            1,
            0,
            None,
            None,
            None,
            None,
            Some(EndStackTrace::esf(StackFrameId::default())),
        ));
    }

    #[test]
    fn unknown_activity() {
        let activity = Box::<str>::from("ThisIsMadness");
        assert_eq!(
            Activity::parse(activity.clone(), None),
            Err(ActivityParseError::UnknownActivity(activity, None))
        );
    }

    #[test]
    fn nullary_activities() {
        let nullary_test = |id, arg_parser| {
            // Determine activity name and associated Activity struct
            let name = <&str>::from(id);
            let activity = Activity {
                id,
                arg: RawActivityArgument::new(ActivityArgumentType::Nothing, None),
            };
            test_valid_activity(None, &activity);
            test_valid_activity(Some(HashMap::new()), &activity);

            // Add an undesired detail argument (true nullary only)
            if arg_parser == ActivityArgumentType::Nothing {
                let args = maplit::hashmap! { "detail".into() => json::json!("") };
                assert_eq!(
                    Activity::parse(name.into(), Some(args.clone()),),
                    Err(ActivityParseError::BadArguments(
                        id,
                        ArgParseError::UnexpectedKeys(args)
                    ))
                );
            }
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentType::Nothing {
                nullary_test(*activity_id, activity_parser.clone());
            }
        }
    }

    fn unary_test(arg: &str, expected: Activity, expected_wo_args: Option<Activity>) {
        // Test happy path
        let good_args = maplit::hashmap! { "detail".into() => json::json!(arg) };
        test_valid_activity(Some(good_args.clone()), &expected);

        // Try not providing the requested argument
        let name = <Box<str>>::from(expected.name());
        if let Some(activity_wo_args) = expected_wo_args {
            test_valid_activity(None, &activity_wo_args);
            test_valid_activity(Some(HashMap::new()), &activity_wo_args);
        } else {
            let missing_arg_error = Err(ActivityParseError::BadArguments(
                expected.id(),
                ArgParseError::MissingKey("detail"),
            ));
            assert_eq!(Activity::parse(name.clone(), None), missing_arg_error);
            assert_eq!(
                Activity::parse(name.clone(), Some(HashMap::new()),),
                missing_arg_error
            );
        }

        // Try providing a wrongly typed value
        let bad_value = json::json!(42usize);
        let bad_arg_value = maplit::hashmap! { "detail".into() => bad_value.clone() };
        assert_eq!(
            Activity::parse(name.clone(), Some(bad_arg_value),),
            Err(ActivityParseError::BadArguments(
                expected.id(),
                ArgParseError::UnexpectedValue("detail", bad_value)
            ))
        );

        // Try adding a meaningless argument
        let mut bad_arg = good_args.clone();
        bad_arg.insert("wat".into(), json::json!(""));
        assert_eq!(
            Activity::parse(name, Some(bad_arg.clone()),),
            Err(ActivityParseError::BadArguments(
                expected.id(),
                ArgParseError::UnexpectedKeys(maplit::hashmap! { "wat".into() => json::json!("") })
            ))
        );
    }

    #[test]
    fn string_activities() {
        let string_test = |id| {
            const MOCK_ARG: &str = "X86 DAG->DAG Instruction Selection";
            unary_test(
                MOCK_ARG,
                Activity {
                    id,
                    arg: RawActivityArgument::new(
                        ActivityArgumentType::String,
                        Some(MOCK_ARG.into()),
                    ),
                },
                None,
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentType::String {
                string_test(*activity_id);
            }
        }
    }

    #[test]
    fn path_activities() {
        let path_test = |id| {
            const MOCK_PATH: &str =
                "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
            unary_test(
                MOCK_PATH,
                Activity {
                    id,
                    arg: RawActivityArgument::new(
                        ActivityArgumentType::FilePath,
                        Some(MOCK_PATH.into()),
                    ),
                },
                None,
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentType::FilePath {
                path_test(*activity_id);
            }
        }
    }

    #[test]
    fn entity_activities() {
        let entity_test = |id| {
            const MOCK_ENTITY: &str = "Acts::Test::MeasurementCreator";
            unary_test(
                MOCK_ENTITY,
                Activity {
                    id,
                    arg: RawActivityArgument::new(
                        ActivityArgumentType::CppEntity,
                        Some(MOCK_ENTITY.into()),
                    ),
                },
                None,
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentType::CppEntity {
                entity_test(*activity_id);
            }
        }
    }

    #[test]
    fn mangled_activities() {
        let mangled_test = |id, parser: ActivityArgumentType| {
            const MOCK_SYMBOL: &str = "_ZN4Acts4Test29comb_kalman_filter_zero_field11test_methodEv";
            unary_test(
                MOCK_SYMBOL,
                Activity {
                    id,
                    arg: RawActivityArgument::new(parser, Some(MOCK_SYMBOL.into())),
                },
                (parser == ActivityArgumentType::MangledSymbolOpt).then_some(Activity {
                    id,
                    arg: RawActivityArgument::new(parser, None),
                }),
            );
        };

        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentType::MangledSymbol
                || *activity_parser == ActivityArgumentType::MangledSymbolOpt
            {
                mangled_test(*activity_id, *activity_parser);
            }
        }
    }

    #[test]
    fn unnamed_activities() {
        let unnamed_test = |id, parser: ActivityArgumentType| {
            const MOCK_LOOP: &str = "<unnamed loop>";
            unary_test(
                MOCK_LOOP,
                Activity {
                    id,
                    arg: RawActivityArgument::new(parser, Some(MOCK_LOOP.into())),
                },
                (parser == ActivityArgumentType::UnnamedLoopOpt).then_some(Activity {
                    id,
                    arg: RawActivityArgument::new(parser, None),
                }),
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentType::UnnamedLoop
                || *activity_parser == ActivityArgumentType::UnnamedLoopOpt
            {
                unnamed_test(*activity_id, *activity_parser);
            }
        }
    }
}
