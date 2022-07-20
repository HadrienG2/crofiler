//! Handling of activities, that is, clang-provided categories of stuff it can
//! be doing at a point in time

use super::ArgParseError;
use crate::{
    ctf::{events::duration::DurationEvent, Duration, Timestamp, TraceEvent},
    PathError, PathKey,
};
use cpp_demangle::{DemangleOptions, ParseOptions, Symbol};
use cpparser::{nom, EntityKey, EntityParser};
use log::trace;
use phf::phf_map;
use serde_json as json;
use std::{cell::RefCell, collections::HashMap};
use thiserror::Error;

/// Tuned 10x above maximum observed requirement
const CPP_DEMANGLE_RECURSION_LIMIT: u32 = 1024;

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
    pub fn new(activity: Activity, start: Timestamp, duration: Timestamp) -> Self {
        Self {
            activity,
            start,
            duration,
        }
    }

    /// Decode a TraceEvent which is expected to contain a timed activity
    pub fn parse(
        t: TraceEvent,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<Self, ActivityStatParseError> {
        match t {
            TraceEvent::X {
                duration_event:
                    DurationEvent {
                        pid: _,
                        tid: _,
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
            } => {
                let activity = Activity::parse(name, args, parser, demangling_buf)?;
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
    pub(crate) arg: ActivityArgument,
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

    /// Parsed argument of the activity
    pub fn argument(&self) -> &ActivityArgument {
        &self.arg
    }

    /// Parse from useful bits of Duration events
    fn parse(
        name: Box<str>,
        mut args: Option<HashMap<Box<str>, json::Value>>,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<Self, ActivityParseError> {
        // Handling of activity name
        let (id, arg_parser) = if let Some(activity) = ACTIVITIES.get(&name) {
            activity
        } else {
            return Err(ActivityParseError::UnknownActivity(
                name.clone(),
                args.take(),
            ));
        };

        // Interior mutability to allow multiple mutable borrows
        let args = RefCell::new(args);

        // Handling of activities with one "detail" argument
        let detail_arg = || -> Result<Box<str>, ArgParseError> {
            Self::parse_detail_arg(args.borrow_mut().take())
        };
        //
        let mut mangled_arg = || -> Result<MangledSymbol, ActivityParseError> {
            let symbol = detail_arg()?;
            Self::parse_mangled_symbol(symbol, parser, demangling_buf)
        };
        //
        let parse_unnamed_loop_arg = || -> Result<(), ActivityParseError> {
            let loop_name = detail_arg()?;
            Self::parse_unnamed_loop(loop_name)
        };
        //
        let arg = match *arg_parser {
            ActivityArgumentParser::Nothing => {
                Self::parse_empty_args(args.borrow_mut().take())?;
                ActivityArgument::Nothing
            }

            ActivityArgumentParser::String => ActivityArgument::String(detail_arg()?),

            ActivityArgumentParser::FilePath => {
                ActivityArgument::FilePath(parser.intern_path(&detail_arg()?))
            }

            ActivityArgumentParser::CppEntity => {
                ActivityArgument::CppEntity(Self::parse_entity(&detail_arg()?, parser)?)
            }

            ActivityArgumentParser::MangledSymbol => {
                ActivityArgument::MangledSymbol(mangled_arg()?)
            }

            ActivityArgumentParser::MangledSymbolOpt => match mangled_arg() {
                Ok(sym) => ActivityArgument::MangledSymbol(sym),
                Err(ActivityParseError::BadArguments(ArgParseError::MissingKey("detail"))) => {
                    ActivityArgument::Nothing
                }
                Err(e) => return Err(e)?,
            },

            ActivityArgumentParser::UnnamedLoop => {
                parse_unnamed_loop_arg()?;
                ActivityArgument::UnnamedLoop
            }

            ActivityArgumentParser::UnnamedLoopOpt => match parse_unnamed_loop_arg() {
                Ok(()) => ActivityArgument::UnnamedLoop,
                Err(ActivityParseError::BadArguments(ArgParseError::MissingKey("detail"))) => {
                    ActivityArgument::Nothing
                }
                Err(e) => return Err(e)?,
            },
        };

        Ok(Self {
            id: id.clone(),
            arg,
        })
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
    ) -> Result<Box<str>, ArgParseError> {
        if let Some(args) = args {
            let mut args_iter = args.into_iter();
            let collect_bad_args = |args_iter, (k, v)| {
                let mut remainder = HashMap::from_iter(args_iter);
                remainder.insert(k, v);
                remainder.remove("detail");
                remainder
            };
            if let Some((k, v)) = args_iter.next() {
                if &*k != "detail" {
                    return Err(ArgParseError::UnexpectedKeys(collect_bad_args(
                        args_iter,
                        (k, v),
                    )));
                }
                let s = if let json::Value::String(s) = v {
                    s.into()
                } else {
                    return Err(ArgParseError::UnexpectedValue("detail", v));
                };
                if let Some(kv) = args_iter.next() {
                    return Err(ArgParseError::UnexpectedKeys(collect_bad_args(
                        args_iter, kv,
                    )));
                }
                Ok(s)
            } else {
                Err(ArgParseError::MissingKey("detail"))
            }
        } else {
            Err(ArgParseError::MissingKey("detail"))
        }
    }

    /// Parse a "detail" argument payload that contains a C++ entity name
    fn parse_entity(s: &str, parser: &mut EntityParser) -> Result<EntityKey, ActivityParseError> {
        parser.parse_entity(s).map_err(|e| {
            ActivityParseError::from(nom::error::Error::new(Box::<str>::from(e.input), e.code))
        })
    }

    /// Parse a "detail" argument payload that contains a mangled C++ symbol
    fn parse_mangled_symbol(
        symbol: Box<str>,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<MangledSymbol, ActivityParseError> {
        let mut parse_demangled = |entity: Box<str>| -> MangledSymbol {
            if let Ok(parsed) = Self::parse_entity(&*entity, parser) {
                MangledSymbol::Parsed(parsed)
            } else {
                MangledSymbol::Demangled(entity)
            }
        };

        let demangling_result = match Symbol::new_with_options(
            &*symbol,
            &ParseOptions::default().recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
        )
        .map(|s| {
            demangling_buf.clear();
            s.structured_demangle(
                &mut *demangling_buf,
                &DemangleOptions::default()
                    .hide_expression_literal_types()
                    .no_return_type()
                    .recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
            )
        }) {
            // Mangled symbol was successfully demangled, intern it along with the rest
            Ok(Ok(())) => parse_demangled(demangling_buf.clone().into_boxed_str()),

            // Symbol failed to demangle, try some patterns that cpp_demangle
            // should not reject but actually does reject before giving up
            Ok(Err(_)) | Err(_) => match &*symbol {
                "main" | "__clang_call_terminate" => parse_demangled(symbol),
                _ => MangledSymbol::Mangled(symbol),
            },
        };

        match &demangling_result {
            MangledSymbol::Parsed(_) => {}
            MangledSymbol::Demangled(d) => trace!("Failed to parse demangled symbol {d:?}"),
            MangledSymbol::Mangled(m) => trace!("Failed to demangle symbol {m:?}"),
        }

        Ok(demangling_result)
    }

    /// Handling of the "<unnamed loop>" constant argument
    fn parse_unnamed_loop(loop_name: Box<str>) -> Result<(), ActivityParseError> {
        if &*loop_name == "<unnamed loop>" {
            Ok(())
        } else {
            Err(ActivityParseError::UnexpectedLoopName(loop_name))
        }
    }
}

/// Generate a PHF that maps from clang activity names (as seen in time-trace
/// files) to a unique identifier (ActivityId) and an ActivityArgumentParsing
/// that tells how the activity argument should be interpreted.
macro_rules! generate_activities {
    ($($string:literal => ($enum:ident, $arg:ident)),* $(,)?) => {
        /// Clang activity identifier
        #[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, strum::IntoStaticStr)]
        pub enum ActivityId {
            $(
                #[doc = $string]
                #[strum(to_string = $string)]
                $enum
            ),*
        }

        /// Map from a clang activity name to its identifier and argument type
        static ACTIVITIES: phf::Map<&'static str, (ActivityId, ActivityArgumentParser)> = phf_map! {
            $(
                $string => (ActivityId::$enum, ActivityArgumentParser::$arg)
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
//
/// Empirically observed activity argument parsing logics for time-trace entries
#[derive(Clone, Debug, Eq, PartialEq)]
enum ActivityArgumentParser {
    /// No argument
    Nothing,

    /// An arbitrary string
    String,

    /// An interned file path
    FilePath,

    /// A C++ entity (class, function, ...)
    CppEntity,

    /// A C++ mangled symbol
    MangledSymbol,

    /// Either a C++ mangled symbol or nothing
    MangledSymbolOpt,

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,

    /// Either the "<unnamed loop>" constant string or nothing
    UnnamedLoopOpt,
}
//
/// Concrete things that an activity can take as an argument
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ActivityArgument {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(Box<str>),

    /// An interned file path
    FilePath(PathKey),

    /// A C++ entity (class, function, ...)
    CppEntity(EntityKey),

    /// A C++ mangled symbol
    MangledSymbol(MangledSymbol),

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,
}
//
/// A mangled C++ symbol that we tried to demangle and parse
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MangledSymbol {
    /// Symbol was successfully mangled and interned
    Parsed(EntityKey),

    /// The symbol was demangled, but could not be parsed into an AST
    ///
    /// This normally happens when the demangler emits ill-formed output such
    /// as `SomeTemplate<int, && >` or `()...`. If you find reasonable output
    /// which we do not parse, please submit it as a bug.
    ///
    Demangled(Box<str>),

    /// Demangling failed and the symbol was kept in its original form.
    ///
    /// Typical patterns that fail to demangle include
    /// - __cxx_global_var_init(.<number>)?
    /// - _GLOBAL__sub_I_<source file>
    ///
    Mangled(Box<str>),
}

/// What can go wrong while parsing an Activity
#[derive(Error, Debug, PartialEq)]
pub enum ActivityParseError {
    /// Encountered an unexpected activity name
    #[error("encountered unknown activity {0:?} with arguments {1:?}")]
    UnknownActivity(Box<str>, Option<HashMap<Box<str>, json::Value>>),

    // FIXME: Specify the ActivityId in the errors below
    //
    /// Failed to parse activity arguments
    #[error("failed to parse activity arguments ({0})")]
    BadArguments(#[from] ArgParseError),

    /// Encountered an unexpected activity file path
    #[error("failed to parse activity file path ({0})")]
    BadFilePath(#[from] PathError),

    /// Failed to parse a C++ entity name
    #[error("failed to parse C++ entity ({0})")]
    BadCppEntity(#[from] nom::error::Error<Box<str>>),

    /// Unexpected loop name
    #[error("unexpected loop name ({0})")]
    UnexpectedLoopName(Box<str>),
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
                arg: ActivityArgument::Nothing,
            },
            start: 12.3,
            duration: 45.6,
        };
        assert_eq!(stat.activity(), &stat.activity);
        assert_eq!(stat.start(), stat.start);
        assert_eq!(stat.duration(), stat.duration);
        assert_eq!(stat.end(), stat.start + stat.duration);
    }

    fn test_valid_activity(
        args: Option<HashMap<Box<str>, json::Value>>,
        expected: &Activity,
        parser: &mut EntityParser,
    ) {
        // Check direct Activity parsing
        let name = Box::<str>::from(expected.name());
        let mut demangling_buf = String::new();
        assert_eq!(
            Activity::parse(name.clone(), args.clone(), parser, &mut demangling_buf),
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
            ActivityStat::parse(
                make_event(true, 1, 0, None, None, None, None, None),
                parser,
                &mut demangling_buf
            ),
            Ok(ActivityStat {
                activity: expected.clone(),
                start,
                duration,
            })
        );

        // Invalid inputs
        let mut test_bad_input = |input: TraceEvent| {
            assert_eq!(
                ActivityStat::parse(input.clone(), parser, &mut demangling_buf),
                Err(ActivityStatParseError::UnexpectedInput(input))
            )
        };
        test_bad_input(make_event(false, 1, 0, None, None, None, None, None));
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
        let mut parser = EntityParser::new();
        let mut demanging_buf = String::new();
        let activity = Box::<str>::from("ThisIsMadness");
        assert_eq!(
            Activity::parse(activity.clone(), None, &mut parser, &mut demanging_buf),
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
                arg: ActivityArgument::Nothing,
            };

            // Test two different ways of passing no arguments
            let mut parser = EntityParser::new();
            test_valid_activity(None, &activity, &mut parser);
            test_valid_activity(Some(HashMap::new()), &activity, &mut parser);

            // Add an undesired detail argument (true nullary only)
            if arg_parser == ActivityArgumentParser::Nothing {
                let args = maplit::hashmap! { "detail".into() => json::json!("") };
                let mut demangling_buf = String::new();
                assert_eq!(
                    Activity::parse(
                        name.into(),
                        Some(args.clone()),
                        &mut parser,
                        &mut demangling_buf,
                    ),
                    Err(ActivityParseError::BadArguments(
                        ArgParseError::UnexpectedKeys(args)
                    ))
                );
            }
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentParser::Nothing
                || *activity_parser == ActivityArgumentParser::MangledSymbolOpt
            {
                nullary_test(*activity_id, activity_parser.clone());
            }
        }
    }

    fn unary_test(
        arg: &str,
        expected: Activity,
        mut parser: EntityParser,
        expected_wo_args: Option<Activity>,
    ) {
        // Test happy path
        let good_args = maplit::hashmap! { "detail".into() => json::json!(arg) };
        test_valid_activity(Some(good_args.clone()), &expected, &mut parser);

        // Try not providing the requested argument
        let name = <Box<str>>::from(<&str>::from(expected.id()));
        let mut demangling_buf = String::new();
        if let Some(activity_wo_args) = expected_wo_args {
            test_valid_activity(None, &activity_wo_args, &mut parser);
            test_valid_activity(Some(HashMap::new()), &activity_wo_args, &mut parser);
        } else {
            let missing_arg_error = Err(ActivityParseError::BadArguments(
                ArgParseError::MissingKey("detail"),
            ));
            assert_eq!(
                Activity::parse(name.clone(), None, &mut parser, &mut demangling_buf),
                missing_arg_error
            );
            assert_eq!(
                Activity::parse(
                    name.clone(),
                    Some(HashMap::new()),
                    &mut parser,
                    &mut demangling_buf
                ),
                missing_arg_error
            );
        }

        // Try providing a wrongly typed value
        let bad_value = json::json!(42usize);
        let bad_arg_value = maplit::hashmap! { "detail".into() => bad_value.clone() };
        assert_eq!(
            Activity::parse(
                name.clone(),
                Some(bad_arg_value),
                &mut parser,
                &mut demangling_buf
            ),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedValue("detail", bad_value)
            ))
        );

        // Try adding a meaningless argument
        let mut bad_arg = good_args.clone();
        bad_arg.insert("wat".into(), json::json!(""));
        assert_eq!(
            Activity::parse(
                name,
                Some(bad_arg.clone()),
                &mut parser,
                &mut demangling_buf
            ),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedKeys(maplit::hashmap! { "wat".into() => json::json!("") })
            ))
        );
    }

    #[test]
    fn string_activities() {
        let string_test = |id| {
            let mock_arg = "X86 DAG->DAG Instruction Selection";
            unary_test(
                mock_arg,
                Activity {
                    id,
                    arg: ActivityArgument::String(mock_arg.into()),
                },
                EntityParser::new(),
                None,
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentParser::String {
                string_test(*activity_id);
            }
        }
    }

    #[test]
    fn path_activities() {
        let path_test = |id| {
            let mock_path = "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
            let mut parser = EntityParser::new();
            let path_key = parser.intern_path(mock_path);
            unary_test(
                mock_path,
                Activity {
                    id,
                    arg: ActivityArgument::FilePath(path_key),
                },
                parser,
                None,
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentParser::FilePath {
                path_test(*activity_id);
            }
        }
    }

    #[test]
    fn entity_activities() {
        let entity_test = |id| {
            let mock_entity = "Acts::Test::MeasurementCreator";
            let mut parser = EntityParser::new();
            let key = parser
                .parse_entity(mock_entity)
                .expect("Known-good parse, shouldn't fail");
            unary_test(
                mock_entity,
                Activity {
                    id,
                    arg: ActivityArgument::CppEntity(key),
                },
                parser,
                None,
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentParser::CppEntity {
                entity_test(*activity_id);
            }
        }
    }

    #[test]
    fn mangled_activities() {
        let mangled_test = |id, optional: bool| {
            // Mangled symbol that demangles
            const VALID: &'static str =
                "_ZN4Acts4Test29comb_kalman_filter_zero_field11test_methodEv";
            let mut parser = EntityParser::new();
            let key = parser
                .parse_entity("Acts::Test::comb_kalman_filter_zero_field::test_method()")
                .expect("Known-good parse, shouldn't fail");
            unary_test(
                VALID,
                Activity {
                    id,
                    arg: ActivityArgument::MangledSymbol(MangledSymbol::Parsed(key)),
                },
                parser,
                optional.then_some(Activity {
                    id,
                    arg: ActivityArgument::Nothing,
                }),
            );

            // Mangled symbol that doesn't demangle
            const INVALID: &'static str = "__cxx_global_var_init.1";
            unary_test(
                INVALID,
                Activity {
                    id,
                    arg: ActivityArgument::MangledSymbol(MangledSymbol::Mangled(INVALID.into())),
                },
                EntityParser::new(),
                optional.then_some(Activity {
                    id,
                    arg: ActivityArgument::Nothing,
                }),
            );
        };

        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentParser::MangledSymbol
                || *activity_parser == ActivityArgumentParser::MangledSymbolOpt
            {
                mangled_test(
                    *activity_id,
                    *activity_parser == ActivityArgumentParser::MangledSymbolOpt,
                );
            }
        }
    }

    #[test]
    fn unnamed_activities() {
        let unnamed_test = |id, optional: bool| {
            // Correct syntax
            unary_test(
                "<unnamed loop>",
                Activity {
                    id,
                    arg: ActivityArgument::UnnamedLoop,
                },
                EntityParser::new(),
                optional.then_some(Activity {
                    id,
                    arg: ActivityArgument::Nothing,
                }),
            );

            // Unexpected loop name
            let activity_name = Box::<str>::from(<&str>::from(id));
            let bad_name = Box::<str>::from("xxx");
            let args = maplit::hashmap! { "detail".into() => json::json!(&*bad_name) };
            let mut demangling_buf = String::new();
            assert_eq!(
                Activity::parse(
                    activity_name,
                    Some(args),
                    &mut EntityParser::new(),
                    &mut demangling_buf,
                ),
                Err(ActivityParseError::UnexpectedLoopName(bad_name))
            );
        };
        for (activity_id, activity_parser) in ACTIVITIES.values() {
            if *activity_parser == ActivityArgumentParser::UnnamedLoop
                || *activity_parser == ActivityArgumentParser::UnnamedLoopOpt
            {
                unnamed_test(
                    *activity_id,
                    *activity_parser == ActivityArgumentParser::UnnamedLoopOpt,
                );
            }
        }
    }
}
