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
        parser: &EntityParser,
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
#[derive(Clone, Debug, PartialEq)]
pub struct Activity {
    /// Machine identifier that can be translated back into a human identifier
    id: ActivityId,

    /// Supplementary data received as an argument, if any
    arg: ActivityArgument,
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
        parser: &EntityParser,
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
                ActivityArgument::FilePath(parser.path_to_key(&detail_arg()?))
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
    fn parse_entity(s: &str, parser: &EntityParser) -> Result<EntityKey, ActivityParseError> {
        parser.parse_entity(s).map_err(|e| {
            ActivityParseError::from(nom::error::Error::new(Box::<str>::from(e.input), e.code))
        })
    }

    /// Parse a "detail" argument payload that contains a mangled C++ symbol
    fn parse_mangled_symbol(
        symbol: Box<str>,
        parser: &EntityParser,
        demangling_buf: &mut String,
    ) -> Result<MangledSymbol, ActivityParseError> {
        let parse_demangled = |entity: Box<str>| -> MangledSymbol {
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
#[derive(Clone, Debug, PartialEq)]
pub enum ActivityArgumentParser {
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
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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
            activity: Activity::ExecuteCompiler,
            start: 12.3,
            duration: 45.6,
        };
        assert_eq!(stat.activity(), &stat.activity);
        assert_eq!(stat.start(), stat.start);
        assert_eq!(stat.duration(), stat.duration);
        assert_eq!(stat.end(), stat.start + stat.duration);
    }

    fn test_valid_activity(
        name: &str,
        args: Option<HashMap<Box<str>, json::Value>>,
        expected: &Activity,
        expected_arg: &ActivityArgument,
        parser: &EntityParser,
    ) {
        // Check name and argument accessors
        assert_eq!(expected.name(), name);
        assert_eq!(expected.argument(), *expected_arg);

        // Check direct Activity parsing
        let name = Box::<str>::from(name);
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
        test_bad_input(make_event(true, 0, 0, None, None, None, None, None));
        test_bad_input(make_event(true, 1, 1, None, None, None, None, None));
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
        let parser = EntityParser::new();
        let mut demanging_buf = String::new();
        let activity = Box::<str>::from("ThisIsMadness");
        assert_eq!(
            Activity::parse(activity.clone(), None, &parser, &mut demanging_buf),
            Err(ActivityParseError::UnknownActivity(activity))
        );
    }

    fn nullary_test(name: &str, a: Activity) {
        // Test two different ways of passing no arguments
        let parser = EntityParser::new();
        test_valid_activity(name, None, &a, &ActivityArgument::Nothing, &parser);
        test_valid_activity(
            name,
            Some(HashMap::new()),
            &a,
            &ActivityArgument::Nothing,
            &parser,
        );

        // Add an undesired detail argument
        let args = maplit::hashmap! { "detail".into() => json::json!("") };
        let mut demangling_buf = String::new();
        assert_eq!(
            Activity::parse(
                name.into(),
                Some(args.clone()),
                &parser,
                &mut demangling_buf,
            ),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedKeys(args)
            ))
        );
    }

    #[test]
    fn perform_pending_instantiations() {
        nullary_test(
            "PerformPendingInstantiations",
            Activity::PerformPendingInstantiations,
        );
    }

    #[test]
    fn frontend() {
        nullary_test("Frontend", Activity::Frontend);
    }

    #[test]
    fn per_function_passes() {
        nullary_test("PerFunctionPasses", Activity::PerFunctionPasses);
    }

    #[test]
    fn per_module_passes() {
        nullary_test("PerModulePasses", Activity::PerModulePasses);
    }

    #[test]
    fn code_gen_passes() {
        nullary_test("CodeGenPasses", Activity::CodeGenPasses);
    }

    #[test]
    fn backend() {
        nullary_test("Backend", Activity::Backend);
    }

    #[test]
    fn execute_compiler() {
        nullary_test("ExecuteCompiler", Activity::ExecuteCompiler);
    }

    fn unary_test(
        name: &str,
        arg: &str,
        expected: Activity,
        expected_argument: ActivityArgument,
        parser: EntityParser,
    ) {
        // Test happy path
        let name = Box::<str>::from(name);
        let good_args = maplit::hashmap! { "detail".into() => json::json!(arg) };
        test_valid_activity(
            &name,
            Some(good_args.clone()),
            &expected,
            &expected_argument,
            &parser,
        );

        // Try not providing the requested argument
        let mut demangling_buf = String::new();
        let missing_arg_error = Err(ActivityParseError::BadArguments(ArgParseError::MissingKey(
            "detail",
        )));
        assert_eq!(
            Activity::parse(name.clone(), None, &parser, &mut demangling_buf),
            missing_arg_error
        );
        assert_eq!(
            Activity::parse(
                name.clone(),
                Some(HashMap::new()),
                &parser,
                &mut demangling_buf
            ),
            missing_arg_error
        );

        // Try providing a wrongly typed value
        let bad_value = json::json!(42usize);
        let bad_arg_value = maplit::hashmap! { "detail".into() => bad_value.clone() };
        assert_eq!(
            Activity::parse(
                name.clone(),
                Some(bad_arg_value),
                &parser,
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
            Activity::parse(name, Some(bad_arg.clone()), &parser, &mut demangling_buf),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedKeys(maplit::hashmap! { "wat".into() => json::json!("") })
            ))
        );
    }

    #[test]
    fn source() {
        const PATH: &'static str =
            "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
        let parser = EntityParser::new();
        let key = parser.path_to_key(PATH);
        unary_test(
            "Source",
            PATH,
            Activity::Source(key),
            ActivityArgument::FilePath(key),
            parser,
        );
    }

    #[test]
    fn parse_class() {
        const CLASS: &'static str = "Acts::Test::MeasurementCreator";
        let parser = EntityParser::new();
        let key = parser.parse_entity(CLASS).unwrap();
        unary_test(
            "ParseClass",
            CLASS,
            Activity::ParseClass(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn instantiate_class() {
        const CLASS: &'static str = "std::invoke_result<(lambda at /mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp:354:40), Acts::detail_lt::TrackStateProxy<Acts::Test::ExtendedMinimalSourceLink, 6, 6, true> >";
        let parser = EntityParser::new();
        let key = parser.parse_entity(CLASS).unwrap();
        unary_test(
            "InstantiateClass",
            CLASS,
            Activity::InstantiateClass(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn parse_template() {
        const TEMPLATE: &'static str = "<unknown>"; // Yes, clang can do that
        let parser = EntityParser::new();
        let key = parser.parse_entity(TEMPLATE).unwrap();
        unary_test(
            "ParseTemplate",
            TEMPLATE,
            Activity::ParseTemplate(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn instantiate_function() {
        const FUNCTION: &'static str = "boost::unit_test::lazy_ostream_impl<boost::unit_test::lazy_ostream, boost::unit_test::basic_cstring<const char>, const boost::unit_test::basic_cstring<const char> &>::operator()";
        let parser = EntityParser::new();
        let key = parser.parse_entity(FUNCTION).unwrap();
        unary_test(
            "InstantiateFunction",
            FUNCTION,
            Activity::InstantiateFunction(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_type() {
        const TYPE: &'static str = "generic_dense_assignment_kernel<DstEvaluatorType, SrcEvaluatorType, Eigen::internal::add_assign_op<double, double> >";
        let parser = EntityParser::new();
        let key = parser.parse_entity(TYPE).unwrap();
        unary_test(
            "DebugType",
            TYPE,
            Activity::DebugType(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_global_variable() {
        const VAR: &'static str = "std::__detail::__variant::__gen_vtable<true, void, (lambda at /mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp:819:11) &&, std::variant<Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime> > &&>::_S_vtable";
        let parser = EntityParser::new();
        let key = parser.parse_entity(VAR).unwrap();
        unary_test(
            "DebugGlobalVariable",
            VAR,
            Activity::DebugGlobalVariable(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn code_gen_function() {
        const FUNCTION: &'static str =
            "boost::unit_test::operator<<<char, std::char_traits<char>, const char>";
        let parser = EntityParser::new();
        let key = parser.parse_entity(FUNCTION).unwrap();
        unary_test(
            "CodeGen Function",
            FUNCTION,
            Activity::CodeGenFunction(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_function() {
        const FUNCTION: &'static str = "Eigen::operator*<Eigen::PermutationMatrix<6, 6, int>, Eigen::CwiseNullaryOp<Eigen::internal::scalar_identity_op<double>, Eigen::Matrix<double, 6, 6, 1, 6, 6> > >";
        let parser = EntityParser::new();
        let key = parser.parse_entity(FUNCTION).unwrap();
        unary_test(
            "DebugFunction",
            FUNCTION,
            Activity::DebugFunction(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn run_pass() {
        const PASS: &'static str = "X86 DAG->DAG Instruction Selection";
        unary_test(
            "RunPass",
            PASS,
            Activity::RunPass(PASS.into()),
            ActivityArgument::String(PASS.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn opt_function() {
        let parser = EntityParser::new();

        const VALID: &'static str = "_ZN4Acts4Test29comb_kalman_filter_zero_field11test_methodEv";
        let key = parser
            .parse_entity("Acts::Test::comb_kalman_filter_zero_field::test_method()")
            .unwrap();
        unary_test(
            "OptFunction",
            VALID,
            Activity::OptFunction(MangledSymbol::Parsed(key)),
            ActivityArgument::MangledSymbol(MangledSymbol::Parsed(key)),
            EntityParser::new(),
        );

        const INVALID: &'static str = "__cxx_global_var_init.1";
        unary_test(
            "OptFunction",
            INVALID,
            Activity::OptFunction(MangledSymbol::Mangled(INVALID.into())),
            ActivityArgument::MangledSymbol(MangledSymbol::Mangled(INVALID.into())),
            EntityParser::new(),
        );
    }

    #[test]
    fn run_loop_pass() {
        const PASS: &'static str = "Induction Variable Users";
        unary_test(
            "RunLoopPass",
            PASS,
            Activity::RunLoopPass(PASS.into()),
            ActivityArgument::String(PASS.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn opt_module() {
        const MODULE: &'static str =
            "/mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp";
        let parser = EntityParser::new();
        let key = parser.path_to_key(MODULE);
        unary_test(
            "OptModule",
            MODULE,
            Activity::OptModule(key.clone()),
            ActivityArgument::FilePath(key),
            parser,
        );
    }
}
