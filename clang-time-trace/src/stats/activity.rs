//! Handling of activities, that is, clang-provided categories of stuff it can
//! be doing at a point in time

use super::ArgParseError;
use crate::{
    ctf::{events::duration::DurationEvent, Duration, Timestamp, TraceEvent},
    PathError, PathKey,
};
use cpparser::EntityParser;
use serde_json as json;
use std::{cell::RefCell, collections::HashMap};
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
    pub fn new(activity: Activity, start: Timestamp, duration: Timestamp) -> Self {
        Self {
            activity,
            start,
            duration,
        }
    }

    /// Decode a TraceEvent which is expected to contain a timed activity
    pub fn parse(t: TraceEvent, parser: &EntityParser) -> Result<Self, ActivityStatParseError> {
        match t {
            TraceEvent::X {
                duration_event:
                    DurationEvent {
                        pid: 1,
                        tid: 0,
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
                let activity = Activity::parse(name, args, parser)?;
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
pub enum Activity {
    /// Processing a source file
    Source(PathKey),

    /// Parsing a class
    // TODO: Switch to a namespace + AST representation
    ParseClass(Box<str>),

    /// Instantiating a class
    // TODO: Switch to a namespace + AST representation
    InstantiateClass(Box<str>),

    /// Parsing a template
    // TODO: Switch to a namespace + AST representation
    ParseTemplate(Box<str>),

    /// Instantiating a function
    // TODO: Switch to a namespace + AST representation
    InstantiateFunction(Box<str>),

    /// Generating debug info for a type
    // TODO: Switch to a namespace + AST representation
    DebugType(Box<str>),

    /// Generating debug info for a global variable
    // TODO: Switch to a namespace + AST representation
    DebugGlobalVariable(Box<str>),

    /// Generate a function's code
    // TODO: Switch to a namespace + AST representation
    CodeGenFunction(Box<str>),

    /// Generating debug info for a function
    // TODO: Switch to a namespace + AST representation
    DebugFunction(Box<str>),

    /// Perform pending instantiations (as the name suggests)
    PerformPendingInstantiations,

    /// Compiler front-end work
    Frontend,

    /// Running a named compiler pass
    RunPass(Box<str>),

    /// Optimizing code
    // TODO: Demangle then switch to a namespace + AST representation
    OptFunction(Box<str>),

    /// Per-function compiler passes
    PerFunctionPasses,

    /// Running a named loop compiler pass
    RunLoopPass(Box<str>),

    /// Optimizing a module
    OptModule(PathKey),

    /// Per-module compiler passes
    PerModulePasses,

    /// Code generation passes
    CodeGenPasses,

    /// Compiler back-end work
    Backend,

    /// Compiler execution
    ExecuteCompiler,
}
//
impl Activity {
    /// Textual name of the activity, as featured in the JSON data
    pub fn name(&self) -> &'static str {
        use Activity::*;
        match self {
            Source(_) => "Source",
            ParseClass(_) => "ParseClass",
            InstantiateClass(_) => "InstantiateClass",
            ParseTemplate(_) => "ParseTemplate",
            InstantiateFunction(_) => "InstantiateFunction",
            DebugType(_) => "DebugType",
            DebugGlobalVariable(_) => "DebugGlobalVariable",
            CodeGenFunction(_) => "CodeGen Function",
            DebugFunction(_) => "DebugFunction",
            PerformPendingInstantiations => "PerformPendingInstantiations",
            Frontend => "Frontend",
            RunPass(_) => "RunPass",
            OptFunction(_) => "OptFunction",
            PerFunctionPasses => "PerFunctionPasses",
            RunLoopPass(_) => "RunLoopPass",
            OptModule(_) => "OptModule",
            PerModulePasses => "PerModulePasses",
            CodeGenPasses => "CodeGenPasses",
            Backend => "Backend",
            ExecuteCompiler => "ExecuteCompiler",
        }
    }

    /// Parsed argument of the activity
    pub fn argument(&self) -> ActivityArgument {
        use Activity::*;
        use ActivityArgument::*;
        match self {
            PerformPendingInstantiations
            | Frontend
            | PerFunctionPasses
            | PerModulePasses
            | CodeGenPasses
            | Backend
            | ExecuteCompiler => Nothing,
            RunPass(s) | RunLoopPass(s) => String(s.clone()),
            Source(p) | OptModule(p) => FilePath(*p),
            ParseClass(i)
            | InstantiateClass(i)
            | ParseTemplate(i)
            | InstantiateFunction(i)
            | DebugType(i)
            | DebugGlobalVariable(i)
            | CodeGenFunction(i)
            | DebugFunction(i) => CppEntity(i.clone()),
            OptFunction(m) => MangledSymbol(m.clone()),
        }
    }

    /// Parse from useful bits of Duration events
    fn parse(
        name: Box<str>,
        args: Option<HashMap<Box<str>, json::Value>>,
        parser: &EntityParser,
    ) -> Result<Self, ActivityParseError> {
        // Interior mutability to allow multiple mutable borrows
        let args = RefCell::new(args);

        // Handling of activities with no arguments
        let no_args = |a: Activity| -> Result<Activity, ActivityParseError> {
            Self::parse_empty_args(args.borrow_mut().take())?;
            Ok(a)
        };

        // Handling of activities with one "detail" argument
        let detail_arg = || -> Result<Box<str>, ArgParseError> {
            Self::parse_detail_arg(args.borrow_mut().take())
        };
        //
        let fill_str_arg =
            |constructor: fn(Box<str>) -> Activity| -> Result<Activity, ActivityParseError> {
                Ok(constructor(detail_arg()?))
            };
        //
        let fill_path_arg =
            |constructor: fn(PathKey) -> Activity| -> Result<Activity, ActivityParseError> {
                let path: &str = &detail_arg()?;
                Ok(constructor(parser.path_to_key(path)))
            };

        // Parse the activity name and parse arguments accordingly
        use Activity::*;
        match &*name {
            "PerformPendingInstantiations" => no_args(PerformPendingInstantiations),
            "Frontend" => no_args(Frontend),
            "PerFunctionPasses" => no_args(PerFunctionPasses),
            "PerModulePasses" => no_args(PerModulePasses),
            "CodeGenPasses" => no_args(CodeGenPasses),
            "Backend" => no_args(Backend),
            "ExecuteCompiler" => no_args(ExecuteCompiler),
            "Source" => fill_path_arg(Source),
            "ParseClass" => fill_str_arg(ParseClass),
            "InstantiateClass" => fill_str_arg(InstantiateClass),
            "ParseTemplate" => fill_str_arg(ParseTemplate),
            "InstantiateFunction" => fill_str_arg(InstantiateFunction),
            "DebugType" => fill_str_arg(DebugType),
            "DebugGlobalVariable" => fill_str_arg(DebugGlobalVariable),
            "CodeGen Function" => fill_str_arg(CodeGenFunction),
            "DebugFunction" => fill_str_arg(DebugFunction),
            "RunPass" => fill_str_arg(RunPass),
            "OptFunction" => fill_str_arg(OptFunction),
            "RunLoopPass" => fill_str_arg(RunLoopPass),
            "OptModule" => fill_path_arg(OptModule),
            _ => Err(ActivityParseError::UnknownActivity(name.clone())),
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
}

/// What an activity can take as an argument
#[derive(Clone, Debug, PartialEq)]
pub enum ActivityArgument {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(Box<str>),

    /// An interned file path
    FilePath(PathKey),

    /// A C++ entity (class, function, ...)
    // TODO: Switch to a namespace + AST representation
    CppEntity(Box<str>),

    /// A C++ mangled symbol
    // TODO: Demangle, then switch to a namespace + AST representation, and if
    //       that works well consider unifying with the CppEntity type
    MangledSymbol(Box<str>),
}

/// What can go wrong while parsing an Activity
#[derive(Error, Debug, PartialEq)]
pub enum ActivityParseError {
    /// Encountered an unexpected activity name
    #[error("encountered unknown activity \"{0}\"")]
    UnknownActivity(Box<str>),

    /// Failed to parse activity arguments
    #[error("failed to parse activity arguments ({0})")]
    BadArguments(#[from] ArgParseError),

    /// Encountered an unexpected activity file path
    #[error("failed to parse activity file path ({0})")]
    BadFilePath(#[from] PathError),
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
        assert_eq!(
            Activity::parse(name.clone(), args.clone(), parser),
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
            ActivityStat::parse(make_event(true, 1, 0, None, None, None, None, None), parser),
            Ok(ActivityStat {
                activity: expected.clone(),
                start,
                duration,
            })
        );

        // Invalid inputs
        let test_bad_input = |input: TraceEvent| {
            assert_eq!(
                ActivityStat::parse(input.clone(), parser),
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
        let activity = Box::<str>::from("ThisIsMadness");
        assert_eq!(
            Activity::parse(activity.clone(), None, &parser),
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
        assert_eq!(
            Activity::parse(name.into(), Some(args.clone()), &parser),
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
        let missing_arg_error = Err(ActivityParseError::BadArguments(ArgParseError::MissingKey(
            "detail",
        )));
        assert_eq!(
            Activity::parse(name.clone(), None, &parser),
            missing_arg_error
        );
        assert_eq!(
            Activity::parse(name.clone(), Some(HashMap::new()), &parser),
            missing_arg_error
        );

        // Try providing a wrongly typed value
        let bad_value = json::json!(42usize);
        let bad_arg_value = maplit::hashmap! { "detail".into() => bad_value.clone() };
        assert_eq!(
            Activity::parse(name.clone(), Some(bad_arg_value), &parser),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedValue("detail", bad_value)
            ))
        );

        // Try adding a meaningless argument
        let mut bad_arg = good_args.clone();
        bad_arg.insert("wat".into(), json::json!(""));
        assert_eq!(
            Activity::parse(name, Some(bad_arg.clone()), &parser),
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
            Activity::Source(key.clone()),
            ActivityArgument::FilePath(key),
            parser,
        );
    }

    #[test]
    fn parse_class() {
        const CLASS: &'static str = "Acts::Test::MeasurementCreator";
        unary_test(
            "ParseClass",
            CLASS,
            Activity::ParseClass(CLASS.into()),
            ActivityArgument::CppEntity(CLASS.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn instantiate_class() {
        const CLASS: &'static str = "std::invoke_result<(lambda at /mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp:354:40), Acts::detail_lt::TrackStateProxy<Acts::Test::ExtendedMinimalSourceLink, 6, 6, true> >";
        unary_test(
            "InstantiateClass",
            CLASS,
            Activity::InstantiateClass(CLASS.into()),
            ActivityArgument::CppEntity(CLASS.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn parse_template() {
        const TEMPLATE: &'static str = "<unknown>"; // Yes, clang can do that
        unary_test(
            "ParseTemplate",
            TEMPLATE,
            Activity::ParseTemplate(TEMPLATE.into()),
            ActivityArgument::CppEntity(TEMPLATE.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn instantiate_function() {
        const FUNCTION: &'static str = "boost::unit_test::lazy_ostream_impl<boost::unit_test::lazy_ostream, boost::unit_test::basic_cstring<const char>, const boost::unit_test::basic_cstring<const char> &>::operator()";
        unary_test(
            "InstantiateFunction",
            FUNCTION,
            Activity::InstantiateFunction(FUNCTION.into()),
            ActivityArgument::CppEntity(FUNCTION.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_type() {
        const TYPE: &'static str = "generic_dense_assignment_kernel<DstEvaluatorType, SrcEvaluatorType, Eigen::internal::add_assign_op<double, double> >";
        unary_test(
            "DebugType",
            TYPE,
            Activity::DebugType(TYPE.into()),
            ActivityArgument::CppEntity(TYPE.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_global_variable() {
        const VAR: &'static str = "std::__detail::__variant::__gen_vtable<true, void, (lambda at /mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp:819:11) &&, std::variant<Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime> > &&>::_S_vtable";
        unary_test(
            "DebugGlobalVariable",
            VAR,
            Activity::DebugGlobalVariable(VAR.into()),
            ActivityArgument::CppEntity(VAR.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn code_gen_function() {
        const FUNCTION: &'static str =
            "boost::unit_test::operator<<<char, std::char_traits<char>, const char>";
        unary_test(
            "CodeGen Function",
            FUNCTION,
            Activity::CodeGenFunction(FUNCTION.into()),
            ActivityArgument::CppEntity(FUNCTION.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_function() {
        const FUNCTION: &'static str = "Eigen::operator*<Eigen::PermutationMatrix<6, 6, int>, Eigen::CwiseNullaryOp<Eigen::internal::scalar_identity_op<double>, Eigen::Matrix<double, 6, 6, 1, 6, 6> > >";
        unary_test(
            "DebugFunction",
            FUNCTION,
            Activity::DebugFunction(FUNCTION.into()),
            ActivityArgument::CppEntity(FUNCTION.into()),
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
        const FUNCTION: &'static str = "_GLOBAL__sub_I_CombinatorialKalmanFilterTests.cpp";
        unary_test(
            "OptFunction",
            FUNCTION,
            Activity::OptFunction(FUNCTION.into()),
            ActivityArgument::MangledSymbol(FUNCTION.into()),
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
