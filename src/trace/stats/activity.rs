//! Handling of activities, that is, clang-provided categories of stuff it can
//! be doing at a point in time

use super::ArgParseError;
use crate::trace::ctf::{events::duration::DurationEvent, Duration, Timestamp, TraceEvent};
use serde_json as json;
use std::collections::HashMap;
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
    /// Decode a TraceEvent which is expected to contain a timed activity
    pub fn parse(t: &TraceEvent) -> Result<Self, ActivityStatParseError> {
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
                let activity = Activity::parse(name, args)?;
                Ok(Self {
                    activity,
                    start: *ts,
                    duration: *dur,
                })
            }
            _ => Err(ActivityStatParseError::UnexpectedInput(t.clone())),
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
    #[error("attempted to parse GlobalStat from unexpected {0:#?}")]
    UnexpectedInput(TraceEvent),

    #[error("failed to parse activity ({0})")]
    BadArguments(#[from] ActivityParseError),
}

/// Activity that Clang can engage in during the compilation process
#[derive(Clone, Debug, PartialEq)]
pub enum Activity {
    /// Processing a source file
    // TODO: Switch to Path + normalize
    Source(String),

    /// Parsing a class
    // TODO: Switch to a namespace + AST representation
    ParseClass(String),

    /// Instantiating a class
    // TODO: Switch to a namespace + AST representation
    InstantiateClass(String),

    /// Parsing a template
    // TODO: Switch to a namespace + AST representation
    ParseTemplate(String),

    /// Instantiating a function
    // TODO: Switch to a namespace + AST representation
    InstantiateFunction(String),

    /// Generating debug info for a type
    // TODO: Switch to a namespace + AST representation
    DebugType(String),

    /// Generating debug info for a global variable
    // TODO: Switch to a namespace + AST representation
    DebugGlobalVariable(String),

    /// Generate a function's code
    // TODO: Switch to a namespace + AST representation
    CodeGenFunction(String),

    /// Generating debug info for a function
    // TODO: Switch to a namespace + AST representation
    DebugFunction(String),

    /// Perform pending instantiations (as the name suggests)
    PerformPendingInstantiations,

    /// Compiler front-end work
    Frontend,

    /// Running a named compiler pass
    RunPass(String),

    /// Optimizing code
    // TODO: Demangle then switch to a namespace + AST representation
    OptFunction(String),

    /// Per-function compiler passes
    PerFunctionPasses,

    /// Running a named loop compiler pass
    RunLoopPass(String),

    /// Optimizing a module
    // TODO: Switch to Path + normalize
    OptModule(String),

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
    /// Parse from useful bits of Duration events
    fn parse(
        name: &String,
        args: &Option<HashMap<String, json::Value>>,
    ) -> Result<Self, ActivityParseError> {
        // Handling of activities with no arguments
        let assert_no_args = |a: Activity| -> Result<Activity, ActivityParseError> {
            Self::parse_empty_args(args)?;
            Ok(a)
        };

        // Handling of activities with one "detail" argument
        let fill_detail_arg =
            |constructor: fn(String) -> Activity| -> Result<Activity, ActivityParseError> {
                Ok(constructor(Self::parse_detail_arg(args)?))
            };

        // Parse the activity name and parse arguments accordingly
        match &**name {
            "PerformPendingInstantiations" => {
                assert_no_args(Activity::PerformPendingInstantiations)
            }
            "Frontend" => assert_no_args(Activity::Frontend),
            "PerFunctionPasses" => assert_no_args(Activity::PerFunctionPasses),
            "PerModulePasses" => assert_no_args(Activity::PerModulePasses),
            "CodeGenPasses" => assert_no_args(Activity::CodeGenPasses),
            "Backend" => assert_no_args(Activity::Backend),
            "ExecuteCompiler" => assert_no_args(Activity::ExecuteCompiler),
            "Source" => fill_detail_arg(Activity::Source),
            "ParseClass" => fill_detail_arg(Activity::ParseClass),
            "InstantiateClass" => fill_detail_arg(Activity::InstantiateClass),
            "ParseTemplate" => fill_detail_arg(Activity::ParseTemplate),
            "InstantiateFunction" => fill_detail_arg(Activity::InstantiateFunction),
            "DebugType" => fill_detail_arg(Activity::DebugType),
            "DebugGlobalVariable" => fill_detail_arg(Activity::DebugGlobalVariable),
            "CodeGen Function" => fill_detail_arg(Activity::CodeGenFunction),
            "DebugFunction" => fill_detail_arg(Activity::DebugFunction),
            "RunPass" => fill_detail_arg(Activity::RunPass),
            "OptFunction" => fill_detail_arg(Activity::OptFunction),
            "RunLoopPass" => fill_detail_arg(Activity::RunLoopPass),
            "OptModule" => fill_detail_arg(Activity::OptModule),
            _ => Err(ActivityParseError::UnknownActivity(name.clone())),
        }
    }

    /// Check for absence of arguments
    fn parse_empty_args(args: &Option<HashMap<String, json::Value>>) -> Result<(), ArgParseError> {
        if let Some(args) = args {
            if args.is_empty() {
                Ok(())
            } else {
                Err(ArgParseError::UnexpectedKeys(args.clone()))
            }
        } else {
            Ok(())
        }
    }

    /// Parse a single "detail" string argument
    fn parse_detail_arg(
        args: &Option<HashMap<String, json::Value>>,
    ) -> Result<String, ArgParseError> {
        if let Some(args) = args {
            let mut args_iter = args.iter();
            if let Some((k, v)) = args_iter.next() {
                if k != "detail" {
                    return Err(ArgParseError::UnexpectedKeys(args.clone()));
                }
                let s = if let json::Value::String(s) = v {
                    s
                } else {
                    return Err(ArgParseError::UnexpectedValue("detail", v.clone()));
                };
                if args_iter.next().is_some() {
                    return Err(ArgParseError::UnexpectedKeys(args.clone()));
                }
                Ok(s.clone())
            } else {
                Err(ArgParseError::MissingKey("detail"))
            }
        } else {
            Err(ArgParseError::MissingKey("detail"))
        }
    }
}

/// What can go wrong while parsing an Activity
#[derive(Error, Debug, PartialEq)]
pub enum ActivityParseError {
    #[error("encountered unknown activity \"{0}\"")]
    UnknownActivity(String),

    #[error("failed to parse activity arguments ({0})")]
    BadArguments(#[from] ArgParseError),
}

#[cfg(test)]
mod tests {
    use super::*;

    // FIXME: Add ActivityStat::parse tests

    #[test]
    fn unknown_activity() {
        let activity = "ThisIsMadness".to_owned();
        assert_eq!(
            Activity::parse(&activity, &None),
            Err(ActivityParseError::UnknownActivity(activity))
        );
    }

    fn nullary_test(name: &str, a: Activity) {
        let name = name.to_owned();
        assert_eq!(Activity::parse(&name, &None), Ok(a.clone()));
        assert_eq!(Activity::parse(&name, &Some(HashMap::new())), Ok(a));
        let args = maplit::hashmap! { "detail".to_owned() => json::json!("") };
        assert_eq!(
            Activity::parse(&name, &Some(args.clone())),
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

    fn unary_test(name: &str, arg: &str, a: Activity) {
        let name = name.to_owned();
        const ARG_NAME: &'static str = "detail";

        let missing_arg_error = Err(ActivityParseError::BadArguments(ArgParseError::MissingKey(
            ARG_NAME,
        )));
        assert_eq!(Activity::parse(&name, &None), missing_arg_error);
        assert_eq!(
            Activity::parse(&name, &Some(HashMap::new())),
            missing_arg_error
        );

        let good_args = maplit::hashmap! { ARG_NAME.to_owned() => json::json!(arg) };
        assert_eq!(Activity::parse(&name, &Some(good_args)), Ok(a));

        let bad_value = json::json!(42usize);
        let bad_arg_value = maplit::hashmap! { ARG_NAME.to_owned() => bad_value.clone() };
        assert_eq!(
            Activity::parse(&name, &Some(bad_arg_value)),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedValue(ARG_NAME, bad_value)
            ))
        );

        let bad_arg = maplit::hashmap! { "wat".to_owned() => json::json!("") };
        assert_eq!(
            Activity::parse(&name, &Some(bad_arg.clone())),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedKeys(bad_arg)
            ))
        );
    }

    #[test]
    fn source() {
        const PATH: &'static str =
            "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
        unary_test("Source", PATH, Activity::Source(PATH.to_owned()));
    }

    #[test]
    fn parse_class() {
        const CLASS: &'static str = "Acts::Test::MeasurementCreator";
        unary_test("ParseClass", CLASS, Activity::ParseClass(CLASS.to_owned()));
    }

    #[test]
    fn instantiate_class() {
        const CLASS: &'static str = "std::invoke_result<(lambda at /mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp:354:40), Acts::detail_lt::TrackStateProxy<Acts::Test::ExtendedMinimalSourceLink, 6, 6, true> >";
        unary_test(
            "InstantiateClass",
            CLASS,
            Activity::InstantiateClass(CLASS.to_owned()),
        );
    }

    #[test]
    fn parse_template() {
        const TEMPLATE: &'static str = "<unknown>"; // Yes, clang can do that
        unary_test(
            "ParseTemplate",
            TEMPLATE,
            Activity::ParseTemplate(TEMPLATE.to_owned()),
        );
    }

    #[test]
    fn instantiate_function() {
        const FUNCTION: &'static str = "boost::unit_test::lazy_ostream_impl<boost::unit_test::lazy_ostream, boost::unit_test::basic_cstring<const char>, const boost::unit_test::basic_cstring<const char> &>::operator()";
        unary_test(
            "InstantiateFunction",
            FUNCTION,
            Activity::InstantiateFunction(FUNCTION.to_owned()),
        );
    }

    #[test]
    fn debug_type() {
        const TYPE: &'static str = "generic_dense_assignment_kernel<DstEvaluatorType, SrcEvaluatorType, Eigen::internal::add_assign_op<double, double> >";
        unary_test("DebugType", TYPE, Activity::DebugType(TYPE.to_owned()));
    }

    #[test]
    fn debug_global_variable() {
        const VAR: &'static str = "std::__detail::__variant::__gen_vtable<true, void, (lambda at /mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp:819:11) &&, std::variant<Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime> > &&>::_S_vtable";
        unary_test(
            "DebugGlobalVariable",
            VAR,
            Activity::DebugGlobalVariable(VAR.to_owned()),
        );
    }

    #[test]
    fn code_gen_function() {
        const FUNCTION: &'static str =
            "boost::unit_test::operator<<<char, std::char_traits<char>, const char>";
        unary_test(
            "CodeGen Function",
            FUNCTION,
            Activity::CodeGenFunction(FUNCTION.to_owned()),
        );
    }

    #[test]
    fn debug_function() {
        const FUNCTION: &'static str = "Eigen::operator*<Eigen::PermutationMatrix<6, 6, int>, Eigen::CwiseNullaryOp<Eigen::internal::scalar_identity_op<double>, Eigen::Matrix<double, 6, 6, 1, 6, 6> > >";
        unary_test(
            "DebugFunction",
            FUNCTION,
            Activity::DebugFunction(FUNCTION.to_owned()),
        );
    }

    #[test]
    fn run_pass() {
        const PASS: &'static str = "X86 DAG->DAG Instruction Selection";
        unary_test("RunPass", PASS, Activity::RunPass(PASS.to_owned()));
    }

    #[test]
    fn opt_function() {
        const FUNCTION: &'static str = "_GLOBAL__sub_I_CombinatorialKalmanFilterTests.cpp";
        unary_test(
            "OptFunction",
            FUNCTION,
            Activity::OptFunction(FUNCTION.to_owned()),
        );
    }

    #[test]
    fn run_loop_pass() {
        const PASS: &'static str = "Induction Variable Users";
        unary_test("RunLoopPass", PASS, Activity::RunLoopPass(PASS.to_owned()));
    }

    #[test]
    fn opt_module() {
        const MODULE: &'static str =
            "/mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp";
        unary_test("OptModule", MODULE, Activity::OptModule(MODULE.to_owned()));
    }
}
