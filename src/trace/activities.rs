//! Handling of activities, that is, clang-provided categories of stuff it can
//! be doing at a point in time

use serde_json as json;
use std::collections::HashMap;
use thiserror::Error;

/// Clang activity without profiling information
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

    /// Instantiating a template
    // TODO: Switch to a namespace + AST representation, beware that this can contain "<unknown>", check for others
    InstantiateTemplate(String),

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
    pub fn parse(
        name: &String,
        args: &Option<HashMap<String, json::Value>>,
    ) -> Result<Self, ActivityParseError> {
        // Handling of activities with no arguments
        let assert_nullary = |a: Activity| -> Result<Activity, ActivityParseError> {
            if let Some(args) = args {
                if args.is_empty() {
                    Ok(a)
                } else {
                    Err(ActivityParseError::UnexpectedArguments(args.clone()))
                }
            } else {
                Ok(a)
            }
        };

        // Handling of activities with one "detail" argument
        let unary_argument = || -> Result<String, ActivityParseError> {
            const ARG_NAME: &'static str = "detail";
            if let Some(args) = args {
                let mut args_iter = args.iter();
                if let Some((k, v)) = args_iter.next() {
                    if k != ARG_NAME {
                        return Err(ActivityParseError::UnexpectedArguments(args.clone()));
                    }
                    let s = if let json::Value::String(s) = v {
                        s
                    } else {
                        return Err(ActivityParseError::UnexpectedArgumentValue(
                            ARG_NAME,
                            v.clone(),
                        ));
                    };
                    if args_iter.next().is_some() {
                        return Err(ActivityParseError::UnexpectedArguments(args.clone()));
                    }
                    Ok(s.clone())
                } else {
                    Err(ActivityParseError::MissingArgument(ARG_NAME))
                }
            } else {
                Err(ActivityParseError::MissingArgument(ARG_NAME))
            }
        };

        // Parse the activity name and parse arguments accordingly
        match &**name {
            "PerformPendingInstantiations" => {
                assert_nullary(Activity::PerformPendingInstantiations)
            }
            "Frontend" => assert_nullary(Activity::Frontend),
            "PerFunctionPasses" => assert_nullary(Activity::PerFunctionPasses),
            "PerModulePasses" => assert_nullary(Activity::PerModulePasses),
            "CodeGenPasses" => assert_nullary(Activity::CodeGenPasses),
            "Backend" => assert_nullary(Activity::Backend),
            "ExecuteCompiler" => assert_nullary(Activity::ExecuteCompiler),
            _ => {
                let arg = unary_argument()?;
                match &**name {
                    "Source" => Ok(Activity::Source(arg)),
                    "ParseClass" => dbg!(Ok(Activity::ParseClass(arg))),
                    "InstantiateClass" => Ok(Activity::InstantiateClass(arg)),
                    "InstantiateTemplate" => Ok(Activity::InstantiateTemplate(arg)),
                    "ParseTemplate" => Ok(Activity::ParseTemplate(arg)),
                    "InstantiateFunction" => Ok(Activity::InstantiateFunction(arg)),
                    "DebugType" => Ok(Activity::DebugType(arg)),
                    "DebugGlobalVariable" => Ok(Activity::DebugGlobalVariable(arg)),
                    "CodeGen Function" => Ok(Activity::CodeGenFunction(arg)),
                    "DebugFunction" => Ok(Activity::DebugFunction(arg)),
                    "RunPass" => Ok(Activity::RunPass(arg)),
                    "OptFunction" => Ok(Activity::OptFunction(arg)),
                    "RunLoopPass" => Ok(Activity::RunLoopPass(arg)),
                    "OptModule" => Ok(Activity::OptModule(arg)),
                    _ => Err(ActivityParseError::UnknownActivity(name.clone())),
                }
            }
        }
    }
}

/// Things that can go wrong while parsing an Activity
#[derive(Error, Debug, PartialEq)]
pub enum ActivityParseError {
    #[error("encountered unknown activity \"{0}\"")]
    UnknownActivity(String),
    #[error("expected activity argument \"{0}\" was not found")]
    MissingArgument(&'static str),
    #[error("Got unexpected value for argument \"{0}\": {1:?}")]
    UnexpectedArgumentValue(&'static str, json::Value),
    #[error("received unexpected activity arguments {0:?}")]
    UnexpectedArguments(HashMap<String, json::Value>),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn nullary_test(name: &str, a: Activity) {
        let name = name.to_owned();
        assert_eq!(Activity::parse(&name, &None), Ok(a.clone()));
        assert_eq!(Activity::parse(&name, &Some(HashMap::new())), Ok(a));
        let args = maplit::hashmap! { "detail".to_owned() => json::json!("") };
        assert_eq!(
            Activity::parse(&name, &Some(args.clone())),
            Err(ActivityParseError::UnexpectedArguments(args))
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

        let missing_arg_error = Err(ActivityParseError::MissingArgument(ARG_NAME));
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
            Err(ActivityParseError::UnexpectedArgumentValue(
                ARG_NAME, bad_value
            ))
        );

        let bad_arg = maplit::hashmap! { "wat".to_owned() => json::json!("") };
        assert_eq!(
            Activity::parse(&name, &Some(bad_arg.clone())),
            Err(ActivityParseError::UnexpectedArguments(bad_arg))
        );
    }

    #[test]
    fn source() {
        const PATH: &'static str =
            "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
        unary_test("Source", PATH, Activity::Source(PATH.to_owned()));
    }

    /* TODO
    "ParseClass" => Ok(Activity::ParseClass(arg)),
    "InstantiateClass" => Ok(Activity::InstantiateClass(arg)),
    "InstantiateTemplate" => Ok(Activity::InstantiateTemplate(arg)),
    "ParseTemplate" => Ok(Activity::ParseTemplate(arg)),
    "InstantiateFunction" => Ok(Activity::InstantiateFunction(arg)),
    "DebugType" => Ok(Activity::DebugType(arg)),
    "DebugGlobalVariable" => Ok(Activity::DebugGlobalVariable(arg)),
    "CodeGen Function" => Ok(Activity::CodeGenFunction(arg)),
    "DebugFunction" => Ok(Activity::DebugFunction(arg)),
    "RunPass" => Ok(Activity::RunPass(arg)),
    "OptFunction" => Ok(Activity::OptFunction(arg)),
    "RunLoopPass" => Ok(Activity::RunLoopPass(arg)),
    "OptModule" => Ok(Activity::OptModule(arg)), */
}
