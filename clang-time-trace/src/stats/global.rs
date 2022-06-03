//! Handling of global execution statistics

use super::ArgParseError;
use crate::{
    ctf::{events::duration::DurationEvent, TraceEvent},
    Duration,
};
use serde_json as json;
use std::collections::HashMap;
use thiserror::Error;

/// Global clang execution statistics for a certain kind of activity
///
/// According to the LLVM source code, this is the sum of of the durations of
/// the topmost activities of this type in the LLVM call stack: if an activity
/// recursively calls itself, double-counting is avoided.
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalStat {
    /// Execution duration
    total_duration: Duration,

    /// Number of occurences of this event
    count: usize,
}
//
impl GlobalStat {
    /// Build new global execution statistics
    pub fn new(total_duration: Duration, count: usize) -> Self {
        Self {
            total_duration,
            count,
        }
    }

    /// Decode a TraceEvent which is expected to contain global statistics
    pub fn parse(t: TraceEvent) -> Result<(Box<str>, Self), GlobalStatParseError> {
        match t {
            TraceEvent::X {
                duration_event:
                    DurationEvent {
                        pid: 1,
                        tid,
                        ts,
                        name: Some(name),
                        cat: None,
                        tts: None,
                        args: Some(args),
                        stack_trace: None,
                    },
                dur,
                tdur: None,
                end_stack_trace: None,
            } if tid != 0 && ts == 0.0 => {
                // Global stats should have a name starting with a "Total "
                // string, which we shall strip since it brings no useful info
                let name = if let Some(stripped) = name.strip_prefix("Total ") {
                    stripped
                } else {
                    return Err(GlobalStatParseError::NoTotalPrefix(name));
                };

                // Parse arguments and emit result
                let args = GlobalStatArgs::parse(args)?;
                Ok((
                    name.into(),
                    Self {
                        total_duration: dur,
                        count: args.count as usize,
                    },
                ))
            }
            _ => Err(GlobalStatParseError::UnexpectedInput(t)),
        }
    }

    /// Total execution duration across all events
    pub fn total_duration(&self) -> Duration {
        self.total_duration
    }

    /// Number of occurences of this event
    pub fn count(&self) -> usize {
        self.count
    }

    /// Average duration of this event
    pub fn average_duration(&self) -> Duration {
        self.total_duration / (self.count as Duration)
    }
}

/// What can go wrong while parsing a GlobalStat
#[derive(Error, Debug, PartialEq)]
pub enum GlobalStatParseError {
    /// Encountered unexpected input
    #[error("attempted to parse GlobalStat from unexpected {0:#?}")]
    UnexpectedInput(TraceEvent),

    /// Event named lacked the usual "Total " prefix
    #[error("GlobalStat name \"{0}\" lacked expected \"Total \" prefix")]
    NoTotalPrefix(Box<str>),

    /// Failed to parse GlobalStat arguments
    #[error("failed to parse activity arguments ({0})")]
    BadArguments(#[from] ArgParseError),
}

/// Arguments to global execution statistics events
#[derive(Debug, Default, PartialEq)]
struct GlobalStatArgs {
    /// Number of events of this kind
    count: u64,

    /// Average time per event in milliseconds
    _avg_ms: f64,
}
//
impl GlobalStatArgs {
    /// Parse global execution statistics arguments
    fn parse(args: HashMap<Box<str>, json::Value>) -> Result<Self, ArgParseError> {
        // Process arguments
        let mut count = None;
        let mut _avg_ms = None;
        let mut args_iter = args.into_iter();
        while let Some((k, v)) = args_iter.next() {
            match &*k {
                "count" => {
                    if let Some(c) = v.as_u64() {
                        count = Some(c);
                    } else {
                        return Err(ArgParseError::UnexpectedValue("count", v));
                    }
                }
                "avg ms" => {
                    if let Some(f) = v.as_f64() {
                        _avg_ms = Some(f);
                    } else {
                        return Err(ArgParseError::UnexpectedValue("avg ms", v));
                    }
                }
                _ => {
                    let mut remainder = HashMap::from_iter(args_iter);
                    remainder.insert(k, v);
                    remainder.remove("count");
                    remainder.remove("avg ms");
                    return Err(ArgParseError::UnexpectedKeys(remainder));
                }
            }
        }

        // Make sure all arguments were provided, emit result
        match (count, _avg_ms) {
            (Some(count), Some(_avg_ms)) => Ok(Self { count, _avg_ms }),
            (None, _) => Err(ArgParseError::MissingKey("count")),
            (_, None) => Err(ArgParseError::MissingKey("avg ms")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ctf::{
        stack::{EndStackTrace, StackFrameId, StackTrace},
        EventCategories,
    };

    #[test]
    fn global_stat_accessors() {
        let global_stat = GlobalStat {
            total_duration: 12.3,
            count: 10,
        };
        assert_eq!(global_stat.total_duration(), global_stat.total_duration);
        assert_eq!(global_stat.count(), global_stat.count);
        assert_eq!(
            global_stat.average_duration(),
            global_stat.total_duration / (global_stat.count as Duration)
        );
    }

    #[test]
    fn global_stat_parse() {
        // Generate a set of correct arguments and the associated duration
        let count = 1usize << 3;
        let _avg_ms = 4.5f64;
        let args = maplit::hashmap! {
            "count".into() => json::json!(count),
            "avg ms".into() => json::json!(_avg_ms),
        };
        let total_duration = (count as f64) * _avg_ms;

        // Have a way to generate good and bad test inputs
        let make_event =
            |good_type, pid, tid, name, cat, tts, stack_trace, tdur, end_stack_trace| {
                let duration_event = DurationEvent {
                    pid,
                    tid,
                    ts: 0.0,
                    name,
                    cat,
                    tts,
                    args: Some(args.clone()),
                    stack_trace,
                };
                if good_type {
                    TraceEvent::X {
                        duration_event,
                        dur: total_duration,
                        tdur,
                        end_stack_trace,
                    }
                } else {
                    TraceEvent::B(duration_event)
                }
            };

        // Valid GlobalStat::parse input
        let good_name = Some("Total ExecuteCompiler".into());
        assert_eq!(
            GlobalStat::parse(make_event(
                true,
                1,
                1,
                good_name.clone(),
                None,
                None,
                None,
                None,
                None
            )),
            Ok((
                "ExecuteCompiler".into(),
                GlobalStat {
                    total_duration,
                    count,
                }
            ))
        );

        // Invalid name
        let bad_name = Box::<str>::from("ExecuteCompiler");
        assert_eq!(
            GlobalStat::parse(make_event(
                true,
                1,
                1,
                Some(bad_name.clone()),
                None,
                None,
                None,
                None,
                None
            )),
            Err(GlobalStatParseError::NoTotalPrefix(bad_name))
        );

        // Various flavors of unexpected input
        let test_unexpected_input = |input: TraceEvent| {
            assert_eq!(
                GlobalStat::parse(input.clone()),
                Err(GlobalStatParseError::UnexpectedInput(input))
            )
        };
        test_unexpected_input(make_event(
            false,
            1,
            1,
            good_name.clone(),
            None,
            None,
            None,
            None,
            None,
        ));
        test_unexpected_input(make_event(
            true,
            0,
            1,
            good_name.clone(),
            None,
            None,
            None,
            None,
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            0,
            good_name.clone(),
            None,
            None,
            None,
            None,
            None,
        ));
        test_unexpected_input(make_event(true, 1, 1, None, None, None, None, None, None));
        test_unexpected_input(make_event(
            true,
            1,
            1,
            good_name.clone(),
            Some(EventCategories::default()),
            None,
            None,
            None,
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            1,
            good_name.clone(),
            None,
            Some(0.0),
            None,
            None,
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            1,
            good_name.clone(),
            None,
            None,
            Some(StackTrace::sf(StackFrameId::default())),
            None,
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            1,
            good_name.clone(),
            None,
            None,
            None,
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            1,
            good_name.clone(),
            None,
            None,
            None,
            None,
            Some(EndStackTrace::esf(StackFrameId::default())),
        ));
    }

    #[test]
    fn global_stat_args() {
        // Check parsing of a complete argument sequence
        let count = 123u64;
        let _avg_ms = 45.6f64;
        let correct_args = maplit::hashmap! {
            "count".into() => json::json!(count),
            "avg ms".into() => json::json!(_avg_ms),
        };
        assert_eq!(
            GlobalStatArgs::parse(correct_args.clone()),
            Ok(GlobalStatArgs { count, _avg_ms })
        );

        // Try adding an extra argument
        let mut extra_arg = correct_args.clone();
        extra_arg.insert("wat".into(), json::json!(""));
        assert_eq!(
            GlobalStatArgs::parse(extra_arg.clone()),
            Err(ArgParseError::UnexpectedKeys(
                maplit::hashmap! { "wat".into() => json::json!("") }
            ))
        );

        // Test for replacing the arguments with wrongly typed values
        let test_bad_value = |key: &'static str| {
            let mut bad_value = correct_args.clone();
            bad_value.insert(key.into(), json::json!(""));
            assert_eq!(
                GlobalStatArgs::parse(bad_value.clone()),
                Err(ArgParseError::UnexpectedValue(key, bad_value[key].clone()))
            );
        };

        // Test for removing an expected argument
        let test_missing_key = |key: &'static str| {
            let mut missing_key = correct_args.clone();
            missing_key.remove(key);
            assert_eq!(
                GlobalStatArgs::parse(missing_key),
                Err(ArgParseError::MissingKey(key))
            );
        };

        // Run above tests for all expected keys
        for key in ["count", "avg ms"] {
            test_bad_value(key);
            test_missing_key(key);
        }
    }
}
