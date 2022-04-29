//! Clang provides a number of global execution statistics in -ftime-trace.
//!
//! These are attributed to nonzero TIDs, likely so that they appear as
//! dedicated lanes in Chrome's about:tracing viz.
//!
//! Their precise semantics (based on self time of all tasks ? children time of
//! toplevel tasks ?) are currently unknown.

use super::ArgParseError;
use crate::trace::{
    ctf::{events::duration::DurationEvent, TraceEvent},
    Duration,
};
use serde_json as json;
use std::collections::HashMap;
use thiserror::Error;

/// Global clang execution statistics for a certain kind of activity
///
/// The precise semantics are unknown: are we talking about top-level entities?
/// all entities? self time? children time? To be investigated...
///
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalStat {
    /// Execution duration
    total_duration: Duration,

    /// Number of occurences of this event
    count: usize,
}
//
impl GlobalStat {
    /// Decode a TraceEvent which is expected to contain global statistics
    pub fn parse(t: &TraceEvent) -> Result<(String, Self), GlobalStatParseError> {
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
            } if *tid != 0 && *ts == 0.0 => {
                // Global stats should have a name starting with a "Total "
                // string, which we shall strip since it brings no useful info
                let name = if let Some(stripped) = name.strip_prefix("Total ") {
                    stripped
                } else {
                    return Err(GlobalStatParseError::NoTotalPrefix);
                };

                // Parse arguments and emit result
                let args = GlobalStatArgs::parse(args)?;
                Ok((
                    name.to_owned(),
                    Self {
                        total_duration: *dur,
                        count: args.count as usize,
                    },
                ))
            }
            _ => Err(GlobalStatParseError::UnexpectedInput(t.clone())),
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

/// Things that can go wrong while parsing a GlobalStat
#[derive(Error, Debug, PartialEq)]
pub enum GlobalStatParseError {
    #[error("attempted to parse GlobalStat from unexpected {0:#?}")]
    UnexpectedInput(TraceEvent),

    #[error("lacking expected \"Total \" name prefix")]
    NoTotalPrefix,

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
    fn parse(args: &HashMap<String, json::Value>) -> Result<Self, ArgParseError> {
        // Process arguments
        let mut count = None;
        let mut _avg_ms = None;
        for (k, v) in args {
            match &**k {
                "count" => {
                    if let Some(c) = v.as_u64() {
                        count = Some(c);
                    } else {
                        return Err(ArgParseError::UnexpectedValue("count", v.clone()));
                    }
                }
                "avg ms" => {
                    if let Some(f) = v.as_f64() {
                        _avg_ms = Some(f);
                    } else {
                        return Err(ArgParseError::UnexpectedValue("avg ms", v.clone()));
                    }
                }
                _ => {
                    return Err(ArgParseError::UnexpectedKeys(args.clone()));
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

    // FIXME: Add some tests of GlobalStat::parse

    #[test]
    fn global_stat_args() {
        // Check parsing of a complete argument sequence
        let count = 123u64;
        let _avg_ms = 45.6f64;
        let correct_args = maplit::hashmap! {
            "count".to_owned() => json::json!(count),
            "avg ms".to_owned() => json::json!(_avg_ms),
        };
        assert_eq!(
            GlobalStatArgs::parse(&correct_args),
            Ok(GlobalStatArgs { count, _avg_ms })
        );

        // Try adding an extra argument
        let mut extra_arg = correct_args.clone();
        extra_arg.insert("wat".to_owned(), json::json!(""));
        assert_eq!(
            GlobalStatArgs::parse(&extra_arg),
            Err(ArgParseError::UnexpectedKeys(extra_arg))
        );

        // Test for replacing the arguments with wrongly typed values
        let test_bad_value = |key: &'static str| {
            let mut bad_value = correct_args.clone();
            bad_value.insert(key.to_owned(), json::json!(""));
            assert_eq!(
                GlobalStatArgs::parse(&bad_value),
                Err(ArgParseError::UnexpectedValue(key, bad_value[key].clone()))
            );
        };

        // Test for removing an expected argument
        let test_missing_key = |key: &'static str| {
            let mut missing_key = correct_args.clone();
            missing_key.remove(key);
            assert_eq!(
                GlobalStatArgs::parse(&missing_key),
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
