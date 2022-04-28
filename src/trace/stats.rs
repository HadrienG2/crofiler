//! Clang provides a number of global execution statistics in -ftime-trace.
//!
//! These are attributed to nonzero TIDs, likely so that they appear as
//! dedicated lanes in Chrome's about:tracing viz.
//!
//! Their precise semantics (based on self time of all tasks ? children time of
//! toplevel tasks ?) are currently unknown.

use super::{
    parser::{events::duration::DurationEvent, TraceEvent},
    Duration,
};
use serde_json as json;
use thiserror::Error;

/// Global clang execution statistics for a certain kind of activity
///
/// The precise semantics are unknown: are we talking about top-level entities?
/// all entities? self time? children time?
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

                // Check the arguments
                let mut count = None;
                let mut avg_ms = None;
                for (k, v) in args {
                    match &**k {
                        "count" => {
                            if let Some(c) = v.as_u64() {
                                count = Some(c);
                            } else {
                                return Err(GlobalStatParseError::NonIntegerCount(v.clone()));
                            }
                        }
                        "avg ms" => {
                            if let Some(f) = v.as_f64() {
                                avg_ms = Some(f);
                            } else {
                                return Err(GlobalStatParseError::NonFloatAvgMs(v.clone()));
                            }
                        }
                        _ => {
                            return Err(GlobalStatParseError::UnexpectedArgument(
                                k.clone(),
                                v.clone(),
                            ))
                        }
                    }
                }
                if let None = avg_ms {
                    return Err(GlobalStatParseError::MissingAvgMs);
                }
                if let Some(count) = count {
                    Ok((
                        name.to_owned(),
                        GlobalStat {
                            total_duration: *dur,
                            count: count as usize,
                        },
                    ))
                } else {
                    Err(GlobalStatParseError::MissingCount)
                }
            }
            _ => Err(GlobalStatParseError::UnexpectedTraceEvent(t.clone())),
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
    pub fn avg_duration(&self) -> Duration {
        self.total_duration / (self.count as Duration)
    }
}

/// Things that can go wrong while parsing a GlobalStat
#[derive(Error, Debug, PartialEq)]
pub enum GlobalStatParseError {
    #[error("lacking expected \"Total\" name prefix")]
    NoTotalPrefix,

    #[error("lacking \"count\" argument")]
    MissingCount,

    #[error("\"count\" argument is not an integer: {0:?}")]
    NonIntegerCount(json::Value),

    #[error("lacking \"avg ms\" argument")]
    MissingAvgMs,

    #[error("\"avg ms\" argument is not a float: {0:?}")]
    NonFloatAvgMs(json::Value),

    #[error("unexpected \"{0}\": {1} argument")]
    UnexpectedArgument(String, json::Value),

    #[error("unexpected {0:#?}")]
    UnexpectedTraceEvent(TraceEvent),
}

// FIXME: Add some tests
