//! Ergonomic representation of the output from clang's -ftime-trace, with a
//! mechanism to load and parse it.

#![deny(missing_docs)]

mod ctf;
mod metadata;
mod stats;
mod tree;

use self::{
    ctf::{events::metadata::MetadataEvent, TraceDataObject, TraceEvent},
    stats::activity::ActivityStat,
    tree::{ActivityTree, ActivityTreeBuilder},
};
use cpparser::{EntityKey, EntityParser, EntityView};
use serde_json as json;
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    path::Path,
    str::FromStr,
};
use thiserror::Error;

// Reexport types which appear in the public interface
pub use self::{
    ctf::{Duration, Pid, Timestamp, DAY, HOUR, MICROSECOND, MILLISECOND, MINUTE, SECOND},
    metadata::NameParseError,
    stats::{
        activity::{
            argument::{
                ActivityArgument, ActivityArgumentError, ActivityArgumentType, MangledSymbol,
                ParsedActivityArgument, ParsedMangledSymbol, RawActivityArgument,
            },
            Activity, ActivityId, ActivityParseError, ActivityStatParseError,
        },
        global::{GlobalStat, GlobalStatParseError},
        ArgParseError,
    },
    tree::{ActivityTrace, ActivityTraceId, ActivityTreeError},
};
pub use cpparser::{
    asylum::path::{InternedComponent, PathError},
    display::CustomDisplay,
    PathComponentKey, PathKey,
};
pub use json::Error as CtfParseError;

/// Interned path
pub type InternedPath<'interner> = cpparser::InternedPath<'interner>;

/// Simplified -ftime-trace profile from a clang execution
pub struct ClangTrace {
    /// Clang activities recorded by -ftime-trace
    activities: ActivityTree,

    /// Interned C++ entities and file paths within activities
    entities: EntityParser,

    /// Buffer used for symbol demangling
    demangling_buf: String,

    /// Global statistics
    global_stats: HashMap<Box<str>, GlobalStat>,

    /// Name of the clang process
    process_name: Box<str>,

    /// Pid of the clang process, if not obviously invalid
    pid: Option<Pid>,

    /// Name of the clang thread, if known
    thread_name: Option<Box<str>>,

    /// Beginning of time, if specified
    beginning_of_time: Option<Duration>,
}
//
impl ClangTrace {
    /// Load from clang -ftime-trace output in a file
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, ClangTraceLoadError> {
        // Load JSON data from the input file and parse it as CTF JSON
        let mut profile_str = String::new();
        File::open(path)?.read_to_string(&mut profile_str)?;
        Ok(Self::from_str(&profile_str)?)
    }

    /// Activities that were directly spawned by the clang driver
    ///
    /// From this, you can recursively iterate over child tasks in order to
    /// construct a hierarchical execution profile. The order in which root
    /// activities are emitted by this iterator is unspecified.
    pub fn root_activities(&self) -> impl Iterator<Item = ActivityTrace> + Clone {
        self.activities.root_activities()
    }

    /// Complete list of activities that clang engaged in
    ///
    /// You can get a temporal trace of everything that happened by sorting this
    /// in ascending start() order and you can get a flat time profile by
    /// sorting this in descending self_duration() order. The order in which
    /// activities are emitted by this iterator is unspecified.
    ///
    /// When using such flat iteration, be careful not to double-count quantites
    /// that are aggregated over transitive children of each activity, including
    /// duration() and anything derived from all_children().
    pub fn all_activities(&self) -> impl Iterator<Item = ActivityTrace> + Clone {
        self.activities.all_activities()
    }

    /// Retrieve an activity by a previously acquired identifier
    pub fn activity_trace(&self, id: ActivityTraceId) -> ActivityTrace {
        self.activities.activity_trace(id)
    }

    /// Global statistics on clang activities
    ///
    /// LLVM has double-counting protection when an activity calls itself
    /// recursively, but not when it calls other activities, so you should
    /// be careful when comparing statistics associated with different
    /// activities: one timing may include a subset of the other. For the same
    /// reason, summing statistics would be highly ill-advised.
    pub fn global_stats(&self) -> &HashMap<Box<str>, GlobalStat> {
        &self.global_stats
    }

    /// Name of the clang process that acquired this data
    pub fn process_name(&self) -> &str {
        &self.process_name
    }

    /// Process identifier of the clang process that acquired this data
    pub fn pid(&self) -> Option<Pid> {
        self.pid
    }

    /// Name of the clang thread that acquired this data
    pub fn thread_name(&self) -> Option<&str> {
        self.thread_name.as_ref().map(|s| s.as_ref())
    }

    /// Beginning of time
    ///
    /// This metadata is emitted by newer versions of clang and can be used to
    /// sync up timings from multiple clang processes.
    ///
    pub fn beginning_of_time(&self) -> Option<Duration> {
        self.beginning_of_time
    }

    /// Access the entity parser and symbol demangling buffer
    pub(crate) fn parser_and_demangling_buf(&mut self) -> (&mut EntityParser, &mut String) {
        (&mut self.entities, &mut self.demangling_buf)
    }

    /// Access a file path using a PathKey
    pub(crate) fn file_path(&self, key: PathKey) -> InternedPath {
        self.entities.path(key)
    }

    /// Access a parsed C++ entity using an EntityKey
    pub(crate) fn entity(&self, key: EntityKey) -> EntityView {
        self.entities.entity(key)
    }

    /// Log how the entity parser was used so far
    ///
    /// This is internally used in EntityParser development to adjust tuning
    /// parameters like interner key sizes based on real world workloads. What
    /// will be logged by this function is unspecified and subjected to change
    /// without advance notice!
    ///
    pub fn log_interner_usage(&self) {
        use log::info;
        let parser = &self.entities;
        info!("EntityParser interner usage statistics:");
        info!("- Identifiers: {}", parser.num_identifiers());
        info!(
            "- Paths: {} interned components, {} total components, max {} components/path",
            parser.num_unique_path_components(),
            parser.num_path_components(),
            parser.max_path_len().unwrap_or(0)
        );
        info!("- Types: {}", parser.num_types());
        info!("- Values: {}", parser.num_values());
        info!(
            "- Template parameters: {} total parameters, max {} parameters/set",
            parser.num_template_parameters(),
            parser.max_template_parameter_set_len().unwrap_or(0)
        );
        info!(
            "- Value trailers: {} total AfterValue, max {} AfterValue/set",
            parser.num_after_value(),
            parser.max_value_trailer_len().unwrap_or(0)
        );
        info!(
            "- Function calls: {} total arguments, max {} arguments/set",
            parser.num_function_arguments(),
            parser.max_function_arguments_len().unwrap_or(0)
        );
        info!(
            "- Function parameters: {} total parameters, max {} parameters/set",
            parser.num_function_parameters(),
            parser.max_function_parameters_len().unwrap_or(0)
        );
        info!(
            "- Scopes: {} total Scopes, max {} Scopes/set",
            parser.num_scopes(),
            parser.max_scope_sequence_len().unwrap_or(0)
        );
        info!(
            "- Declarators: {} total DeclOperators, max {} DeclOperators/set",
            parser.num_decl_operators(),
            parser.max_declarator_len().unwrap_or(0)
        );
    }
}
//
impl FromStr for ClangTrace {
    type Err = ClangTraceParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Parse the string as CTF JSON data
        let profile_ctf = json::from_str::<TraceDataObject>(s)?;

        // Clang's -ftime-trace uses the Trace Data Object format but does not
        // leverage any of its standardized fields
        let profile_wo_events_and_extra = TraceDataObject {
            traceEvents: Box::default(),
            extra: HashMap::default(),
            ..profile_ctf
        };
        if profile_wo_events_and_extra != TraceDataObject::default() {
            return Err(ClangTraceParseError::UnexpectedTraceMetadata(
                profile_wo_events_and_extra,
            ));
        }

        // However, newer versions of clang provide some beginningOfTime
        // metadata that can be used to sync up time-traces originating from
        // multiple clang processes
        let beginning_of_time = if let Some(value) = profile_ctf.extra.get("beginningOfTime") {
            if let Some(f64_value) = value.as_f64() {
                Some(f64_value)
            } else {
                return Err(ClangTraceParseError::InvalidBeginningOfTime(value.clone()));
            }
        } else {
            None
        };

        // Process the trace events
        let mut activities = ActivityTreeBuilder::with_capacity(profile_ctf.traceEvents.len() - 1);
        let mut global_stats = HashMap::new();
        let mut process_name = None;
        let mut thread_name = None;
        let mut clang_pid = None;
        let merge_pid =
            |curr_pid: &mut Option<Pid>, proposed_pid: Pid| match (*curr_pid, proposed_pid) {
                (None, _) => {
                    *curr_pid = Some(proposed_pid);
                    Ok(())
                }
                (Some(pid1), pid2) if pid1 == pid2 => Ok(()),
                (Some(pid1), pid2) => Err(ClangTraceParseError::InconsistentPid(pid1, pid2)),
            };
        //
        for event in profile_ctf.traceEvents.into_vec() {
            match event {
                // Durations associated with a timestamp greater than 1µs are activity profiles
                TraceEvent::X {
                    ref duration_event, ..
                } if duration_event.ts > 1.0 => {
                    // Parse activity statistics and insert the new activity
                    // into the activity tree
                    merge_pid(&mut clang_pid, duration_event.pid)?;
                    activities.insert(ActivityStat::parse(event)?)?;
                }

                // Durations associated with a lower timestamp (typically 100ns) are global stats
                TraceEvent::X {
                    ref duration_event, ..
                } => {
                    merge_pid(&mut clang_pid, duration_event.pid)?;
                    let (name, stat) = GlobalStat::parse(event)?;
                    if let Some(old) = global_stats.insert(name.clone(), stat) {
                        let new = global_stats[&name].clone();
                        return Err(ClangTraceParseError::DuplicateGlobalStat(name, old, new));
                    }
                }

                // Metadata
                TraceEvent::M(m) => match m {
                    // Name of the clang process
                    MetadataEvent::process_name { ref pid, .. } => {
                        merge_pid(&mut clang_pid, *pid)?;
                        let name = metadata::parse_process_name(m)?;
                        if let Some(process_name) = process_name {
                            return Err(ClangTraceParseError::DuplicateProcessName(
                                process_name,
                                name,
                            ));
                        } else {
                            process_name = Some(name);
                        }
                    }

                    // Name of the clang thread
                    MetadataEvent::thread_name { ref pid, .. } => {
                        if let Some(pid) = pid {
                            merge_pid(&mut clang_pid, *pid)?;
                        }
                        let name = metadata::parse_thread_name(m)?;
                        if let Some(thread_name) = thread_name {
                            return Err(ClangTraceParseError::DuplicateThreadName(
                                thread_name,
                                name,
                            ));
                        } else {
                            thread_name = Some(name);
                        }
                    }

                    // No other metadata is expected from -ftime-trace
                    _ => return Err(ClangTraceParseError::UnexpectedMetadataEvent(m)),
                },

                // No other CTF record is expected from -ftime-trace
                _ => return Err(ClangTraceParseError::UnexpectedEvent(event)),
            }
        }

        // Ignore blatantly wrong PIDs reported by older clang
        let pid = clang_pid.filter(|&pid| pid > 1);

        // Build the final ClangTrace
        if let Some(process_name) = process_name {
            Ok(Self {
                activities: activities.build(),
                entities: EntityParser::new(),
                demangling_buf: String::new(),
                global_stats,
                process_name,
                thread_name,
                pid,
                beginning_of_time,
            })
        } else {
            Err(ClangTraceParseError::NoProcessName)
        }
    }
}

/// What can go wrong while loading clang's -ftime-trace data from a file
#[derive(Error, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum ClangTraceLoadError {
    /// Failed to load data from the file
    #[error("failed to load time trace from file ({0})")]
    Io(#[from] io::Error),

    /// Failed to parse data from the file
    #[error("failed to parse time trace ({0})")]
    Parse(#[from] ClangTraceParseError),
}

/// What can go wrong while parsing clang's -ftime-trace data from a string
#[derive(Error, Debug)]
pub enum ClangTraceParseError {
    /// Failed to parse data as CTF-style JSON
    #[error("failed to parse data as CTF JSON ({0})")]
    CtfParseError(#[from] CtfParseError),

    /// Encountered unexpected trace-wide metadata
    #[error("encountered unexpected trace-wide metadata ({0:#?})")]
    UnexpectedTraceMetadata(TraceDataObject),

    /// Encountered invalid beginningOfTimeMetadata
    #[error("encountered invalid beginningOfTime metadata ({0:#?})")]
    InvalidBeginningOfTime(json::Value),

    /// Failed to parse per-activity statistics
    #[error("failed to parse per-activity statistics ({0})")]
    ActivityStatParseError(#[from] ActivityStatParseError),

    /// Failed to insert an activity into the activity tree
    #[error("failed to insert an activity into the activity tree ({0})")]
    ActivityTreeError(#[from] ActivityTreeError),

    /// Failed to parse global statistics
    #[error("failed to parse global statistics ({0})")]
    GlobalStatParseError(#[from] GlobalStatParseError),

    /// Encountered two occurences of the same global statistics
    #[error("encountered global statistic \"{0}\" twice ({1:?} then {2:?})")]
    DuplicateGlobalStat(Box<str>, GlobalStat, GlobalStat),

    /// Encountered two occurences of the process name
    #[error("encountered process name twice (\"{0}\" then \"{1}\")")]
    DuplicateProcessName(Box<str>, Box<str>),

    /// Did not find the clang process' name
    #[error("did not encounter process name")]
    NoProcessName,

    /// Failed to parse the clang thread' name
    #[error("failed to parse name ({0})")]
    NameParseError(#[from] NameParseError),

    /// Encountered two occurences of the process name
    #[error("encountered thread name twice (\"{0}\" then \"{1}\")")]
    DuplicateThreadName(Box<str>, Box<str>),

    /// The clang pid was reported twice with different valies
    #[error("inconsistent clang pid values ({0} then {1})")]
    InconsistentPid(Pid, Pid),

    /// Encountered an unexpected CTF metadata event
    #[error("encountered unexpected metadata event {0:#?}")]
    UnexpectedMetadataEvent(MetadataEvent),

    /// Encountered an unexpected CTF event
    #[error("encountered unexpected event {0:#?}")]
    UnexpectedEvent(TraceEvent),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ctf::{
            events::{
                duration::DurationEvent,
                metadata::{MetadataOptions, SortIndexArgs},
            },
            DisplayTimeUnit, EventCategories,
        },
        stats::activity::argument::ActivityArgumentType,
    };
    use assert_matches::assert_matches;

    #[test]
    fn good_trace_clang10() {
        // Build the trace
        let trace = ClangTrace::from_str(
            r#"{
    "traceEvents": [
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 1.3,
            "dur": 6787.7,
            "name": "Frontend"
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 6789.3,
            "dur": 5554.2,
            "name": "CodeGenPasses"
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 6789.1,
            "dur": 5554.5,
            "name": "Backend"
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 1.1,
            "dur": 12343.8,
            "name": "ExecuteCompiler"
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 1,
            "ts": 0,
            "dur": 12345,
            "name": "Total ExecuteCompiler",
            "args": {
                "count": 1,
                "avg ms": 12345
            }
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 2,
            "ts": 0,
            "dur": 6789,
            "name": "Total Frontend",
            "args": {
                "count": 1,
                "avg ms": 6789
            }
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 3,
            "ts": 0,
            "dur": 5555,
            "name": "Total Backend",
            "args": {
                "count": 1,
                "avg ms": 5555
            }
        },
        {
            "ph":"M",
            "pid": 1,
            "tid": 0,
            "ts": 0,
            "cat": "",
            "name": "process_name",
            "args": {
                "name": "clang-10.0.0"
            }
        }
    ]
}"#,
        )
        .expect("This is a known-good parse which should not fail");

        // Check global metadata
        assert_eq!(trace.process_name(), "clang-10.0.0");
        assert_eq!(
            trace.global_stats(),
            &maplit::hashmap! {
                "ExecuteCompiler".into() => GlobalStat::new(12345.0, 1),
                "Frontend".into() => GlobalStat::new(6789.0, 1),
                "Backend".into() => GlobalStat::new(5555.0, 1),
            }
        );
        assert_eq!(trace.pid(), None);
        assert_eq!(trace.beginning_of_time(), None);
        assert_eq!(trace.thread_name(), None);

        // Check flat activity list
        let expected_activities = [
            (ActivityId::Frontend, 1.3, 6787.7),
            (ActivityId::CodeGenPasses, 6789.3, 5554.2),
            (ActivityId::Backend, 6789.1, 5554.5),
            (ActivityId::ExecuteCompiler, 1.1, 12343.8),
        ];
        for (activity_trace, (expected_activity, expected_start, expected_duration)) in trace
            .all_activities()
            .zip(expected_activities.iter().cloned())
        {
            assert_eq!(activity_trace.activity().id(), &expected_activity);
            assert_eq!(activity_trace.start(), expected_start);
            assert_eq!(activity_trace.duration(), expected_duration);
            assert!(
                activity_trace.activity().raw_argument()
                    == &RawActivityArgument::new(ActivityArgumentType::Nothing, None)
            );
        }

        // Check root node list
        let mut root_iter = trace.root_activities();
        let (root_activity, root_start, root_duration) = expected_activities
            .last()
            .expect("Already checked there is >1 activity");
        assert_matches!(root_iter.next(), Some(root) => {
            assert_eq!(root.activity().id(), root_activity);
            assert_eq!(root.start(), *root_start);
            assert_eq!(root.duration(), *root_duration);
        });
        assert_eq!(root_iter.next(), None);
    }

    #[test]
    fn good_trace_clang14() {
        // Build the trace
        let trace = ClangTrace::from_str(
            r#"{
    "beginningOfTime": 42.0,
    "traceEvents": [
        {
            "ph": "X",
            "pid": 42,
            "tid": 42,
            "ts": 1.3,
            "dur": 6787.7,
            "name": "Frontend"
        },
        {
            "ph": "X",
            "pid": 42,
            "tid": 42,
            "ts": 6789.3,
            "dur": 5554.2,
            "name": "CodeGenPasses"
        },
        {
            "ph": "X",
            "pid": 42,
            "tid": 42,
            "ts": 6789.1,
            "dur": 5554.5,
            "name": "Backend"
        },
        {
            "ph": "X",
            "pid": 42,
            "tid": 42,
            "ts": 1.1,
            "dur": 12343.8,
            "name": "ExecuteCompiler"
        },
        {
            "ph": "X",
            "pid": 42,
            "tid": 43,
            "ts": 0,
            "dur": 12345,
            "name": "Total ExecuteCompiler",
            "args": {
                "count": 1,
                "avg ms": 12345
            }
        },
        {
            "ph": "X",
            "pid": 42,
            "tid": 44,
            "ts": 0,
            "dur": 6789,
            "name": "Total Frontend",
            "args": {
                "count": 1,
                "avg ms": 6789
            }
        },
        {
            "ph": "X",
            "pid": 42,
            "tid": 45,
            "ts": 0,
            "dur": 5555,
            "name": "Total Backend",
            "args": {
                "count": 1,
                "avg ms": 5555
            }
        },
        {
            "ph":"M",
            "pid": 42,
            "tid": 42,
            "ts": 0,
            "cat": "",
            "name": "process_name",
            "args": {
                "name": "clang-14.0.0"
            }
        },
        {
            "ph":"M",
            "pid": 42,
            "tid": 42,
            "ts": 0,
            "cat": "",
            "name": "thread_name",
            "args": {
                "name": "clang"
            }
        }
    ]
}"#,
        )
        .expect("This is a known-good parse which should not fail");

        // Check global metadata
        assert_eq!(trace.process_name(), "clang-14.0.0");
        assert_eq!(
            trace.global_stats(),
            &maplit::hashmap! {
                "ExecuteCompiler".into() => GlobalStat::new(12345.0, 1),
                "Frontend".into() => GlobalStat::new(6789.0, 1),
                "Backend".into() => GlobalStat::new(5555.0, 1),
            }
        );
        assert_eq!(trace.pid(), Some(42));
        assert_eq!(trace.beginning_of_time(), Some(42.0));
        assert_eq!(trace.thread_name(), Some("clang"));

        // Check flat activity list
        let expected_activities = [
            (ActivityId::Frontend, 1.3, 6787.7),
            (ActivityId::CodeGenPasses, 6789.3, 5554.2),
            (ActivityId::Backend, 6789.1, 5554.5),
            (ActivityId::ExecuteCompiler, 1.1, 12343.8),
        ];
        for (activity_trace, (expected_activity, expected_start, expected_duration)) in trace
            .all_activities()
            .zip(expected_activities.iter().cloned())
        {
            assert_eq!(activity_trace.activity().id(), &expected_activity);
            assert!(
                activity_trace.activity().raw_argument()
                    == &RawActivityArgument::new(ActivityArgumentType::Nothing, None)
            );
            assert_eq!(activity_trace.start(), expected_start);
            assert_eq!(activity_trace.duration(), expected_duration);
        }

        // Check root node list
        let mut root_iter = trace.root_activities();
        let (root_activity, root_start, root_duration) = expected_activities
            .last()
            .expect("Already checked there is >1 activity");
        assert_matches!(root_iter.next(), Some(root) => {
            assert_eq!(root.activity().id(), root_activity);
            assert_eq!(root.start(), *root_start);
            assert_eq!(root.duration(), *root_duration);
        });
        assert_eq!(root_iter.next(), None);
    }

    macro_rules! expect_err {
        ($e:expr) => {
            if let Err(error) = $e {
                error
            } else {
                unreachable!(
                    "{} is a known-bad parse that should error out",
                    stringify!($e)
                )
            }
        };
    }

    #[test]
    fn invalid_ctf_json() {
        // Missing traceEvents
        assert_matches!(
            expect_err!(ClangTrace::from_str("{}")),
            ClangTraceParseError::CtfParseError(_)
        );
    }

    #[test]
    fn invalid_beginning_of_time() {
        assert_matches!(
            // Expecting nothing but traceEvents
            expect_err!(ClangTrace::from_str(r#"{"traceEvents": [], "beginningOfTime": "lolnope"}"#)),
            ClangTraceParseError::InvalidBeginningOfTime(value) => {
                assert_eq!(value, serde_json::json!("lolnope"));
            }
        )
    }

    #[test]
    fn unexpected_metadata() {
        assert_matches!(
            // Expecting nothing but traceEvents
            expect_err!(ClangTrace::from_str(r#"{"traceEvents": [], "displayTimeUnit": "ns"}"#)),
            ClangTraceParseError::UnexpectedTraceMetadata(trace_data_object) => {
                assert_eq!(trace_data_object, TraceDataObject {
                    displayTimeUnit: DisplayTimeUnit::ns,
                    ..TraceDataObject::default()
                });
            }
        )
    }

    #[test]
    fn invalid_activity_stat() {
        assert_matches!(
            // Invalid name
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "X",
        "pid": 1,
        "tid": 0,
        "ts": 1.3,
        "dur": 6787.7,
        "name": "WhatIsThisThing"
    }]
}"#
            )),
            ClangTraceParseError::ActivityStatParseError(_)
        );
    }

    #[test]
    fn inconsistent_pid() {
        assert_matches!(
            // Invalid name
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [
        {
            "ph": "X",
            "pid": 42,
            "tid": 42,
            "ts": 1.3,
            "dur": 6787.7,
            "name": "Frontend"
        },
        {
            "ph": "X",
            "pid": 43,
            "tid": 43,
            "ts": 6789.3,
            "dur": 5554.2,
            "name": "CodeGenPasses"
        }
    ]
}"#
            )),
            ClangTraceParseError::InconsistentPid(42, 43)
        );
    }

    #[test]
    fn invalid_activity_tree() {
        assert_matches!(
            // Events not in increading end timestamp order
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 6789.1,
            "dur": 5554.5,
            "name": "Backend"
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 1.3,
            "dur": 6787.7,
            "name": "Frontend"
        }
    ]
}"#
            )),
            ClangTraceParseError::ActivityTreeError(_)
        );
    }

    #[test]
    fn invalid_global_stat() {
        assert_matches!(
            // Invalid start timestamp
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "X",
        "pid": 1,
        "tid": 1,
        "ts": 0.2,
        "dur": 12345,
        "name": "Total ExecuteCompiler",
        "args": {
            "count": 1,
            "avg ms": 12345
        }
    }]
}"#
            )),
            ClangTraceParseError::GlobalStatParseError(_)
        );
    }

    #[test]
    fn duplicate_global_stat() {
        assert_matches!(
            // Duplicate global stat
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [
        {
            "ph": "X",
            "pid": 1,
            "tid": 1,
            "ts": 0,
            "dur": 12345,
            "name": "Total ExecuteCompiler",
            "args": {
                "count": 1,
                "avg ms": 12345
            }
        },
        {
            "ph": "X",
            "pid": 1,
            "tid": 1,
            "ts": 0,
            "dur": 54321,
            "name": "Total ExecuteCompiler",
            "args": {
                "count": 1,
                "avg ms": 54321
            }
        }
    ]
}"#
            )),
            ClangTraceParseError::DuplicateGlobalStat(
                name,
                old,
                new
            ) => {
                assert_eq!(&*name, "ExecuteCompiler");
                assert_eq!(old, GlobalStat::new(12345.0, 1));
                assert_eq!(new, GlobalStat::new(54321.0, 1));
            }
        );
    }

    #[test]
    fn invalid_process_name() {
        assert_matches!(
            // Bad start timestamp
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph":"M",
        "pid": 1,
        "tid": 0,
        "ts": 0.42,
        "cat": "",
        "name": "process_name",
        "args": {
            "name": "clang-14.0.0"
        }
    }]
}"#
            )),
            ClangTraceParseError::NameParseError(_)
        );
    }

    #[test]
    fn duplicate_process_name() {
        assert_matches!(
            // Multiple process names
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [
        {
            "ph":"M",
            "pid": 1,
            "tid": 0,
            "ts": 0,
            "cat": "",
            "name": "process_name",
            "args": {
                "name": "clang-14.0.0"
            }
        },
        {
            "ph":"M",
            "pid": 1,
            "tid": 0,
            "ts": 0,
            "cat": "",
            "name": "process_name",
            "args": {
                "name": "clang-13.9.9"
            }
        }
    ]
}"#
            )),
            ClangTraceParseError::DuplicateProcessName(old, new) => {
                assert_eq!(&*old, "clang-14.0.0");
                assert_eq!(&*new, "clang-13.9.9");
            }
        );
    }

    #[test]
    fn no_process_name() {
        assert_matches!(
            // No process name in an otherwise correct stream
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "X",
        "pid": 1,
        "tid": 0,
        "ts": 1.1,
        "dur": 12343.8,
        "name": "ExecuteCompiler"
    }]
}"#
            )),
            ClangTraceParseError::NoProcessName
        );
    }

    #[test]
    fn duplicate_thread_name() {
        assert_matches!(
            // Multiple process names
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [
        {
            "ph":"M",
            "pid": 42,
            "tid": 42,
            "ts": 0,
            "cat": "",
            "name": "thread_name",
            "args": {
                "name": "clang"
            }
        },
        {
            "ph":"M",
            "pid": 42,
            "tid": 42,
            "ts": 0,
            "cat": "",
            "name": "thread_name",
            "args": {
                "name": "cling"
            }
        }
    ]
}"#
            )),
            ClangTraceParseError::DuplicateThreadName(old, new) => {
                assert_eq!(&*old, "clang");
                assert_eq!(&*new, "cling");
            }
        );
    }

    #[test]
    fn unexpected_event() {
        assert_matches!(
            // clang should not emit Begin/End events
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "B",
        "pid": 1,
        "tid": 0,
        "ts": 0.1,
        "name": "ExecuteCompiler"
    }]
}"#
            )),
            ClangTraceParseError::UnexpectedEvent(e) => {
                assert_eq!(e, TraceEvent::B(DurationEvent {
                    pid: 1,
                    tid: 0,
                    ts: 0.1,
                    name: Some("ExecuteCompiler".into()),
                    ..DurationEvent::default()
                }));
            }
        );
    }

    #[test]
    fn unexpected_metadata_event() {
        assert_matches!(
            // clang should not emit process_sort_index events
            expect_err!(ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph":"M",
        "pid": 42,
        "tid": 42,
        "ts": 0,
        "cat": "",
        "name": "process_sort_index",
        "args": {
            "sort_order": 123
        }
    }]
}"#
            )),
            ClangTraceParseError::UnexpectedMetadataEvent(e) => {
                assert_eq!(e, MetadataEvent::process_sort_index {
                    pid: 42,
                    tid: Some(42),
                    args: SortIndexArgs {
                        sort_order: 123,
                        ..Default::default()
                    },
                    options: MetadataOptions {
                        cat: Some(EventCategories(Box::default())),
                        ts: Some(0.0),
                        tts: None,
                    }
                });
            }
        );
    }
}
