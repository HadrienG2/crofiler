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
use cpparser::{Entities, EntityKey, EntityParser, EntityView};
use log::debug;
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
    ctf::{Duration, Pid, Timestamp},
    metadata::NameParseError,
    stats::{
        activity::{
            Activity, ActivityArgument, ActivityParseError, ActivityStatParseError, MangledSymbol,
        },
        global::{GlobalStat, GlobalStatParseError},
        ArgParseError,
    },
    tree::{ActivityId, ActivityTrace, ActivityTreeError},
};
pub use cpparser::{
    asylum::path::{InternedComponent, InternedPath, PathError},
    display::CustomDisplay,
    PathComponentKey, PathKey,
};
pub use json::Error as CtfParseError;

/// Simplified -ftime-trace profile from a clang execution
#[derive(Debug, PartialEq)]
pub struct ClangTrace {
    /// Clang activities recorded by -ftime-trace
    activities: ActivityTree,

    /// Interned C++ entities and file paths within activities
    entities: Entities,

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
    pub fn activity(&self, id: ActivityId) -> ActivityTrace {
        self.activities.activity(id)
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

    /// Access a file path using a PathKey
    pub fn file_path(&self, key: PathKey) -> InternedPath<PathComponentKey> {
        self.entities.path(key)
    }

    /// Access a parsed C++ entity using an EntityKey
    pub fn entity(&self, key: EntityKey) -> EntityView {
        self.entities.entity(key)
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
        let entities = EntityParser::new();
        let mut demangling_buf = String::new();
        let mut global_stats = HashMap::new();
        let mut process_name = None;
        let mut thread_name = None;
        let mut clang_pid = None;
        let merge_pid =
            |curr_pid: &mut Option<Pid>, proposed_pid: Pid| match (*curr_pid, proposed_pid) {
                (None, _) => Ok(*curr_pid = Some(proposed_pid)),
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
                    activities.insert(ActivityStat::parse(
                        event,
                        &entities,
                        &mut demangling_buf,
                    )?)?;
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

        // Display entity parser usage statistics
        log_entity_parser_usage(&entities);

        // Build the final ClangTrace
        if let Some(process_name) = process_name {
            Ok(Self {
                activities: activities.build(),
                entities: entities.finalize(),
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

/// Log how the entity parser was used, to ease interner key choices
fn log_entity_parser_usage(parser: &EntityParser) {
    debug!("EntityParser interner usage statistics:");
    debug!("- Identifiers: {}", parser.num_identifiers());
    debug!(
        "- Paths: {} interned components, {} total components, max {} components/path",
        parser.num_unique_path_components(),
        parser.num_path_components(),
        parser.max_path_len().unwrap_or(0)
    );
    debug!("- Types: {}", parser.num_types());
    debug!("- Values: {}", parser.num_values());
    debug!(
        "- Template parameters: {} total parameters, max {} parameters/set",
        parser.num_template_parameters(),
        parser.max_template_parameter_set_len().unwrap_or(0)
    );
    debug!(
        "- Value trailers: {} total AfterValue, max {} AfterValue/set",
        parser.num_after_value(),
        parser.max_value_trailer_len().unwrap_or(0)
    );
    debug!(
        "- Function calls: {} total arguments, max {} arguments/set",
        parser.num_function_arguments(),
        parser.max_function_arguments_len().unwrap_or(0)
    );
    debug!(
        "- Function parameters: {} total parameters, max {} parameters/set",
        parser.num_function_parameters(),
        parser.max_function_parameters_len().unwrap_or(0)
    );
    debug!(
        "- Scopes: {} total Scopes, max {} Scopes/set",
        parser.num_scopes(),
        parser.max_scope_sequence_len().unwrap_or(0)
    );
    debug!(
        "- Declarators: {} total DeclOperators, max {} DeclOperators/set",
        parser.num_decl_operators(),
        parser.max_declarator_len().unwrap_or(0)
    );
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
    use super::{
        ctf::{events::duration::DurationEvent, DisplayTimeUnit},
        stats::activity::Activity,
        *,
    };
    use assert_matches::assert_matches;

    #[test]
    fn good_trace() {
        // Build the trace
        let trace = ClangTrace::from_str(
            r#"{
    "traceEvents": [
        {
            "ph": "X",
            "pid": 1,
            "tid": 0,
            "ts": 0.3,
            "dur": 6788.7,
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
            "ts": 0.1,
            "dur": 12344.8,
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
                "name": "clang-14.0.0"
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

        // Check flat activity list
        let expected_activities = [
            (Activity::Frontend, 0.3, 6788.7),
            (Activity::CodeGenPasses, 6789.3, 5554.2),
            (Activity::Backend, 6789.1, 5554.5),
            (Activity::ExecuteCompiler, 0.1, 12344.8),
        ];
        for (trace, (expected_activity, expected_start, expected_duration)) in trace
            .all_activities()
            .zip(expected_activities.iter().cloned())
        {
            assert_eq!(trace.activity(), &expected_activity);
            assert_eq!(trace.start(), expected_start);
            assert_eq!(trace.duration(), expected_duration);
        }

        // Check root node list
        let mut root_iter = trace.root_activities();
        let (root_activity, root_start, root_duration) = expected_activities
            .last()
            .expect("Already checked there is >1 activity");
        assert_matches!(root_iter.next(), Some(root) => {
            assert_eq!(root.activity(), root_activity);
            assert_eq!(root.start(), *root_start);
            assert_eq!(root.duration(), *root_duration);
        });
        assert_eq!(root_iter.next(), None);
    }

    #[test]
    fn invalid_ctf_json() {
        assert_matches!(
            // Missing traceEvents
            ClangTrace::from_str("{}"),
            Err(ClangTraceParseError::CtfParseError(_))
        );
    }

    #[test]
    fn unexpected_metadata() {
        assert_matches!(
            // Expecting nothing but traceEvents
            ClangTrace::from_str(r#"{"traceEvents": [], "displayTimeUnit": "ns"}"#),
            Err(ClangTraceParseError::UnexpectedTraceMetadata(trace_data_object)) => {
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
            ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "X",
        "pid": 1,
        "tid": 0,
        "ts": 0.3,
        "dur": 6788.7,
        "name": "WhatIsThisThing"
    }]
}"#
            ),
            Err(ClangTraceParseError::ActivityStatParseError(_))
        );
    }

    #[test]
    fn invalid_activity_tree() {
        assert_matches!(
            // Events not in increading end timestamp order
            ClangTrace::from_str(
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
            "ts": 0.3,
            "dur": 6788.7,
            "name": "Frontend"
        }
    ]
}"#
            ),
            Err(ClangTraceParseError::ActivityTreeError(_))
        );
    }

    #[test]
    fn invalid_global_stat() {
        assert_matches!(
            // Invalid start timestamp
            ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "X",
        "pid": 1,
        "tid": 1,
        "ts": 4.2,
        "dur": 12345,
        "name": "Total ExecuteCompiler",
        "args": {
            "count": 1,
            "avg ms": 12345
        }
    }]
}"#
            ),
            Err(ClangTraceParseError::GlobalStatParseError(_))
        );
    }

    #[test]
    fn duplicate_global_stat() {
        assert_matches!(
            // Duplicate global stat
            ClangTrace::from_str(
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
            ),
            Err(ClangTraceParseError::DuplicateGlobalStat(
                name,
                old,
                new
            )) => {
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
            ClangTrace::from_str(
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
            ),
            Err(ClangTraceParseError::NameParseError(_))
        );
    }

    #[test]
    fn duplicate_process_name() {
        assert_matches!(
            // Multiple process names
            ClangTrace::from_str(
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
            ),
            Err(ClangTraceParseError::DuplicateProcessName(old, new)) => {
                assert_eq!(&*old, "clang-14.0.0");
                assert_eq!(&*new, "clang-13.9.9");
            }
        );
    }

    #[test]
    fn no_process_name() {
        assert_matches!(
            // No process name in an otherwise correct stream
            ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "X",
        "pid": 1,
        "tid": 0,
        "ts": 0.1,
        "dur": 12344.8,
        "name": "ExecuteCompiler"
    }]
}"#
            ),
            Err(ClangTraceParseError::NoProcessName)
        );
    }

    #[test]
    fn unexpected_event() {
        assert_matches!(
            // clang should not emit Begin/End events
            ClangTrace::from_str(
                r#"{
    "traceEvents": [{
        "ph": "B",
        "pid": 1,
        "tid": 0,
        "ts": 0.1,
        "name": "ExecuteCompiler"
    }]
}"#
            ),
            Err(ClangTraceParseError::UnexpectedEvent(e)) => {
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
}
