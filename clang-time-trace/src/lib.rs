//! Ergonomic representation of the output from clang's -ftime-trace, with a
//! mechanism to load and parse it.

#![deny(missing_docs)]

mod ctf;
mod metadata;
mod stats;
mod tree;

use self::{
    ctf::{events::duration::DurationEvent, TraceDataObject, TraceEvent},
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
    ctf::{Duration, Timestamp},
    metadata::ProcessNameParseError,
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
        // leverage any of its extra fields
        let profile_wo_events = TraceDataObject {
            traceEvents: Box::default(),
            ..profile_ctf
        };
        if profile_wo_events != TraceDataObject::default() {
            return Err(ClangTraceParseError::UnexpectedTraceMetadata(
                profile_wo_events,
            ));
        }

        // Process the trace events
        let mut activities = ActivityTreeBuilder::with_capacity(profile_ctf.traceEvents.len() - 1);
        let entities = EntityParser::new();
        let mut demangling_buf = String::new();
        let mut global_stats = HashMap::new();
        let mut process_name = None;
        //
        for event in profile_ctf.traceEvents.into_vec() {
            match event {
                // Durations associated with a zero tid are activity profiles
                t @ TraceEvent::X {
                    duration_event: DurationEvent { tid: 0, .. },
                    ..
                } => {
                    // Parse activity statistics and insert the new activity
                    // into the activity tree
                    activities.insert(ActivityStat::parse(t, &entities, &mut demangling_buf)?)?;
                }

                // Durations associated with a nonzero tid are global stats
                t @ TraceEvent::X { .. } => {
                    let (name, stat) = GlobalStat::parse(t)?;
                    if let Some(old) = global_stats.insert(name.clone(), stat) {
                        let new = global_stats[&name].clone();
                        return Err(ClangTraceParseError::DuplicateGlobalStat(name, old, new));
                    }
                }

                // Process name metadata
                TraceEvent::M(m) => {
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

                // No other CTF record is expected from -ftime-trace
                _ => return Err(ClangTraceParseError::UnexpectedEvent(event)),
            }
        }

        // Display entity parser usage statistics
        log_entity_parser_usage(&entities);

        // Build the final ClangTrace
        if let Some(process_name) = process_name {
            Ok(Self {
                activities: activities.build(),
                entities: entities.finalize(),
                global_stats,
                process_name,
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
        parser.max_path_len().unwrap()
    );
    debug!("- Types: {}", parser.num_types());
    debug!("- Values: {}", parser.num_values());
    debug!(
        "- Template parameters: {} total parameters, max {} parameters/set",
        parser.num_template_parameters(),
        parser.max_template_parameter_set_len().unwrap()
    );
    debug!(
        "- Value trailers: {} total AfterValue, max {} AfterValue/set",
        parser.num_after_value(),
        parser.max_value_trailer_len().unwrap()
    );
    debug!(
        "- Function calls: {} total arguments, max {} arguments/set",
        parser.num_function_arguments(),
        parser.max_function_arguments_len().unwrap()
    );
    debug!(
        "- Function parameters: {} total parameters, max {} parameters/set",
        parser.num_function_parameters(),
        parser.max_function_parameters_len().unwrap()
    );
    debug!(
        "- Scopes: {} total Scopes, max {} Scopes/set",
        parser.num_scopes(),
        parser.max_scope_sequence_len().unwrap()
    );
    debug!(
        "- Declarators: {} total DeclOperators, max {} DeclOperators/set",
        parser.num_decl_operators(),
        parser.max_declarator_len().unwrap()
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

    /// Failed to parse the clang process' name
    #[error("failed to parse process name ({0})")]
    ProcessNameParseError(#[from] ProcessNameParseError),

    /// Encountered two occurences of the process name
    #[error("encountered process name twice (\"{0}\" then \"{1}\")")]
    DuplicateProcessName(Box<str>, Box<str>),

    /// Did not find the clang process' name
    #[error("did not encounter process name")]
    NoProcessName,

    /// Encountered an unexpected CTF event
    #[error("encountered unexpected {0:#?}")]
    UnexpectedEvent(TraceEvent),
}

#[cfg(test)]
mod tests {
    use super::{ctf::DisplayTimeUnit, stats::activity::Activity, *};
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
        .unwrap();

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
        let (root_activity, root_start, root_duration) = expected_activities.last().unwrap();
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
            Err(ClangTraceParseError::ProcessNameParseError(_))
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
