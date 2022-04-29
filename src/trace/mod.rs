//! Ergonomic representation of the output from clang's -ftime-trace, with a
//! mechanism to load and parse it.

mod ctf;
mod metadata;
mod stats;

use self::{
    ctf::{events::duration::DurationEvent, Duration, Timestamp, TraceDataObject, TraceEvent},
    metadata::ProcessNameParseError,
    stats::{
        activity::{Activity, ActivityStat, ActivityStatParseError},
        global::{GlobalStat, GlobalStatParseError},
    },
};
use serde_json as json;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    fs::File,
    io::{self, Read},
    ops::Range,
    path::Path,
    slice::SliceIndex,
};
use thiserror::Error;

/// Simplified -ftime-trace profile from a clang execution
#[derive(Debug, PartialEq)]
pub struct ClangTrace {
    /// Clang activities recorded by -ftime-trace
    activities: Box<[ActivityData]>,

    /// Parent/child relationship of activities
    ///
    /// For each activity, we get a slice of indices inside of this array
    /// corresponding to other activities (in the main activities array) that
    /// were triggered by doing it.
    ///
    /// At the end, there is also a slice of root activity indices which were
    /// directly triggered by toplevel clang logic.
    activity_tree: Box<[usize]>,

    /// Start of the list of root activities, at the end of the activity_tree
    first_root_idx: usize,

    /// Global statistics
    global_stats: HashMap<String, GlobalStat>,

    /// Name of the clang process
    process_name: String,
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

    /// Parce a string of clang -ftime-trace data
    // FIXME/WIP: Modularize and add proper error handling
    pub fn from_str(s: &str) -> Result<Self, ClangTraceParseError> {
        // Parse the string as CTF JSON data
        let profile_ctf = json::from_str::<TraceDataObject>(s)?;

        // Clang's -ftime-trace uses the Trace Data Object format but does not
        // leverage any of its extra fields
        let profile_wo_events = TraceDataObject {
            traceEvents: Box::default(),
            ..profile_ctf
        };
        if profile_wo_events != TraceDataObject::default() {
            return Err(ClangTraceParseError::UnexpectedGlobalMetadata(
                profile_wo_events,
            ));
        }

        // Process the trace events
        let mut process_name = None;
        let mut last_end = Timestamp::MIN;
        let mut activities: Vec<ActivityData> =
            Vec::with_capacity(profile_ctf.traceEvents.len() - 1);
        let mut activity_tree = Vec::with_capacity(profile_ctf.traceEvents.len() - 1);
        let mut global_stats = HashMap::new();
        let mut children_accumulator = Vec::new();
        //
        for event in profile_ctf.traceEvents.into_iter() {
            match event {
                // Durations associated with a zero tid are activity profiles
                t @ TraceEvent::X {
                    duration_event: DurationEvent { tid: 0, .. },
                    ..
                } => {
                    // Parse activity statistics
                    let activity = ActivityStat::parse(t)?;

                    // Check assumption that clang -ftime-trace activities are
                    // sorted in order of increasing end timestamp (this means
                    // that a parent activity follows the sequence of its
                    // transitive children, which simplifies tree building).
                    let start = activity.start();
                    let end = activity.end();
                    // FIXME: No assertion
                    assert!(end >= last_end, "Bad -ftime-trace logic guess");
                    last_end = end;

                    // Collect the list of this activity's children (if any)
                    let mut first_related_idx = activities.len();
                    let mut child_candidates = &activities[..];
                    let mut children_duration = 0.0;
                    children_accumulator.clear();
                    while let Some((candidate, next_candidates)) = child_candidates.split_last() {
                        // Abort once we find a candidate which starts before we
                        // do: that's not a child, and we know no further child
                        // will come before that by the above ordering property.
                        let candidate_idx = next_candidates.len();
                        if candidate.stat.start() < start {
                            debug_assert!(candidate.stat.end() <= start);
                            break;
                        }
                        debug_assert!(candidate.stat.end() <= end);

                        // This is a child, add its index to our child list and
                        // accumulate its duration for self-duration computation
                        children_accumulator.push(candidate_idx);
                        children_duration += candidate.stat.duration();

                        // Ignore transitive children of this child and add them to
                        // our own set of transitive children.
                        first_related_idx = candidate.first_related_idx;
                        child_candidates = &next_candidates[..first_related_idx];
                    }

                    // Append this children list at the end of the tree and
                    // reset the accumulator.
                    let first_child_index = activity_tree.len();
                    activity_tree.extend_from_slice(&children_accumulator[..]);
                    let children_indices = first_child_index..activity_tree.len();

                    // Fill profile
                    let self_duration = activity.duration() - children_duration;
                    activities.push(ActivityData {
                        stat: activity,
                        first_related_idx,
                        children_indices,
                        self_duration,
                    });
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
                _ => return Err(ClangTraceParseError::UnexpectedEvent(event.clone())),
            }
        }

        // Collect the list of tree roots
        let mut root_accumulator = children_accumulator;
        root_accumulator.clear();
        let mut root_candidates = &activities[..];
        while let Some((root, next_candidates)) = root_candidates.split_last() {
            // This is a root, add its index to our root list.
            root_accumulator.push(next_candidates.len());

            // Skip transitive children to find the previous root
            root_candidates = &next_candidates[..root.first_related_idx];
        }
        let first_root_idx = activity_tree.len();
        activity_tree.extend_from_slice(&root_accumulator[..]);

        // After this, activity_tree should contain as many nodes as activities,
        // since each node is either a root or a child of another node
        // FIXME: No assertion
        assert_eq!(activities.len(), activity_tree.len());

        // Build the final ClangTrace
        if let Some(process_name) = process_name {
            Ok(Self {
                activities: activities.into_boxed_slice(),
                activity_tree: activity_tree.into_boxed_slice(),
                first_root_idx,
                global_stats,
                process_name,
            })
        } else {
            Err(ClangTraceParseError::NoProcessName)
        }
    }

    /// Activities that were directly spawned by the clang driver
    ///
    /// From this, you can recursively iterate over child tasks in order to
    /// construct a hierarchical execution profile.
    pub fn root_activities(&self) -> impl Iterator<Item = ActivityTrace> {
        self.hierarchy_iter(self.first_root_idx..)
    }

    /// Iterator over a set of nodes identified by consecutive indices in
    /// ClangTrace::activity_tree. Usable both for tree roots and children of
    /// a tree node.
    fn hierarchy_iter<'self_>(
        &'self_ self,
        tree_indices: impl SliceIndex<[usize], Output = [usize]>,
    ) -> impl Iterator<Item = ActivityTrace> + 'self_ {
        self.activity_tree[tree_indices]
            .iter()
            .map(move |&activity_idx| ActivityTrace {
                trace: self,
                activity: &self.activities[activity_idx],
                activity_idx,
            })
    }

    /// Complete list of activities that clang engaged in
    ///
    /// You can get a temporal trace of everything that happened by sorting this
    /// in ascending activity.start() order and you can get a flat time profile
    /// by sorting this in descending activity.self_duration() order. The order
    /// in which activities are emitted by this iterator is unspecified.
    ///
    /// When using such flat iteration, be careful not to double-count quantites
    /// that are aggregated over transitive children of each activity, including
    /// activity.duration() and anything derived from activity.all_children().
    pub fn all_activities(&self) -> impl Iterator<Item = ActivityTrace> {
        self.activities
            .iter()
            .enumerate()
            .map(|(activity_idx, activity)| ActivityTrace {
                trace: self,
                activity,
                activity_idx,
            })
    }

    /// Expose the global statistics
    pub fn global_stats(&self) -> &HashMap<String, GlobalStat> {
        &self.global_stats
    }

    /// Name of the clang process that acquired this data
    pub fn process_name(&self) -> &str {
        &self.process_name
    }
}

/// Things that can go wrong while loading clang's -ftime-trace data from a file
#[derive(Error, Debug)]
pub enum ClangTraceLoadError {
    #[error("failed to load time trace from file ({0})")]
    Io(#[from] io::Error),

    #[error("failed to parse time trace ({0})")]
    Parse(#[from] ClangTraceParseError),
}

/// Things that can go wrong while parsing clang's -ftime-trace data from a string
#[derive(Error, Debug)]
pub enum ClangTraceParseError {
    #[error("failed to parse data as CTF JSON ({0})")]
    CtfParseError(#[from] json::Error),

    #[error("unexpected global metadata ({0:#?})")]
    UnexpectedGlobalMetadata(TraceDataObject),

    #[error("failed to parse activity statistics ({0})")]
    ActivityStatParseError(#[from] ActivityStatParseError),

    #[error("failed to parse global statistics ({0})")]
    GlobalStatParseError(#[from] GlobalStatParseError),

    #[error("duplicate global statistic \"{0}\" ({1:?} then {2:?})")]
    DuplicateGlobalStat(String, GlobalStat, GlobalStat),

    #[error("failed to parse process name ({0})")]
    ProcessNameParseError(#[from] ProcessNameParseError),

    #[error("multiple process names (\"{0}\" then \"{1}\")")]
    DuplicateProcessName(String, String),

    #[error("missing process name metadata")]
    NoProcessName,

    #[error("encountered unexpected {0:#?}")]
    UnexpectedEvent(TraceEvent),
}

/// Hierarchical view over an activity which Clang engaged in
#[derive(PartialEq)]
pub struct ActivityTrace<'a> {
    /// Which clang profile this activity comes from
    trace: &'a ClangTrace,

    /// Which activity we are looking at
    activity: &'a ActivityData,

    /// What is the index of this activity in the ClangTrace::activities array
    activity_idx: usize,
}
//
impl ActivityTrace<'_> {
    /// What clang was doing
    pub fn activity(&self) -> &Activity {
        &self.activity.stat.activity()
    }

    /// When clang started doing this activity
    pub fn start(&self) -> Timestamp {
        self.activity.stat.start()
    }

    /// How much time was spent on this activity (including child tasks)
    pub fn duration(&self) -> Duration {
        self.activity.stat.duration()
    }

    /// How much time was spent excluding child tasks
    pub fn self_duration(&self) -> Duration {
        self.activity.self_duration
    }

    /// When clang stopped doing this activity
    pub fn end(&self) -> Timestamp {
        self.activity.stat.end()
    }

    /// Activities that were directly spawned by this activity
    ///
    /// Like `ClangTrace::root_activities()`, but for children of one activity
    pub fn direct_children(&self) -> impl Iterator<Item = ActivityTrace> {
        self.trace
            .hierarchy_iter(self.activity.children_indices.clone())
    }

    /// Activities that were spawned while processing this activity, either
    /// directly or as an indirect result of directly spawned activities
    ///
    /// Like `ClangTrace::all_activities()`, but for children of one activity
    pub fn all_children(&self) -> impl Iterator<Item = ActivityTrace> {
        let first_child_idx = self.activity.first_related_idx;
        self.trace.activities[first_child_idx..self.activity_idx]
            .iter()
            .enumerate()
            .map(move |(rel_child_idx, activity)| {
                let activity_idx = rel_child_idx + first_child_idx;
                ActivityTrace {
                    trace: self.trace,
                    activity,
                    activity_idx,
                }
            })
    }
}
//
impl Debug for ActivityTrace<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_struct("ActivityTrace")
            .field("activity", &self.activity)
            .field("activity_idx", &self.activity_idx)
            // Elide ClangTrace from output as that's huge
            .finish_non_exhaustive()
    }
}

/// Individual clang activity with -ftime-trace profiling information
#[derive(Debug, PartialEq)]
struct ActivityData {
    /// What activity are talking about and when was it running
    stat: ActivityStat,

    /// Activity duration excluding children activities
    self_duration: Duration,

    /// Index of the first event which, within ClangTrace::activities, belongs
    /// to the set composed of this event and all of its transitive children.
    first_related_idx: usize,

    /// Indices of the child activities in the global ClangTrace::tree array
    children_indices: Range<usize>,
}

// FIXME: Add some tests that exercise accessors and all non-#[from] errors
