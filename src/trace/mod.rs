//! Ergonomic representation of the output from clang's -ftime-trace, with a
//! mechanism to load and parse it.

mod ctf;
mod stats;

use self::{
    ctf::{
        events::{
            duration::DurationEvent,
            metadata::{MetadataEvent, MetadataOptions, NameArgs},
        },
        Duration, Timestamp, TraceDataObject, TraceEvent,
    },
    stats::{
        activity::{Activity, ActivityStat, ActivityStatParseError},
        global::{GlobalStat, GlobalStatParseError},
    },
};
use serde_json as json;
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    ops::{Bound, Range, RangeBounds},
    path::Path,
    slice::SliceIndex,
};
use thiserror::Error;

/// Simplified -ftime-trace profile from a clang execution
#[derive(Debug, PartialEq)]
pub struct TimeTrace {
    /// Name of the clang process
    process_name: String,

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
    ///
    activity_tree: Box<[usize]>,

    /// Start of the list of root activities, at the end of the activity_tree
    first_root_idx: usize,

    /// Global statistics
    global_stats: HashMap<String, GlobalStat>,
}
//
impl TimeTrace {
    /// Load from clang -ftime-trace output in a file
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, TimeTraceLoadError> {
        // Load JSON data from the input file and parse it as CTF JSON
        let mut profile_str = String::new();
        File::open(path)?.read_to_string(&mut profile_str)?;
        Ok(Self::from_str(&profile_str)?)
    }

    /// Parce a string of clang -ftime-trace data
    // FIXME/WIP: Modularize and add proper error handling
    pub fn from_str(s: &str) -> Result<Self, TimeTraceParseError> {
        // Parse the string as CTF JSON data
        let profile_ctf = json::from_str::<TraceDataObject>(s)?;

        // Clang's -ftime-trace uses the Trace Data Object format but does not
        // leverage any of its extra fields
        let profile_wo_events = TraceDataObject {
            traceEvents: Box::default(),
            ..profile_ctf
        };
        if profile_wo_events != TraceDataObject::default() {
            return Err(TimeTraceParseError::UnexpectedGlobalMetadata(
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
                    let activity_stat = ActivityStat::parse(t)?;

                    // Check assumption that clang -ftime-trace activities are
                    // sorted in order of increasing end timestamp (this means
                    // that a parent activity follows the sequence of its
                    // transitive children, which simplifies tree building).
                    let start = activity_stat.start();
                    let end = activity_stat.end();
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
                        if candidate.activity_stat.start() < start {
                            debug_assert!(candidate.activity_stat.end() <= start);
                            break;
                        }
                        debug_assert!(candidate.activity_stat.end() <= end);

                        // This is a child, add its index to our child list and
                        // accumulate its duration for self-duration computation
                        children_accumulator.push(candidate_idx);
                        children_duration += candidate.activity_stat.duration();

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
                    let self_duration = activity_stat.duration() - children_duration;
                    activities.push(ActivityData {
                        activity_stat,
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
                        return Err(TimeTraceParseError::DuplicateGlobalStat(name, old, new));
                    }
                }

                // Process name metadata
                TraceEvent::M(m) => {
                    let name = Self::parse_process_name(m)?;
                    if let Some(process_name) = process_name {
                        return Err(TimeTraceParseError::DuplicateProcessName(
                            process_name,
                            name,
                        ));
                    } else {
                        process_name = Some(name);
                    }
                }

                // No other CTF record is expected from -ftime-trace
                _ => return Err(TimeTraceParseError::UnexpectedEvent(event.clone())),
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

        // Build the final TimeTrace
        if let Some(process_name) = process_name {
            Ok(Self {
                process_name,
                activities: activities.into_boxed_slice(),
                activity_tree: activity_tree.into_boxed_slice(),
                first_root_idx,
                global_stats,
            })
        } else {
            Err(TimeTraceParseError::NoProcessName)
        }
    }

    /// Decode the clang process name (which is currently the only metadata
    /// event that has been observed in -ftime-trace data)
    // TODO: Extract into a dedicated metadata module
    fn parse_process_name(m: &MetadataEvent) -> Result<String, TimeTraceParseError> {
        match m {
            MetadataEvent::process_name {
                pid: 1,
                args: NameArgs { name, extra },
                tid: Some(0),
                options:
                    MetadataOptions {
                        cat: Some(cat),
                        ts: Some(ts),
                        tts: None,
                    },
            } if extra.is_empty() && cat.0.is_empty() && *ts == 0.0 => Ok(name.clone()),
            _ => Err(TimeTraceParseError::UnexpectedMetadata(m.clone())),
        }
    }

    /// Name of the clang process that acquired this data
    pub fn process_name(&self) -> &str {
        &self.process_name
    }

    /// Iterate over all activities and their self time
    ///
    /// If you sort activites by decreasing self time, you get a flat profile.
    /// But we don't do it automatically because you may want to do some
    /// grouping beforehand (e.g. by activity type, by file, by namespace...)
    ///
    pub fn flat_iter(&self) -> impl Iterator<Item = (&Activity, Duration)> {
        self.activities
            .iter()
            .map(|prof| (prof.activity_stat.activity(), prof.self_duration))
    }

    /// Iterate over top-level activities, enabling iteration over their
    /// children as the caller desires
    pub fn hierarchy_iter(&self) -> impl Iterator<Item = ActivityProfile> {
        self.hierarchy_iter_impl(self.first_root_idx..)
    }

    /// Generalization of hierarchy_iter that works with any contiguous subset
    /// of the activity tree, which makes it usable for children of nodes in
    /// addition to tree roots.
    fn hierarchy_iter_impl<'_self>(
        &'_self self,
        node_set: impl RangeBounds<usize> + SliceIndex<[usize], Output = [usize]>,
    ) -> impl Iterator<Item = ActivityProfile> + '_self {
        let idx_shift = match node_set.start_bound() {
            Bound::Unbounded => 0,
            Bound::Included(&s) => s,
            Bound::Excluded(&s) => s + 1,
        };
        self.activity_tree[node_set]
            .iter()
            .enumerate()
            .map(move |(shifted_idx, &activity_idx)| {
                let tree_idx = shifted_idx + idx_shift;
                ActivityProfile {
                    top_profile: self,
                    activity_data: &self.activities[activity_idx],
                    tree_idx,
                }
            })
    }

    /// Expose the global statistics
    pub fn global_stats(&self) -> &HashMap<String, GlobalStat> {
        &self.global_stats
    }
}

/// Things that can go wrong while loading clang's -ftime-trace data from a file
#[derive(Error, Debug)]
pub enum TimeTraceLoadError {
    #[error("failed to load time trace from file ({0})")]
    Io(#[from] io::Error),

    #[error("failed to parse time trace ({0})")]
    Parse(#[from] TimeTraceParseError),
}

/// Things that can go wrong while parsing clang's -ftime-trace data from a string
#[derive(Error, Debug)]
pub enum TimeTraceParseError {
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

    #[error("unexpected {0:#?}")]
    UnexpectedMetadata(MetadataEvent),

    #[error("missing process name metadata")]
    NoProcessName,

    #[error("multiple process names (\"{0}\" then \"{1}\")")]
    DuplicateProcessName(String, String),

    #[error("unexpected {0:#?}")]
    UnexpectedEvent(TraceEvent),
}

/// View over an activity and its children
#[derive(Debug, PartialEq)]
pub struct ActivityProfile<'a> {
    /// Which profile this activity comes from
    top_profile: &'a TimeTrace,

    /// Which activity we are looking at
    activity_data: &'a ActivityData,

    /// What is the index of this activity in the TimeTrace::tree array
    tree_idx: usize,
}
//
impl ActivityProfile<'_> {
    /// What is going on
    pub fn activity(&self) -> &Activity {
        &self.activity_data.activity_stat.activity()
    }

    /// When this activity started
    pub fn start(&self) -> Timestamp {
        self.activity_data.activity_stat.start()
    }

    /// How much time was spent on it
    pub fn duration(&self) -> Duration {
        self.activity_data.activity_stat.duration()
    }

    /// ...excluding transitively spawned children activity
    pub fn self_duration(&self) -> Duration {
        self.activity_data.self_duration
    }

    /// ...only accounting for transitively spawned children activity
    pub fn children_duration(&self) -> Duration {
        self.duration() - self.activity_data.self_duration
    }

    /// When this activity ended
    pub fn end(&self) -> Timestamp {
        self.activity_data.activity_stat.end()
    }

    /// Iterate over children of this activity, enabling iteration over their
    /// children as the caller desires
    pub fn child_hierarchy_iter(&self) -> impl Iterator<Item = ActivityProfile> {
        self.top_profile
            .hierarchy_iter_impl(self.activity_data.children_indices.clone())
    }

    /// Iterate over all transitively spawned children and their self time
    ///
    /// If you sort activites by decreasing self time, you get a flat profile.
    /// But we don't do it automatically because you may want to do some
    /// grouping beforehand (e.g. by activity type, by file, by namespace...)
    ///
    pub fn child_flat_iter(&self) -> impl Iterator<Item = (&Activity, Duration)> {
        self.top_profile.activity_tree[self.activity_data.first_related_idx..=self.tree_idx]
            .iter()
            .map(|&child_idx| {
                let activity_data = &self.top_profile.activities[child_idx];
                (
                    activity_data.activity_stat.activity(),
                    activity_data.self_duration,
                )
            })
    }
}

/// Clang activity with -ftime-trace profiling information
#[derive(Debug, PartialEq)]
struct ActivityData {
    /// What activity are talking about and when was it running
    activity_stat: ActivityStat,

    /// Activity duration excluding children activities
    self_duration: Duration,

    /// Index of the first event which, within TimeTrace::activities, belongs
    /// to the set composed of this event and all of its transitive children.
    first_related_idx: usize,

    /// Indices of the child activities in the global TimeTrace::tree array
    children_indices: Range<usize>,
}

// FIXME: Add some tests
