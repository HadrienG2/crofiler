mod parser;

use crate::parser::{
    events::{
        duration::DurationEvent,
        metadata::{MetadataEvent, MetadataOptions, NameArgs},
    },
    DisplayTimeUnit, Duration, Timestamp, TraceDataObject, TraceEvent,
};
use serde_json as json;
use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    ops::{Bound, Range, RangeBounds},
    slice::SliceIndex,
};

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
    /// corresponding to other activities that were triggered by doing it.
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
    // TODO: Constructor from file, with clean error handling

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
            .map(|prof| (&prof.activity, prof.self_duration))
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
}

/// View over an activity and its children
#[derive(Debug)]
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
        &self.activity_data.activity
    }

    /// When this activity started
    pub fn start(&self) -> Timestamp {
        self.activity_data.start
    }

    /// How much time was spent on it
    pub fn duration(&self) -> Duration {
        self.activity_data.duration
    }

    /// ...excluding transitively spawned children activity
    pub fn self_duration(&self) -> Duration {
        self.activity_data.self_duration
    }

    /// ...only accounting for transitively spawned children activity
    pub fn children_duration(&self) -> Duration {
        self.activity_data.duration - self.activity_data.self_duration
    }

    /// When this activity ended
    pub fn end(&self) -> Timestamp {
        self.activity_data.start + self.activity_data.duration
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
                (&activity_data.activity, activity_data.self_duration)
            })
    }
}

/// Clang activity with -ftime-trace profiling information
#[derive(Debug, PartialEq)]
struct ActivityData {
    /// What activity we are talking about
    activity: Activity,

    /// When did it start
    start: Timestamp,

    /// How long did it last...
    duration: Duration,

    /// ...excluding children activities
    self_duration: Duration,

    /// Index of the first event which, within TimeTrace::activities, belongs
    /// to the set composed of this event and all of its transitive children.
    first_related_idx: usize,

    /// Indices of the child activities in the global TimeTrace::tree array
    children_indices: Range<usize>,
}

/// Clang activity without profiling information
#[derive(Debug, PartialEq)]
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

/// Global clang execution statistics for a certain kind of activity
///
/// The precise semantics are unknown: are we talking about top-level entities?
/// all entities? self time? children time?
///
#[derive(Debug, PartialEq)]
pub struct GlobalStat {
    /// Execution duration
    total_duration: Duration,

    /// Number of occurences of this event
    count: usize,
}
//
impl GlobalStat {
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

fn main() {
    const FILENAME: &str = "2020-05-25_CombinatorialKalmanFilterTests.cpp.json";
    let mut profile_str = String::new();
    File::open(FILENAME)
        .unwrap()
        .read_to_string(&mut profile_str)
        .unwrap();
    let profile_ctf = json::from_str::<TraceDataObject>(&profile_str).unwrap();

    // Clang's -ftime-trace uses the Trace Data Object format but does not
    // leverage any of its extra fields...
    assert_eq!(profile_ctf.displayTimeUnit, DisplayTimeUnit::ms);
    assert_eq!(profile_ctf.systemTraceEvents, None);
    assert_eq!(profile_ctf.powerTraceAsString, None);
    assert_eq!(profile_ctf.stackFrames, None);
    assert_eq!(profile_ctf.samples, None);
    assert_eq!(profile_ctf.controllerTraceDataKey, None);
    assert_eq!(profile_ctf.extra, HashMap::new());

    // Process the trace events
    let mut process_name = None;
    let mut last_end = Timestamp::MIN;
    let mut activities: Vec<ActivityData> = Vec::with_capacity(profile_ctf.traceEvents.len() - 1);
    let mut activity_tree = Vec::with_capacity(profile_ctf.traceEvents.len() - 1);
    let mut global_stats = HashMap::new();
    let mut children_accumulator = Vec::new();
    //
    'event_loop: for event in profile_ctf.traceEvents.into_iter() {
        match event {
            // Process name metadata with lots of defaulted fields
            TraceEvent::M(MetadataEvent::process_name {
                pid: 1,
                args: NameArgs { name, extra },
                tid: Some(0),
                options:
                    MetadataOptions {
                        cat: Some(cat),
                        ts: Some(ts),
                        tts: None,
                    },
            }) => {
                assert_eq!(extra, &HashMap::new(), "Unexpected extra arguments");
                assert_eq!(cat.0, vec![].into_boxed_slice(), "Unexpected categories");
                assert_eq!(ts, &0.0, "Unexpected timestamp");
                assert_eq!(process_name, None, "Expected only one process name");
                process_name = Some(name.clone());
            }

            // Duration event
            TraceEvent::X {
                duration_event:
                    DurationEvent {
                        pid: 1,
                        tid,
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
                // Events with a nonzero thread ID should be global stats
                if *tid != 0 {
                    // Events with a nonzero thread ID should be global stats
                    assert_eq!(*ts, 0.0, "Bad -ftime-trace logic guess");
                    assert!(name.starts_with("Total "), "Bad -ftime-trace logic guess");
                    let args = args.as_ref().expect("Global stats should have arguments");
                    let mut keys = args.keys().collect::<Vec<&String>>();
                    keys.sort();
                    assert_eq!(
                        keys,
                        vec![&"avg ms".to_owned(), &"count".to_owned()],
                        "Bad -ftime-trace logic guess"
                    );

                    // Keep track of it to allow further examination
                    let old = global_stats.insert(
                        name.clone(),
                        GlobalStat {
                            total_duration: *dur,
                            count: args["count"].as_u64().expect("Expected an integer count")
                                as usize,
                        },
                    );
                    assert_eq!(
                        old, None,
                        "Value {name} appeared twice with values {old:?} and {:?}",
                        global_stats[name]
                    );
                    continue 'event_loop;
                }

                // Other events with a zero thread ID should be activities
                // Among those, PerformPendingInstantiations is special by
                // virtue of taking no arguments, everyone else does.
                let activity = match &**name {
                    "PerformPendingInstantiations" => Activity::PerformPendingInstantiations,
                    "Frontend" => Activity::Frontend,
                    "PerFunctionPasses" => Activity::PerFunctionPasses,
                    "PerModulePasses" => Activity::PerModulePasses,
                    "CodeGenPasses" => Activity::CodeGenPasses,
                    "Backend" => Activity::Backend,
                    "ExecuteCompiler" => Activity::ExecuteCompiler,
                    _ => {
                        if let Some(args) = args {
                            let mut args_iter = args.iter();
                            let (k, v) = args_iter.next().expect("Expected an argument");
                            assert_eq!(k, "detail", "Unexpected argument {k}: {v}");
                            let v = if let json::Value::String(s) = v {
                                s
                            } else {
                                panic!("Detail argument should be a string, but got {v}")
                            };
                            assert_eq!(
                                args_iter.next(),
                                None,
                                "Unexpected extra arguments beyond \"detail\" in {args:?}"
                            );
                            match &**name {
                                "Source" => Activity::Source(v.clone()),
                                "ParseClass" => Activity::ParseClass(v.clone()),
                                "InstantiateClass" => Activity::InstantiateClass(v.clone()),
                                "InstantiateTemplate" => Activity::InstantiateTemplate(v.clone()),
                                "ParseTemplate" => Activity::ParseTemplate(v.clone()),
                                "InstantiateFunction" => Activity::InstantiateFunction(v.clone()),
                                "DebugType" => Activity::DebugType(v.clone()),
                                "DebugGlobalVariable" => Activity::DebugGlobalVariable(v.clone()),
                                "CodeGen Function" => Activity::CodeGenFunction(v.clone()),
                                "DebugFunction" => Activity::DebugFunction(v.clone()),
                                "RunPass" => Activity::RunPass(v.clone()),
                                "OptFunction" => Activity::OptFunction(v.clone()),
                                "RunLoopPass" => Activity::RunLoopPass(v.clone()),
                                "OptModule" => Activity::OptModule(v.clone()),
                                _ => panic!("Unexpected activity: {name} with parameter {v}"),
                            }
                        } else {
                            panic!("Unexpected activity: {name}")
                        }
                    }
                };

                // Check assumption that clang -ftime-trace activities are
                // sorted in order of increasing end timestamp (this means
                // that a parent activity follows the sequence of its
                // transitive children, which simplifies tree building).
                let end = ts + dur;
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
                    if candidate.start < *ts {
                        debug_assert!(candidate.start + candidate.duration <= *ts);
                        break;
                    }
                    debug_assert!(candidate.start + candidate.duration <= end);

                    // This is a child, add its index to our child list and
                    // accumulate its duration for self-duration computation
                    children_accumulator.push(candidate_idx);
                    children_duration += candidate.duration;

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
                activities.push(ActivityData {
                    activity,
                    start: *ts,
                    duration: *dur,
                    first_related_idx,
                    children_indices,
                    self_duration: *dur - children_duration,
                });
            }

            // No other CTF record is expected of -ftime-trace
            _ => panic!("Unexpected event {:#?}", event),
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
    // since each node is either a root or a child of another node.
    println!(
        "From {} trace events, collected {} activities in a tree",
        profile_ctf.traceEvents.len(),
        activities.len()
    );
    assert_eq!(activities.len(), activity_tree.len());

    // Build the final TimeTrace"
    let trace = TimeTrace {
        process_name: process_name.expect("No process name found"),
        activities: activities.into_boxed_slice(),
        activity_tree: activity_tree.into_boxed_slice(),
        first_root_idx,
        global_stats,
    };
}
