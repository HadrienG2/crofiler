//! Caller<->callee tree of activities which clang engaged in

use super::{
    ctf::{Duration, Timestamp},
    stats::activity::{Activity, ActivityStat},
};
use std::{
    fmt::{self, Debug, Formatter},
    num::NonZeroUsize,
    ops::Range,
    slice::SliceIndex,
};
use thiserror::Error;

/// Tree of activities which clang engaged in
#[derive(Debug, PartialEq)]
pub struct ActivityTree {
    /// Clang activities recorded by -ftime-trace
    activities: Box<[ActivityNode]>,

    /// Activities which were spawned by other activities + root activities
    ///
    /// For each activity, we get a slice of indices inside of this array
    /// corresponding to other activities (in the main activities array) that
    /// were triggered by doing it.
    ///
    /// At the end, there is also a slice of root activity indices which were
    /// directly triggered by toplevel clang logic.
    children: Box<[usize]>,

    /// Start of the list of root activities, at the end of the activity_tree
    first_root_idx: usize,
}
//
impl ActivityTree {
    /// Activities that were directly spawned by the clang driver
    ///
    /// From this, you can recursively iterate over child tasks in order to
    /// construct a hierarchical execution profile.
    pub fn root_activities(&self) -> impl Iterator<Item = ActivityTrace> {
        self.hierarchy_iter(self.first_root_idx..)
    }

    /// Iterator over a set of nodes identified by consecutive indices in
    /// ActivityTree::children.
    ///
    /// Usable both for tree roots and children of a tree node.
    fn hierarchy_iter<'self_>(
        &'self_ self,
        children_indices: impl SliceIndex<[usize], Output = [usize]>,
    ) -> impl Iterator<Item = ActivityTrace> + 'self_ {
        self.children[children_indices]
            .iter()
            .map(move |&activity_idx| ActivityTrace {
                tree: self,
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
                tree: self,
                activity,
                activity_idx,
            })
    }
}

/// Hierarchical view of an activity which clang engaged in
#[derive(PartialEq)]
pub struct ActivityTrace<'a> {
    /// Tree which this activity belongs to
    tree: &'a ActivityTree,

    /// Activity which we are looking at
    activity: &'a ActivityNode,

    /// Index of this activity in the ActivityTree::activities array
    activity_idx: usize,
}
//
impl ActivityTrace<'_> {
    /// What clang was doing
    pub fn activity(&self) -> &Activity {
        self.activity.stat.activity()
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
        self.tree
            .hierarchy_iter(self.activity.children_indices.clone())
    }

    /// Activities that were spawned while processing this activity, either
    /// directly or as an indirect result of directly spawned activities
    ///
    /// Like `ClangTrace::all_activities()`, but for children of one activity
    pub fn all_children(&self) -> impl Iterator<Item = ActivityTrace> {
        let first_child_idx = self.activity.first_related_idx;
        self.tree.activities[first_child_idx..self.activity_idx]
            .iter()
            .enumerate()
            .map(move |(rel_child_idx, activity)| {
                let activity_idx = rel_child_idx + first_child_idx;
                ActivityTrace {
                    tree: self.tree,
                    activity,
                    activity_idx,
                }
            })
    }

    /// Activity as part of which this activity was spawned, if any
    pub fn parent(&self) -> Option<ActivityTrace> {
        self.activity.parent_idx.map(|idx| {
            let idx = usize::from(idx);
            ActivityTrace {
                tree: self.tree,
                activity: &self.tree.activities[idx],
                activity_idx: idx,
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

/// Individual clang activity within the activity tree
#[derive(Debug, PartialEq)]
struct ActivityNode {
    /// Activity nature and associated time span
    stat: ActivityStat,

    /// Activity duration excluding children activities
    self_duration: Duration,

    /// Index of the first activity in ActivityTree::activities which belongs to
    /// the set composed of this event and all of its transitive children
    first_related_idx: usize,

    /// Indices of child activities in the global ActivityTree::children array
    children_indices: Range<usize>,

    /// Index of the parent of this activity in ActivityTree::activities
    //
    // We can use NonZeroUsize here because activity #0 cannot be the parent of
    // any other activity as it has the smallest end-time and being the parent
    // of an activity requires ending after it.
    parent_idx: Option<NonZeroUsize>,
}

/// Mechanism to build an ActivityTree
#[derive(Debug, PartialEq)]
pub struct ActivityTreeBuilder {
    /// Activities collected so far
    activities: Vec<ActivityNode>,

    /// Children indices collected so far
    children: Vec<usize>,

    /// Final timestamp of the last collected activity
    last_end: Timestamp,
}
//
impl ActivityTreeBuilder {
    /// Prepare to build an ActivityTree, assuming the number of activities (or
    /// a reasonably close upper bound) is known in advance
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            activities: Vec::with_capacity(capacity),
            children: Vec::with_capacity(capacity),
            last_end: Timestamp::MIN,
        }
    }

    /// Record a new activity in the tree
    ///
    /// Note that the code currently expects activities to be provided in order
    /// of ascending end timestamp, as clang does (and is expected to continue
    /// doing undefinitely, as that's the simplest implementation for them).
    pub fn insert(&mut self, activity: ActivityStat) -> Result<(), ActivityTreeError> {
        // Check assumption that activities are provided in order of increasing
        // end timestamp (this means that a parent activity follows the sequence
        // of its transitive children, which eases tree building).
        let start = activity.start();
        let end = activity.end();
        if end < self.last_end {
            return Err(ActivityTreeError::UnexpectedActivityOrder {
                prev: self.last_end,
                current: end,
            });
        }
        self.last_end = end;

        // Collect the list of this activity's children
        let current_idx = self.activities.len();
        let first_child_idx = self.children.len();
        let mut first_related_idx = current_idx;
        let mut child_candidates = &mut self.activities[..];
        let mut children_duration = 0.0;
        //
        while let Some((candidate, next_candidates)) = child_candidates.split_last_mut() {
            // Abort once we find a candidate which starts and ends before we do:
            // that's not a child, and we know no further child will come before
            // that through the input ordering property asserted above.
            let candidate_idx = next_candidates.len();
            if candidate.stat.start() < start {
                if candidate.stat.end() <= start {
                    break;
                } else {
                    return Err(ActivityTreeError::PartialActivityOverlap {
                        prev: candidate.stat.clone(),
                        current: activity,
                    });
                }
            }

            // This is a child: mark ourselves as its parent, add its index to
            // our child list and accumulate its duration to ultimately allow
            // self-duration computation
            candidate.parent_idx = NonZeroUsize::new(current_idx);
            self.children.push(candidate_idx);
            children_duration += candidate.stat.duration();

            // Make sure that activities nest properly, as mandated by CTF
            if candidate.stat.end() > end {
                return Err(ActivityTreeError::PartialActivityOverlap {
                    prev: candidate.stat.clone(),
                    current: activity,
                });
            }

            // Ignore transitive children of this child for direct child lookup,
            // but add them to our set of transitive children.
            first_related_idx = candidate.first_related_idx;
            child_candidates = &mut next_candidates[..first_related_idx];
        }

        // Append this children list at the end of the tree and
        // reset the accumulator.
        let children_indices = first_child_idx..self.children.len();

        // Fill profile
        let self_duration = activity.duration() - children_duration;
        self.activities.push(ActivityNode {
            stat: activity,
            first_related_idx,
            children_indices,
            self_duration,
            parent_idx: None,
        });
        Ok(())
    }

    /// Finish building the activity tree
    pub fn build(mut self) -> ActivityTree {
        // Collect the list of tree roots
        let first_root_idx = self.children.len();
        let mut root_candidates = &self.activities[..];
        while let Some((root, next_candidates)) = root_candidates.split_last() {
            // This is a root, add its index to our root list.
            self.children.push(next_candidates.len());

            // Skip transitive children to find the previous root
            root_candidates = &next_candidates[..root.first_related_idx];
        }

        // After this, children should contain as many nodes as activities,
        // since each node is either a root or a child of another node
        assert_eq!(
            self.activities.len(),
            self.children.len(),
            "Failed to build a consistent tree"
        );

        // Build the final ActivityTree
        ActivityTree {
            activities: self.activities.into_boxed_slice(),
            children: self.children.into_boxed_slice(),
            first_root_idx,
        }
    }
}

/// What can go wrong while inserting activities into an ActivityTree
#[derive(Error, Debug)]
pub enum ActivityTreeError {
    /// Activities were not provided in increasing end timestamp order
    #[error("activities not provided in increasing end timestamp order (current end {current} < previous end {prev})")]
    UnexpectedActivityOrder { prev: Timestamp, current: Timestamp },

    /// Activities do not properly nest, as mandated by the CTF format
    #[error("activities do not nest ({prev:?} partially overlaps {current:?})")]
    PartialActivityOverlap {
        prev: ActivityStat,
        current: ActivityStat,
    },
}

// FIXME: Add some tests that exercise builder, accessors and all non-#[from] errors
