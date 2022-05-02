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
#[derive(Debug, Default, PartialEq)]
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
#[derive(Error, Debug, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn test_tree(tree: ActivityTree) {
        // Test flat iterator
        for (flat_activity, (expected_idx, expected_activity)) in tree
            .all_activities()
            .zip(tree.activities.iter().enumerate())
        {
            assert_eq!(
                flat_activity,
                ActivityTrace {
                    tree: &tree,
                    activity: expected_activity,
                    activity_idx: expected_idx,
                }
            );
        }

        // Test hierarchical iterator + individual root nodes
        for (root_activity, &expected_idx) in tree
            .root_activities()
            .zip(&tree.children[tree.first_root_idx..])
        {
            assert_eq!(
                root_activity,
                ActivityTrace {
                    tree: &tree,
                    activity: &tree.activities[expected_idx],
                    activity_idx: expected_idx,
                }
            );
            test_node(root_activity, None);
        }
    }

    fn test_node(node: ActivityTrace, expected_parent: Option<NonZeroUsize>) {
        // Test basic activity properties
        assert_eq!(node.activity(), node.activity.stat.activity());
        assert!(node.start() <= node.end());
        assert_eq!(node.duration(), node.end() - node.start());
        assert!(node.self_duration() <= node.duration());

        // Test parent lookup
        let tree = &node.tree;
        assert_eq!(
            node.parent(),
            expected_parent.map(|idx| {
                let parent_idx = usize::from(idx);
                ActivityTrace {
                    tree,
                    activity: &tree.activities[parent_idx],
                    activity_idx: parent_idx,
                }
            })
        );

        // Test flat child iterator
        for (flat_child, expected_idx) in node
            .all_children()
            .zip(node.activity.first_related_idx..=node.activity_idx)
        {
            assert_eq!(
                flat_child,
                ActivityTrace {
                    tree,
                    activity: &tree.activities[expected_idx],
                    activity_idx: expected_idx,
                }
            );
        }

        // Test hierarchical child iterator + individual child nodes
        for (child_activity, &expected_idx) in node
            .direct_children()
            .zip(&tree.children[node.activity.children_indices.clone()])
        {
            assert_eq!(
                child_activity,
                ActivityTrace {
                    tree,
                    activity: &tree.activities[expected_idx],
                    activity_idx: expected_idx,
                }
            );
            test_node(child_activity, NonZeroUsize::new(node.activity_idx));
        }
    }

    #[test]
    fn build_empty_tree() {
        let tree = ActivityTreeBuilder::with_capacity(0).build();
        assert_eq!(tree, ActivityTree::default());
        test_tree(tree);
    }

    #[test]
    fn build_single_node_tree() {
        // Build the tree
        let mut builder = ActivityTreeBuilder::with_capacity(1);
        let activity_stat = ActivityStat::new(Activity::ExecuteCompiler, 0.1, 4.2);
        builder.insert(activity_stat.clone()).unwrap();
        let tree = builder.build();

        // Check expected content and accessors
        let self_duration = activity_stat.duration();
        let activity_node = ActivityNode {
            stat: activity_stat,
            self_duration,
            first_related_idx: 0,
            children_indices: Range { start: 0, end: 0 },
            parent_idx: None,
        };
        assert_eq!(
            tree,
            ActivityTree {
                activities: vec![activity_node].into_boxed_slice(),
                children: vec![0].into_boxed_slice(),
                first_root_idx: 0,
            }
        );
        test_tree(tree);
    }

    #[test]
    fn build_basic_tree() {
        // Build the tree
        let mut builder = ActivityTreeBuilder::with_capacity(5);
        let root = ActivityStat::new(Activity::ExecuteCompiler, 0.1, 4.2e6); // End time 4.2e6
        let child1 = ActivityStat::new(Activity::Frontend, 0.2, 3.2e6); // End time 3.2e6
        let subchild1 =
            ActivityStat::new(Activity::Source("/usr/include/boost.h".into()), 0.3, 3.0e6); // End time 3.0e6
        let child2 = ActivityStat::new(Activity::Backend, 3.5e6, 0.6e6); // End time 4.1e6
        let subchild2 = ActivityStat::new(
            Activity::OptModule("/home/hadrien/main.cpp".into()),
            3.6e6,
            0.4e6,
        ); // End time 4.0e6
        builder.insert(subchild1.clone()).unwrap();
        builder.insert(child1.clone()).unwrap();
        builder.insert(subchild2.clone()).unwrap();
        builder.insert(child2.clone()).unwrap();
        builder.insert(root.clone()).unwrap();
        let tree = builder.build();

        // Check expected content
        let mut self_duration = subchild1.duration();
        let subchild1 = ActivityNode {
            stat: subchild1,
            self_duration,
            first_related_idx: 0,
            children_indices: Range { start: 0, end: 0 },
            parent_idx: NonZeroUsize::new(1),
        };
        self_duration = child1.duration() - subchild1.stat.duration();
        let child1 = ActivityNode {
            stat: child1,
            self_duration,
            first_related_idx: 0,
            children_indices: Range { start: 0, end: 1 },
            parent_idx: NonZeroUsize::new(4),
        };
        self_duration = subchild2.duration();
        let subchild2 = ActivityNode {
            stat: subchild2,
            self_duration,
            first_related_idx: 2,
            children_indices: Range { start: 1, end: 1 },
            parent_idx: NonZeroUsize::new(3),
        };
        self_duration = child2.duration() - subchild2.stat.duration();
        let child2 = ActivityNode {
            stat: child2,
            self_duration,
            first_related_idx: 2,
            children_indices: Range { start: 1, end: 2 },
            parent_idx: NonZeroUsize::new(4),
        };
        self_duration = root.duration() - child1.stat.duration() - child2.stat.duration();
        let root = ActivityNode {
            stat: root,
            self_duration,
            first_related_idx: 0,
            children_indices: Range { start: 2, end: 4 },
            parent_idx: None,
        };
        assert_eq!(
            tree,
            ActivityTree {
                activities: vec![subchild1, child1, subchild2, child2, root].into_boxed_slice(),
                children: vec![0, 2, 3, 1, 4].into_boxed_slice(),
                first_root_idx: 4,
            }
        );
        test_tree(tree);
    }

    #[test]
    fn build_error_unordered_timestamps() {
        let mut builder = ActivityTreeBuilder::with_capacity(2);
        let activity1 = ActivityStat::new(Activity::ExecuteCompiler, 4.6, 6.4);
        builder.insert(activity1.clone()).unwrap();
        let activity2 = ActivityStat::new(Activity::ExecuteCompiler, 0.1, 4.2);
        assert_eq!(
            builder.insert(activity2.clone()),
            Err(ActivityTreeError::UnexpectedActivityOrder {
                prev: activity1.end(),
                current: activity2.end()
            })
        );
    }

    #[test]
    fn build_error_partial_overlap_before() {
        let mut builder = ActivityTreeBuilder::with_capacity(2);
        let activity1 = ActivityStat::new(Activity::ExecuteCompiler, 1.2, 3.4);
        builder.insert(activity1.clone()).unwrap();
        let activity2 = ActivityStat::new(Activity::ExecuteCompiler, 2.3, 5.6);
        assert_eq!(
            builder.insert(activity2.clone()),
            Err(ActivityTreeError::PartialActivityOverlap {
                prev: activity1.clone(),
                current: activity2.clone()
            })
        );
    }
}
