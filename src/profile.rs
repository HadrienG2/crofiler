//! Analysis used in the display of compilation profiles

use clang_time_trace::{ActivityTrace, ClangTrace, Duration};
use std::collections::HashMap;

/// Given a set of root nodes, compute the associated duration norm used to go
/// from absolute child durations to percentages of the root durations
pub fn duration_norm<'a>(roots: impl Iterator<Item = ActivityTrace<'a>>) -> Duration {
    let root_duration = roots.map(|root| root.duration()).sum::<f64>();
    1.0 / root_duration
}

/// Breakdown of self-duration by activity type, ordered by decreasing duration
pub fn activity_type_breakdown(trace: &ClangTrace) -> Box<[(&str, Duration)]> {
    let mut profile = HashMap::<_, Duration>::new();
    for activity_trace in trace.all_activities() {
        *profile.entry(activity_trace.activity().name()).or_default() +=
            activity_trace.self_duration();
    }
    let mut profile = profile.into_iter().collect::<Box<[_]>>();
    profile.sort_unstable_by(|(_, d1), (_, d2)| d2.partial_cmp(d1).unwrap());
    profile
}

/// Extract the hottest activities from an activity iterator
///
/// - `duration` is the sorting criterion (can be duration(), self_duration(),
///   or a normalized version thereof for percentages)
/// - `threshold` is the duration threshold below which activities are dropped
///
pub fn hottest_activities<'activities>(
    activities: impl Iterator<Item = ActivityTrace<'activities>>,
    mut duration: impl FnMut(&ActivityTrace) -> Duration,
    threshold: Duration,
) -> Box<[ActivityTrace<'activities>]> {
    let mut children = activities
        .filter(|a| duration(a) >= threshold)
        .collect::<Box<[_]>>();
    children.sort_unstable_by(|a1, a2| duration(a2).partial_cmp(&duration(a1)).unwrap());
    children
}
