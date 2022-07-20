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
    profile.sort_unstable_by(|(_, d1), (_, d2)| {
        d2.partial_cmp(d1).expect("No NaNs expected in time-trace")
    });
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
    children.sort_unstable_by(|a1, a2| {
        duration(a2)
            .partial_cmp(&duration(a1))
            .expect("No NaNs expected in time-trace")
    });
    children
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TEST_TRACE;

    const MILLISECONDS: Duration = 1000.0;
    const MICROSECONDS: Duration = 1.0;

    fn assert_close(actual: Duration, reference: Duration) {
        assert!((actual - reference).abs() < 0.01 * reference.abs());
    }

    #[test]
    fn duration_norm() {
        assert_close(
            super::duration_norm(TEST_TRACE.lock().unwrap().root_activities()),
            1.0 / (3.77 * 1_000_000.0),
        );
    }

    #[test]
    fn activity_type_breakdown() {
        let expected = [
            ("Source", 911.15 * MILLISECONDS),
            ("InstantiateClass", 711.40 * MILLISECONDS),
            ("InstantiateFunction", 341.60 * MILLISECONDS),
            ("ParseClass", 316.39 * MILLISECONDS),
            ("PassManager<llvm::Function>", 231.79 * MILLISECONDS),
            ("ModuleToPostOrderCGSCCPassAdaptor", 177.07 * MILLISECONDS),
            ("RunPass", 171.33 * MILLISECONDS),
            ("Frontend", 152.09 * MILLISECONDS),
            ("OptFunction", 107.78 * MILLISECONDS),
            ("DevirtSCCRepeatedPass", 85.93 * MILLISECONDS),
            ("CodeGen Function", 78.33 * MILLISECONDS),
            ("ParseTemplate", 76.88 * MILLISECONDS),
            ("InlinerPass", 70.18 * MILLISECONDS),
            ("ModuleToFunctionPassAdaptor", 70.02 * MILLISECONDS),
            ("OptModule", 41.97 * MILLISECONDS),
            ("PerformPendingInstantiations", 41.77 * MILLISECONDS),
            ("InstCombinePass", 29.63 * MILLISECONDS),
            ("FunctionToLoopPassAdaptor", 24.59 * MILLISECONDS),
            ("ExecuteCompiler", 21.79 * MILLISECONDS),
            ("JumpThreadingPass", 21.35 * MILLISECONDS),
            ("IPSCCPPass", 9.83 * MILLISECONDS),
            ("GlobalOptPass", 9.23 * MILLISECONDS),
            ("GVNPass", 7.99 * MILLISECONDS),
            ("MemCpyOptPass", 5.01 * MILLISECONDS),
            ("CalledValuePropagationPass", 4.83 * MILLISECONDS),
            ("DSEPass", 4.70 * MILLISECONDS),
            ("Optimizer", 4.68 * MILLISECONDS),
            ("EarlyCSEPass", 4.67 * MILLISECONDS),
            ("SROAPass", 4.58 * MILLISECONDS),
            ("CGSCCToFunctionPassAdaptor", 3.82 * MILLISECONDS),
            ("DebugType", 3.31 * MILLISECONDS),
            ("CGProfilePass", 2.83 * MILLISECONDS),
            ("RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>", 2.11 * MILLISECONDS),
            ("Backend", 1.80 * MILLISECONDS),
            ("GlobalDCEPass", 1.68 * MILLISECONDS),
            ("SLPVectorizerPass", 1.57 * MILLISECONDS),
            ("CorrelatedValuePropagationPass", 1.37 * MILLISECONDS),
            ("LoopUnrollPass", 1.37 * MILLISECONDS),
            ("RunLoopPass", 1.18 * MILLISECONDS),
            ("ADCEPass", 1.17 * MILLISECONDS),
            ("PromotePass", 1.15 * MILLISECONDS),
            ("ModuleInlinerWrapperPass", 1.08 * MILLISECONDS),
            ("CodeGenPasses", 1.01 * MILLISECONDS),
            ("PostOrderFunctionAttrsPass", 959.0 * MICROSECONDS),
            ("BDCEPass", 751.0 * MICROSECONDS),
            ("SCCPPass", 707.0 * MICROSECONDS),
            ("ReversePostOrderFunctionAttrsPass", 648.0 * MICROSECONDS),
            ("ReassociatePass", 618.0 * MICROSECONDS),
            ("TailCallElimPass", 553.0 * MICROSECONDS),
            ("DeadArgumentEliminationPass", 546.0 * MICROSECONDS),
            ("PassManager<llvm::Loop, llvm::LoopAnalysisManager, llvm::LoopStandardAnalysisResults &, llvm::LPMUpdater &>", 531.0 * MICROSECONDS),
        ];
        let trace = TEST_TRACE.lock().unwrap();
        let actual = super::activity_type_breakdown(&*&trace);
        for ((expected_name, expected_duration), (actual_name, actual_duration)) in
            expected.iter().zip(actual.iter())
        {
            assert_eq!(actual_name, expected_name);
            assert_close(*actual_duration, *expected_duration);
        }
    }

    #[test]
    fn hottest_activities() {
        let expected = [
            ("ModuleToPostOrderCGSCCPassAdaptor", 177.07 * MILLISECONDS),
            ("Frontend", 136.55 * MILLISECONDS),
            ("OptModule", 41.97 * MILLISECONDS),
            ("PerformPendingInstantiations", 41.77 * MILLISECONDS),
            ("ModuleToFunctionPassAdaptor", 35.02 * MILLISECONDS),
            ("ModuleToFunctionPassAdaptor", 25.64 * MILLISECONDS),
            ("RunPass", 22.58 * MILLISECONDS),
            ("ExecuteCompiler", 21.79 * MILLISECONDS),
            ("Source", 21.65 * MILLISECONDS),
            ("JumpThreadingPass", 20.21 * MILLISECONDS),
            ("InlinerPass", 17.24 * MILLISECONDS),
        ];
        let trace = TEST_TRACE.lock().unwrap();
        let actual = super::hottest_activities(
            trace.all_activities(),
            |activity| activity.self_duration(),
            17.20 * MILLISECONDS,
        );
        for ((expected_name, expected_duration), actual_activity) in
            expected.iter().zip(actual.iter())
        {
            assert_eq!(actual_activity.activity().name(), *expected_name);
            assert_close(actual_activity.self_duration(), *expected_duration);
        }
    }
}
