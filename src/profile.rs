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
    use clang_time_trace::{MICROSECOND, MILLISECOND, SECOND};

    fn assert_close(actual: Duration, reference: Duration) {
        assert!((actual - reference).abs() < 0.01 * reference.abs());
    }

    #[test]
    fn duration_norm() {
        TEST_TRACE.with(|trace| {
            let trace = trace.borrow();
            assert_close(
                super::duration_norm(trace.root_activities()),
                1.0 / (3.77 * SECOND),
            );
        });
    }

    #[test]
    fn activity_type_breakdown() {
        let expected = [
            ("Source", 911.15 * MILLISECOND),
            ("InstantiateClass", 711.40 * MILLISECOND),
            ("InstantiateFunction", 341.60 * MILLISECOND),
            ("ParseClass", 316.39 * MILLISECOND),
            ("PassManager<llvm::Function>", 231.79 * MILLISECOND),
            ("ModuleToPostOrderCGSCCPassAdaptor", 177.07 * MILLISECOND),
            ("RunPass", 171.33 * MILLISECOND),
            ("Frontend", 152.09 * MILLISECOND),
            ("OptFunction", 107.78 * MILLISECOND),
            ("DevirtSCCRepeatedPass", 85.93 * MILLISECOND),
            ("CodeGen Function", 78.33 * MILLISECOND),
            ("ParseTemplate", 76.88 * MILLISECOND),
            ("InlinerPass", 70.18 * MILLISECOND),
            ("ModuleToFunctionPassAdaptor", 70.02 * MILLISECOND),
            ("OptModule", 41.97 * MILLISECOND),
            ("PerformPendingInstantiations", 41.77 * MILLISECOND),
            ("InstCombinePass", 29.63 * MILLISECOND),
            ("FunctionToLoopPassAdaptor", 24.59 * MILLISECOND),
            ("ExecuteCompiler", 21.79 * MILLISECOND),
            ("JumpThreadingPass", 21.35 * MILLISECOND),
            ("IPSCCPPass", 9.83 * MILLISECOND),
            ("GlobalOptPass", 9.23 * MILLISECOND),
            ("GVNPass", 7.99 * MILLISECOND),
            ("MemCpyOptPass", 5.01 * MILLISECOND),
            ("CalledValuePropagationPass", 4.83 * MILLISECOND),
            ("DSEPass", 4.70 * MILLISECOND),
            ("Optimizer", 4.68 * MILLISECOND),
            ("EarlyCSEPass", 4.67 * MILLISECOND),
            ("SROAPass", 4.58 * MILLISECOND),
            ("CGSCCToFunctionPassAdaptor", 3.82 * MILLISECOND),
            ("DebugType", 3.31 * MILLISECOND),
            ("CGProfilePass", 2.83 * MILLISECOND),
            ("RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>", 2.11 * MILLISECOND),
            ("Backend", 1.80 * MILLISECOND),
            ("GlobalDCEPass", 1.68 * MILLISECOND),
            ("SLPVectorizerPass", 1.57 * MILLISECOND),
            ("CorrelatedValuePropagationPass", 1.37 * MILLISECOND),
            ("LoopUnrollPass", 1.37 * MILLISECOND),
            ("RunLoopPass", 1.18 * MILLISECOND),
            ("ADCEPass", 1.17 * MILLISECOND),
            ("PromotePass", 1.15 * MILLISECOND),
            ("ModuleInlinerWrapperPass", 1.08 * MILLISECOND),
            ("CodeGenPasses", 1.01 * MILLISECOND),
            ("PostOrderFunctionAttrsPass", 959.0 * MICROSECOND),
            ("BDCEPass", 751.0 * MICROSECOND),
            ("SCCPPass", 707.0 * MICROSECOND),
            ("ReversePostOrderFunctionAttrsPass", 648.0 * MICROSECOND),
            ("ReassociatePass", 618.0 * MICROSECOND),
            ("TailCallElimPass", 553.0 * MICROSECOND),
            ("DeadArgumentEliminationPass", 546.0 * MICROSECOND),
            ("PassManager<llvm::Loop, llvm::LoopAnalysisManager, llvm::LoopStandardAnalysisResults &, llvm::LPMUpdater &>", 531.0 * MICROSECOND),
        ];
        TEST_TRACE.with(|trace| {
            let trace = trace.borrow();
            let actual = super::activity_type_breakdown(&*&trace);
            for ((expected_name, expected_duration), (actual_name, actual_duration)) in
                expected.iter().zip(actual.iter())
            {
                assert_eq!(actual_name, expected_name);
                assert_close(*actual_duration, *expected_duration);
            }
        });
    }

    #[test]
    fn hottest_activities() {
        let expected = [
            ("ModuleToPostOrderCGSCCPassAdaptor", 177.07 * MILLISECOND),
            ("Frontend", 136.55 * MILLISECOND),
            ("OptModule", 41.97 * MILLISECOND),
            ("PerformPendingInstantiations", 41.77 * MILLISECOND),
            ("ModuleToFunctionPassAdaptor", 35.02 * MILLISECOND),
            ("ModuleToFunctionPassAdaptor", 25.64 * MILLISECOND),
            ("RunPass", 22.58 * MILLISECOND),
            ("ExecuteCompiler", 21.79 * MILLISECOND),
            ("Source", 21.65 * MILLISECOND),
            ("JumpThreadingPass", 20.21 * MILLISECOND),
            ("InlinerPass", 17.24 * MILLISECOND),
        ];
        TEST_TRACE.with(|trace| {
            let trace = trace.borrow();
            let actual = super::hottest_activities(
                trace.all_activities(),
                |activity| activity.self_duration(),
                17.20 * MILLISECOND,
            );
            for ((expected_name, expected_duration), actual_activity) in
                expected.iter().zip(actual.iter())
            {
                assert_eq!(actual_activity.activity().name(), *expected_name);
                assert_close(actual_activity.self_duration(), *expected_duration);
            }
        });
    }
}
