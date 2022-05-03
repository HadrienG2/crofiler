//! Easier C++ build profiling

#![deny(missing_docs)]

use clang_time_trace::{ClangTrace, Duration};

fn main() {
    let trace =
        ClangTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();

    println!("Profile from {}", trace.process_name());

    println!("\nGlobal statistics: {:#?}", trace.global_stats());

    // Flat profile prototype
    // TODO: Make it easy to get a flat profile by any criterion
    let mut activities = trace.all_activities().collect::<Box<[_]>>();
    //
    const SELF_CUTOFF: Duration = 1.0;
    println!("\nSelf-time flat profile with {SELF_CUTOFF}% cutoff:");
    activities.sort_unstable_by(|a1, a2| {
        a1.self_duration()
            .partial_cmp(&a2.self_duration())
            .unwrap()
            .reverse()
    });
    let root_duration = trace
        .root_activities()
        .map(|root| root.duration())
        .sum::<f64>();
    for activity in activities.iter() {
        let self_duration = activity.self_duration();
        let percent = self_duration / root_duration * 100.0;
        if percent < SELF_CUTOFF {
            break;
        }
        println!(
            "- {:?} ({} µs, {:.2}%)",
            activity.activity(),
            self_duration,
            percent
        );
    }
    //
    const CHILD_CUTOFF: Duration = 1.0;
    println!("\nDirect children flat profile with {CHILD_CUTOFF}% cutoff:");
    activities.sort_unstable_by(|a1, a2| {
        a1.direct_children()
            .count()
            .cmp(&a2.direct_children().count())
            .reverse()
    });
    let num_activities = trace.all_activities().count();
    for activity in activities.iter() {
        let num_children = activity.direct_children().count();
        let percent = (num_children as f64) / (num_activities as f64) * 100.0;
        if percent < CHILD_CUTOFF {
            break;
        }
        println!(
            "- {:?} ({} children, {:.2}%)",
            activity.activity(),
            num_children,
            percent
        );
    }

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }
}
