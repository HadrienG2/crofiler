//! Easier C++ build profiling

#![deny(missing_docs)]

mod trace;

use trace::{ClangTrace, Duration};

fn main() {
    let trace =
        ClangTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();

    println!("Profile from {}", trace.process_name());

    println!("\nGlobal statistics: {:#?}", trace.global_stats());

    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }

    const CUTOFF: Duration = 0.1;
    println!("\nFlat profile with {CUTOFF}% cutoff:");
    let mut activities = trace.all_activities().collect::<Box<[_]>>();
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
        if percent < CUTOFF {
            break;
        }
        println!(
            "- {:?} ({} µs, {:.2}%)",
            activity.activity(),
            activity.self_duration(),
            activity.self_duration() / root_duration * 100.0
        );
    }
}
