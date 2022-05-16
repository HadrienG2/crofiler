//! Easier C++ build profiling

#![deny(missing_docs)]

mod cpp;
mod path;

use clang_time_trace::{ActivityArgument, ClangTrace, Duration};
use std::collections::HashMap;

fn main() {
    let trace =
        ClangTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();

    println!("Profile from {}", trace.process_name());

    // Total clang execution time
    let root_duration = trace
        .root_activities()
        .map(|root| root.duration())
        .sum::<f64>();

    // Activity types by self-duration
    println!("\nSelf-duration breakdown by activity type:");
    //
    let mut profile = HashMap::<_, Duration>::new();
    for activity_trace in trace.all_activities() {
        *profile.entry(activity_trace.activity().name()).or_default() +=
            activity_trace.self_duration();
    }
    //
    let mut profile = profile.into_iter().collect::<Box<[_]>>();
    profile.sort_unstable_by(|(_, d1), (_, d2)| d2.partial_cmp(d1).unwrap());
    //
    for (name, duration) in profile.iter() {
        let percent = duration / root_duration * 100.0;
        println!("- {name} ({duration} µs, {percent:.2} %)");
    }

    // Flat activity profile by self-duration
    const FLAT_PROFILE_THRESHOLD: Duration = 0.01;
    println!("\nHot activities by self-duration:");
    //
    let norm = 1.0 / root_duration;
    let mut activities = trace
        .all_activities()
        .filter(|a| a.self_duration() * norm >= FLAT_PROFILE_THRESHOLD)
        .collect::<Box<[_]>>();
    //
    activities
        .sort_unstable_by(|a1, a2| a2.self_duration().partial_cmp(&a1.self_duration()).unwrap());
    //
    for activity_trace in activities.iter() {
        let activity = activity_trace.activity();
        let self_duration = activity_trace.self_duration();
        let percent = self_duration * norm * 100.0;
        println!("- {activity:?} ({self_duration} µs, {percent:.2} %)");
    }
    //
    let num_activities = trace.all_activities().count();
    if activities.len() < num_activities {
        let other_activities = num_activities - activities.len();
        println!(
            "- ... and {other_activities} other activities below {} % threshold ...",
            FLAT_PROFILE_THRESHOLD * 100.0
        );
    }

    // Print a list of file paths
    println!("\nFile paths:");
    let (width, _height) = termion::terminal_size().unwrap();
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::FilePath(p) = activity_trace.activity().argument() {
            println!(
                "- {}",
                path::truncate_path(&trace.file_path(&p), width.min(80))
            )
        }
    }

    // Print a list of C++ entities that the parser doesn't handle yet
    println!("\nExamples of incompletely or wrongly parsed C++ entities:");
    let mut bad_entities = 0;
    let mut num_entities = 0;
    const MAX_ERROR_DISPLAY: usize = 0;
    const MAX_ENTITY_DISPLAY: usize = 30;
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::CppEntity(e) = activity_trace.activity().argument() {
            num_entities += 1;
            match cpp::entity(&e) {
                Ok(("", _)) => {}
                other => {
                    if bad_entities < MAX_ENTITY_DISPLAY {
                        println!("- {}(\"{e}\")", activity_trace.activity().name());
                        if bad_entities < MAX_ERROR_DISPLAY {
                            println!("  -> {other:#?}");
                        }
                    }
                    bad_entities += 1;
                }
            }
        }
    }
    if bad_entities >= MAX_ENTITY_DISPLAY {
        println!(
            "- ... and more, for a total of {}/{} badly parsed entities ({:.2}% to go) ...",
            bad_entities - MAX_ENTITY_DISPLAY,
            num_entities,
            (bad_entities as f32) / (num_entities as f32) * 100.0
        );
    }

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }
}
