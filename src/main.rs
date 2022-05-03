//! Easier C++ build profiling

#![deny(missing_docs)]

use clang_time_trace::{Activity, ClangTrace, Duration};
use std::{
    collections::{BTreeMap, HashMap},
    path::{Path, PathBuf},
};

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

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }

    // Display path arguments received by activities in alphabetical order
    println!("\nActivity path arguments:");
    //
    let mut path_occurences = BTreeMap::<_, usize>::new();
    for activity_trace in trace.all_activities() {
        match activity_trace.activity() {
            Activity::Source(path) | Activity::OptModule(path) => {
                // Perform basic path normalization, assuming no symlinks
                let path = Path::new(&**path);
                assert!(path.is_absolute());
                let mut normalized_path = PathBuf::new();
                for component in path.components() {
                    use std::path::Component::*;
                    match component {
                        Prefix(_) | RootDir | Normal(_) => normalized_path.push(component),
                        CurDir => {}
                        ParentDir => assert!(normalized_path.pop()),
                    }
                }
                *path_occurences
                    .entry(normalized_path.into_boxed_path())
                    .or_default() += 1;
            }
            _ => {}
        }
    }
    //
    for (path, count) in path_occurences {
        print!("- {}", path.display());
        if count != 1 {
            print!(" ({count} occurences)");
        }
        println!();
    }
}
