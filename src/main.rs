//! Easier C++ build profiling

#![deny(missing_docs)]

use clang_time_trace::{Activity, ClangTrace, Duration};
use lasso::{MiniSpur, Rodeo};
use std::{
    collections::HashMap,
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

    // Display path arguments received by activities
    println!("\nActivity path arguments:");
    //
    let mut path_components = Rodeo::<MiniSpur>::new(); // TODO: Put this MiniSpur in a typedef
    let mut path_elements = Vec::new();
    let mut path_ranges = Vec::new();
    for activity_trace in trace.all_activities() {
        match activity_trace.activity() {
            Activity::Source(path) | Activity::OptModule(path) => {
                // Perform basic path normalization, assuming no symlinks
                let path = Path::new(&**path);
                assert!(path.is_absolute());
                let path_start = path_elements.len();
                for component in path.components() {
                    use std::path::Component::*;
                    match component {
                        Prefix(_) | RootDir | Normal(_) => {
                            let component_str = component.as_os_str().to_str().expect(
                                "Since this path comes from JSON, it should be valid Unicode",
                            );
                            path_elements.push(path_components.get_or_intern(component_str))
                        }
                        CurDir => {}
                        ParentDir => {
                            assert!(path_elements.len() > path_start);
                            path_elements.pop();
                        }
                    }
                }
                path_ranges.push(path_start..path_elements.len());
            }
            _ => {}
        }
    }
    //
    let path_components = path_components.into_resolver();
    let mut path_buf = PathBuf::new();
    for path_range in &path_ranges {
        for component in &path_elements[path_range.clone()] {
            path_buf.push(path_components.resolve(component));
        }
        println!("- {}", path_buf.display());
        path_buf.clear();
    }
    println!(
        "{} paths, {} interned path components",
        path_ranges.len(),
        path_components.len()
    );
}
