//! Easier C++ build profiling

#![deny(missing_docs)]

mod path;

use clang_time_trace::{ActivityArgument, ActivityTrace, ClangTrace, Duration, MangledSymbol};
use std::collections::HashMap;

fn main() {
    env_logger::init();

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
    let profile = |name, duration: Box<dyn Fn(&ActivityTrace) -> Duration>, threshold: Duration| {
        println!("\nHot activities by {name}:");
        //
        let norm = 1.0 / root_duration;
        let mut activities = trace
            .all_activities()
            .filter(|a| duration(a) * norm >= threshold)
            .collect::<Box<[_]>>();
        //
        activities.sort_unstable_by(|a1, a2| duration(a2).partial_cmp(&duration(a1)).unwrap());
        //
        for activity_trace in activities.iter() {
            let activity_name = activity_trace.activity().name();
            let activity_arg = activity_trace.activity().argument();
            let duration = duration(&activity_trace);
            let percent = duration * norm * 100.0;
            print!("- {activity_name}");
            match activity_arg {
                ActivityArgument::Nothing => {}
                ActivityArgument::String(s)
                | ActivityArgument::MangledSymbol(MangledSymbol::Demangled(s))
                | ActivityArgument::MangledSymbol(MangledSymbol::Mangled(s)) => print!("({s})"),
                ActivityArgument::FilePath(p) => print!("({})", trace.file_path(p)),
                ActivityArgument::CppEntity(e)
                | ActivityArgument::MangledSymbol(MangledSymbol::Parsed(e)) => {
                    print!("({})", trace.entity(e))
                }
            }
            println!(" ({duration} µs, {percent:.2} %)");
        }
        //
        let num_activities = trace.all_activities().count();
        if activities.len() < num_activities {
            let other_activities = num_activities - activities.len();
            println!(
                "- ... and {other_activities} other activities below {} % threshold ...",
                threshold * 100.0
            );
        }
    };
    profile("self-duration", Box::new(|a| a.self_duration()), 0.01);
    profile("total duration", Box::new(|a| a.duration()), 0.01);

    // Print a list of file paths
    /*println!("\nFile paths:");
    let (width, _height) = termion::terminal_size().unwrap();
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::FilePath(p) = activity_trace.activity().argument() {
            println!(
                "- {}",
                path::truncate_path(&trace.file_path(p), width.min(80))
            )
        }
    }*/

    // Play around with collected C++ entities
    println!("\nProcessing C++ entities...");
    for activity_trace in trace.all_activities() {
        match activity_trace.activity().argument() {
            /*ActivityArgument::MangledSymbol(MangledSymbol::Demangled(m))
            | ActivityArgument::MangledSymbol(MangledSymbol::Mangled(m)) => {
                println!("{m:?}");
            } *//*ActivityArgument::CppEntity(e) => {
            println!(
            "- {}({})",
            activity_trace.activity().name(),
            trace.entity(e)
            );
            }*/
            _ => {}
        }
    }
    println!("...all good!");

    // DEBUG
    /* use cpparser::values::*;
    use std::sync::atomic::AtomicUsize;
    println!();
    let print_branch = |name: &str, ctr: &AtomicUsize| {
        println!("    -> {name}: {ctr:?}");
    };
    println!("value_header parser was called {HEADER_CALLS:?} times");
    println!("... with the following outcomes:");
    print_branch("Literal", &HEADER_LITERAL);
    print_branch("Unary operator", &HEADER_UNARY);
    print_branch("Parenthesized value", &HEADER_PAREN);
    print_branch("new-expression", &HEADER_NEW);
    print_branch("id-expression", &HEADER_ID);
    println!(); */

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }
}
