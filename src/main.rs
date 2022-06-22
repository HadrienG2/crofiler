//! Easier C++ build profiling

#![deny(missing_docs)]

mod path;

use clang_time_trace::{
    ActivityArgument, ActivityTrace, ClangTrace, CustomDisplay, Duration, MangledSymbol,
};
use clap::Parser;
use std::{collections::HashMap, path::PathBuf};
use unicode_width::UnicodeWidthStr;

/// Turn a clang time-trace dump into a profiler-like visualization
#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Number of terminal columns available to display the entity
    #[clap(short = 'c', long = "cols", default_value = "150")]
    max_cols: u16,

    /// Self-profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "1.0")]
    self_threshold: f32,

    /// Hierarchical profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "5.0")]
    hierarchical_threshold: f32,

    /// Clang time-trace file to be analyzed
    input: PathBuf,
}

fn main() {
    env_logger::init();
    let args = Args::parse();

    let trace = ClangTrace::from_file(args.input).unwrap();

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
                | ActivityArgument::MangledSymbol(MangledSymbol::Mangled(s)) => {
                    if s.width() <= args.max_cols.into() {
                        print!("({s})")
                    } else {
                        print!("({})", truncate_string(&s, args.max_cols))
                    }
                }
                ActivityArgument::FilePath(p) => {
                    print!(
                        "({})",
                        path::truncate_path(&trace.file_path(p), args.max_cols)
                    )
                }
                ActivityArgument::CppEntity(e)
                | ActivityArgument::MangledSymbol(MangledSymbol::Parsed(e)) => {
                    print!("({})", trace.entity(e).bounded_display(args.max_cols))
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
    profile(
        "self-duration",
        Box::new(|a| a.self_duration()),
        args.self_threshold as Duration / 100.0,
    );
    profile(
        "total duration",
        Box::new(|a| a.duration()),
        args.hierarchical_threshold as Duration / 100.0,
    );

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }
}

/// Truncate a string so that it only eats up n columns, by eating up the middle
fn truncate_string(input: &str, max_cols: u16) -> String {
    // Make sure the request makes sense, set up common infrastructure
    debug_assert!(input.width() > max_cols.into());
    let bytes = input.as_bytes();
    let mut result = String::new();
    let mut last_good = "";

    // Split our column budget into a header and trailer
    let trailer_cols = (max_cols - 1) / 2;
    let header_cols = max_cols - 1 - trailer_cols;

    // Find a terminal header with the right number of columns
    let mut header_bytes = header_cols;
    loop {
        let header_candidate = std::str::from_utf8(&bytes[..header_bytes.into()]);
        if let Ok(candidate) = header_candidate {
            if candidate.width() > header_cols.into() {
                break;
            } else {
                last_good = candidate;
            }
        }
        header_bytes += 1;
    }

    // Start printing out the result accordingly
    result.push_str(last_good);
    result.push('…');

    // Find a terminal trailer with the right amount of columns
    let mut trailer_start = bytes.len() - usize::from(trailer_cols);
    loop {
        let trailer_candidate = std::str::from_utf8(&bytes[trailer_start..]);
        if let Ok(candidate) = trailer_candidate {
            if candidate.width() > trailer_cols.into() {
                break;
            } else {
                last_good = candidate;
            }
        }
        trailer_start -= 1;
    }

    // Emit the result
    result.push_str(last_good);
    result
}
