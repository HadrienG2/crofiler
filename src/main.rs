//! Easier C++ build profiling

#![deny(missing_docs)]

mod path;

use clang_time_trace::{Activity, ActivityArgument, ClangTrace, Duration};
use nom::IResult;
use std::{collections::HashMap, path::Path};
use unicode_xid::UnicodeXID;

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

    // Print a list of things that should be C++, but don't start with identifiers
    println!("\nC++ entities that don't start with an identifier:");
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::CppEntity(e) = activity_trace.activity().argument() {
            let first_char = e.chars().next();
            if "" == e.as_ref() || !is_cppid_start(first_char.unwrap()) {
                println!("- {}({e})", activity_trace.activity().name());
                if e.starts_with("(lambda") {
                    println!("  * {:?}", lambda(&e));
                }
            }
        }
    }

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }
}

/// Property verified by the initial character of a C++ identifier
/// FIXME: Turn it back into a lambda inside cpp_identifier once done with this
///        part of the parsing logic.
fn is_cppid_start(c: char) -> bool {
    c.is_xid_start() || c == '_'
}

/// Parser for C++ identifiers
fn cpp_identifier(s: &str) -> IResult<&str, &str> {
    use nom::{
        character::complete::satisfy, combinator::recognize, multi::many0_count, sequence::pair,
    };
    recognize(pair(
        satisfy(is_cppid_start),
        many0_count(satisfy(UnicodeXID::is_xid_continue)),
    ))(s)
}

// TODO: Parse qualified identifiers as (<identifier>::)+ <identifier>
//       where (<identifier>::) will probably be called a location.

/// Parser for clang's <unknown> C++ entity
///
/// I have only seen this appear in ParseTemplate activities, so I could
/// probably get away with only enabling this part of the parser when parsing
/// these activities.
fn unknown_entity(s: &str) -> IResult<&str, &str> {
    use nom::bytes::complete::tag;
    tag("<unkown>")(s)
}

/// Parser for clang lambda types "(lambda at <file path>:<line>:<col>)"
///
/// I don't think this can be done using nom because file paths can basically
/// contain almost every character, including ':' and ')', so to avoid
/// ambiguities we need to parse from the right edge of the string, whereas nom
/// is designed to only parse from left to right as far as I can see.
///
/// Thus, my idea is to start by applying a nom parser, then apply this parser
/// as a last resort if nom fails.
///
/// Of course, this is assuming lambda names don't end up inside of
/// nom-parseable entities... in that case, I'll probably just go with adding
/// a dumber nom-based lambda parser that does something stupid if the file name
/// contains commas or closing parentheses, and use that inside of the
/// main nom parser.
fn lambda(mut s: &str) -> Option<(&Path, usize, usize)> {
    const HEADER: &str = "(lambda at ";
    const TRAILER: &str = ")";
    s = s.strip_prefix(HEADER)?;
    s = s.strip_suffix(TRAILER)?;
    let mut num_num_path = s.rsplitn(3, ':');
    let col = num_num_path.next()?.parse().ok()?;
    let line = num_num_path.next()?.parse().ok()?;
    let path = Path::new(num_num_path.next()?);
    Some((path, line, col))
}
