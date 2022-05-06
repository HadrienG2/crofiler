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

    // Print a list of C++ entities that the parser doesn't handle yet
    println!("\nExamples of incompletely or wrongly parsed C++ entities:");
    let mut displayed = 0;
    const MAX_DISPLAY: usize = 30;
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::CppEntity(e) = activity_trace.activity().argument() {
            match entity(&e) {
                Ok(("", _)) => {}
                other => {
                    println!("- {other:?}");
                    displayed += 1;
                    if displayed == MAX_DISPLAY {
                        break;
                    }
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

/// Parser for C++ identifiers
fn identifier(s: &str) -> IResult<&str, &str> {
    use nom::{
        character::complete::satisfy, combinator::recognize, multi::many0_count, sequence::pair,
    };
    recognize(pair(
        satisfy(|c| c.is_xid_start() || c == '_'),
        many0_count(satisfy(UnicodeXID::is_xid_continue)),
    ))(s)
}

// TODO: Parse C++ template parameters (we can't tell if they are types or
//       values, so the parser must handle both)
// TODO: ...then parse an identifier optionally followed by an argument list

/// Parser for id-expressions
fn id_expression(s: &str) -> IResult<&str, IdExpression> {
    use nom::{
        bytes::complete::tag,
        combinator::map,
        multi::many0,
        sequence::{pair, terminated},
    };
    // FIXME: Accept templated type names
    let scope = terminated(identifier, tag("::"));
    let path = map(many0(scope), Vec::into_boxed_slice);
    // FIXME: Accept all unqualified id-expressions. In addition to identifiers,
    //        these include...
    //        - Destructors: ~identifier
    //        - Templates: identifier<param...>
    //        - Operators, including conversion operators
    let path_and_id = pair(path, identifier);
    map(path_and_id, |(path, id)| IdExpression { path, id })(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, PartialEq)]
struct IdExpression<'source> {
    /// Hierarchical scope (types or namespaces)
    path: Box<[&'source str]>,

    /// Unqualified id-expression
    id: &'source str,
}

/// Parser for clang's <unknown> C++ entity
fn unknown_entity(s: &str) -> IResult<&str, ()> {
    use nom::{bytes::complete::tag, combinator::map};
    map(tag("<unknown>"), |_| ())(s)
}

/// Parser for clang lambda types "(lambda at <file path>:<line>:<col>)"
///
/// This will fail if the file path contains a ':' sign other than a
/// Windows-style disk designator at the start, because I have no idea how to
/// handle this inherent grammar ambiguity better...
fn lambda(s: &str) -> IResult<&str, Lambda> {
    use nom::{
        bytes::complete::{tag, take_until1},
        character::complete::{anychar, char, u32},
        combinator::{map, opt, recognize},
        sequence::{delimited, pair, separated_pair},
    };
    let disk_designator = recognize(pair(anychar, char(':')));
    let path_str = recognize(pair(opt(disk_designator), take_until1(":")));
    let path = map(path_str, Path::new);
    let location = separated_pair(u32, char(':'), u32);
    let file_location = separated_pair(path, char(':'), location);
    let lambda = map(file_location, |(file, location)| Lambda { file, location });
    delimited(tag("(lambda at "), lambda, char(')'))(s)
}
//
/// Lambda location description
#[derive(Clone, Debug, PartialEq)]
struct Lambda<'source> {
    /// In which file the lambda is declared
    file: &'source Path,

    /// Where exactly in the file
    location: (Line, Col),
}
//
type Line = u32;
type Col = u32;

/// Parser for C++ entities
fn entity(s: &str) -> IResult<&str, Option<CppEntity>> {
    use nom::{branch::alt, combinator::map};
    let id_expression = map(id_expression, |i| Some(CppEntity::IdExpression(i)));
    let unknown = map(unknown_entity, |()| None);
    let lambda = map(lambda, |l| Some(CppEntity::Lambda(l)));
    alt((id_expression, unknown, lambda))(s)
}
//
/// C++ entity description
#[derive(Clone, Debug, PartialEq)]
enum CppEntity<'source> {
    IdExpression(IdExpression<'source>),
    Lambda(Lambda<'source>),
}
