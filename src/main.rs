//! Easier C++ build profiling

#![deny(missing_docs)]

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
                path::truncate_path(&trace.file_path(p), width.min(80))
            )
        }
    }

    // Play around with collected C++ entities
    println!("\nProcessing C++ entities...");
    for activity_trace in trace.all_activities() {
        match activity_trace.activity().argument() {
            ActivityArgument::MangledSymbol(m) => {
                println!("{m:?}");
            } /*ActivityArgument::CppEntity(e) => {
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
    /*println!("Interner usage statistics:");
    println!("- Identifiers: {}", parser.num_identifiers());
    println!(
        "- Paths: {} interned components, {} total components, max {} components/path",
        parser.num_unique_path_components(),
        parser.num_path_components(),
        parser.max_path_len().unwrap()
    );
    println!("- Types: {}", parser.num_types());
    println!("- Values: {}", parser.num_values());
    println!(
        "- Template parameters: {} total parameters, max {} parameters/set",
        parser.num_template_parameters(),
        parser.max_template_parameter_set_len().unwrap()
    );
    println!(
        "- Value trailers: {} total AfterValue, max {} AfterValue/set",
        parser.num_after_value(),
        parser.max_value_trailer_len().unwrap()
    );
    println!(
        "- Function calls: {} total arguments, max {} arguments/set",
        parser.num_function_arguments(),
        parser.max_function_arguments_len().unwrap()
    );
    println!(
        "- Function parameters: {} total parameters, max {} parameters/set",
        parser.num_function_parameters(),
        parser.max_function_parameters_len().unwrap()
    );
    println!(
        "- Scopes: {} total Scopes, max {} Scopes/set",
        parser.num_scopes(),
        parser.max_scope_sequence_len().unwrap()
    );
    println!(
        "- Declarators: {} total DeclOperators, max {} DeclOperators/set",
        parser.num_decl_operators(),
        parser.max_declarator_len().unwrap()
    );*/

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
