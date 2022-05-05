//! Easier C++ build profiling

#![deny(missing_docs)]

use clang_time_trace::{ActivityArgument, ClangTrace, Duration, InternedPath};
use std::collections::HashMap;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

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

    // Print a list of file paths
    println!("\nFile paths:");
    let (width, _height) = termion::terminal_size().unwrap();
    for activity_trace in trace.all_activities() {
        match activity_trace.activity().argument() {
            ActivityArgument::FilePath(p) => {
                println!(
                    "- {}",
                    truncated_path(&trace.file_path(&p), (width / 2).min(80))
                )
            }
            _ => {}
        }
    }
}

/// Render a possibly truncated version of a file path, that aims to fit within
/// a certain number of terminal columns
///
/// Unfortunately, in the age of Unicode, estimating the amount of terminal
/// columns that a certain string will cover when rendered is difficult. This
/// function uses the unicode-width crate's estimate, which itself assigns
/// width to code points using Unicode Standard Annex #11 then sums up across
/// all code points in the string.
///
/// This algorithm is not foolproof, for example it will mis-handle emoji
/// combinations like 👩 + 🔬 ➝ 👩‍🔬, nor isolated combining characters like this
/// Korean jongseong: ᆨ. But in my testing, common Linux terminals wouldn't
/// handle those strings correctly either, likely because they use a similar
/// algorithm, so we're kinda state of the art in this respect...
fn truncated_path(path: &InternedPath, cols: u16) -> Box<str> {
    // Track remaining column budget, keeping 1 spare column to insert an
    // ellipsis if needed
    assert!(
        cols > 0,
        "Need at least one terminal column to display something"
    );
    let cols = usize::from(cols);
    let mut remaining_cols = cols - 1;

    // Track number of accepted path components on front and back
    let mut accepted_front = 0;
    let mut accepted_back = 0;

    // Check how many path components we can print on the front & back sides
    let enumerate_components = || path.components().map(|c| c.value());
    let mut components = enumerate_components();
    let mut front = false;
    let mut other_side_full = false;
    loop {
        // Check out next path component in desired direction
        let candidate = if front {
            components.next()
        } else {
            components.next_back()
        };

        // Assuming there is one...
        if let Some(candidate) = candidate {
            // ...check if it fits within our column budget
            let fitting = candidate.width() < remaining_cols;
            if fitting {
                // If so, take note that we accepted one in that direction...
                if front {
                    accepted_front += 1;
                } else {
                    accepted_back += 1;
                }

                // ...and update column budget, accounting for separator
                remaining_cols -= candidate.width() + 1;
            }

            // Switch candidate lookup direction if there are still possible
            // valid candidates in the other direction
            front = front ^ !other_side_full;

            // Upon encountering a non-fitting candidate, stop looking in the
            // associated direction, or stop search if no valid direction remains.
            if !fitting {
                if other_side_full {
                    break;
                } else {
                    other_side_full = true;
                }
            }
        } else {
            // If there is no remaining path component, we're done
            break;
        }
    }

    // Generate the elided path
    let mut buffer = String::new();
    let mut components = enumerate_components();
    const SEPARATOR: char = std::path::MAIN_SEPARATOR;
    const MIN_FILENAME_WIDTH: usize = 15;
    if accepted_back > 0 {
        // The standard method is to print accepted front components, then … if
        // needed, then accepted back components
        for idx in 0..accepted_front {
            buffer.push_str(components.next().unwrap().as_ref());
            if idx != 0 {
                buffer.push(SEPARATOR);
            }
        }
        //
        let num_components = enumerate_components().count();
        assert!(num_components >= accepted_front + accepted_back);
        let elided_components = num_components - (accepted_front + accepted_back);
        if elided_components > 0 {
            buffer.push('…');
            for _ in 0..elided_components {
                components.next();
            }
        } else {
            buffer.pop();
        }
        //
        for _ in 0..accepted_back {
            buffer.push(SEPARATOR);
            buffer.push_str(components.next().unwrap().as_ref());
        }
    } else if cols >= MIN_FILENAME_WIDTH + 2 {
        // If the file name is so long or the terminal is so short that we can't
        // even print out the file name, switch to the alternate strategy of
        // eliding the middle of the file name
        remaining_cols = cols;
        buffer.push('…');
        buffer.push(SEPARATOR);
        remaining_cols -= buffer.width();
        //
        let mut graphemes = components
            .next_back()
            .expect("Expected file name")
            .graphemes(true);
        //
        let max_front_cols = remaining_cols / 2;
        let mut rem_front_cols = max_front_cols;
        while let Some(front_grapheme) = graphemes.next() {
            if front_grapheme.width() < rem_front_cols {
                buffer.push_str(front_grapheme);
                rem_front_cols -= front_grapheme.width();
            } else {
                break;
            }
        }
        remaining_cols -= max_front_cols - rem_front_cols + 1;
        buffer.push('…');
        //
        let mut back_graphemes = Vec::new();
        while let Some(back_grapheme) = graphemes.next_back() {
            if back_grapheme.width() < remaining_cols {
                back_graphemes.push(back_grapheme);
                remaining_cols -= back_grapheme.width();
            } else {
                break;
            }
        }
        for back_grapheme in back_graphemes.into_iter().rev() {
            buffer.push_str(back_grapheme);
        }
    } else {
        // Not enough terminal space to display anything useful
        buffer.push('…');
    }
    assert!(
        buffer.width() <= cols,
        "Failed to honor requested column budget"
    );
    buffer.into()
}
