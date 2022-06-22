//! Easier C++ build profiling

#![deny(missing_docs)]

mod path;

use clang_time_trace::{
    ActivityArgument, ActivityTrace, ClangTrace, CustomDisplay, Duration, MangledSymbol,
};
use clap::Parser;
use std::{
    collections::HashMap,
    io::{self, Write},
    path::PathBuf,
};
use termtree::{GlyphPalette, Tree};
use unicode_width::UnicodeWidthStr;

/// Turn a clang time-trace dump into a profiler-like visualization
#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Maximal number of terminal columns to be used in the display
    #[clap(short = 'c', long = "cols", default_value = "200")]
    max_cols: u16,

    /// Self-profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "0.5")]
    self_threshold: f32,

    /// Hierarchical profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "0.5")]
    hierarchical_threshold: f32,

    /// Clang time-trace file to be analyzed
    input: PathBuf,
}

fn main() {
    // Read various configuration
    env_logger::init();
    let args = Args::parse();
    let max_cols = termion::terminal_size()
        .map(|(width, _height)| width.min(args.max_cols))
        .unwrap_or(args.max_cols);

    // Load the clang trace
    eprintln!("Processing input data...");
    let trace = ClangTrace::from_file(args.input).unwrap();

    println!("\nData from {}", trace.process_name());

    // Total clang execution time
    let root_duration = trace
        .root_activities()
        .map(|root| root.duration())
        .sum::<f64>();
    let duration_norm = 1.0 / root_duration;

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
    println!("\nHottest activities by self-duration:");
    let hottest = hottest_activities(
        trace.all_activities(),
        |a| a.self_duration() * duration_norm,
        args.self_threshold as Duration / 100.0,
    );
    let num_hottest = hottest.len();
    for activity_trace in hottest.iter() {
        let duration = activity_trace.self_duration();
        let percent = duration * duration_norm * 100.0;
        print!("- ");
        display_activity(
            std::io::stdout(),
            &trace,
            activity_trace,
            max_cols - 2,
            duration,
            percent,
        )
        .unwrap();
        println!();
    }
    let num_activities = trace.all_activities().count();
    if num_hottest < num_activities {
        let other_activities = num_activities - num_hottest;
        println!(
            "- ... and {other_activities} other activities below {}% ...",
            args.self_threshold
        );
    }

    // Hierarchical profile
    let palette = GlyphPalette {
        middle_item: "├",
        last_item: "└",
        item_indent: "─",
        middle_skip: "│",
        last_skip: " ",
        skip_indent: " ",
    };
    println!("\nHierarchical profile:");
    assert_eq!(trace.root_activities().count(), 1);
    println!(
        "{}",
        hierarchical_profile_tree(
            &trace,
            palette,
            trace.root_activities().next().unwrap(),
            duration_norm,
            args.hierarchical_threshold as Duration / 100.0,
            max_cols
        )
    );
}

/// Make a tree display of the hierarchical profile of some build
fn hierarchical_profile_tree(
    trace: &ClangTrace,
    palette: GlyphPalette,
    root: ActivityTrace,
    duration_norm: Duration,
    threshold: Duration,
    max_cols: u16,
) -> Tree<Box<str>> {
    // Render root node
    let mut root_display = Vec::<u8>::new();
    let duration = root.duration();
    let percent = duration * duration_norm * 100.0;
    display_activity(&mut root_display, trace, &root, max_cols, duration, percent).unwrap();
    let root_display = String::from_utf8(root_display).unwrap().into_boxed_str();
    let mut tree = Tree::new(root_display).with_glyphs(palette);

    // Stop recursion when there is no space to render children
    let child_cols = max_cols
        .saturating_sub(palette.middle_item.width() as u16)
        .saturating_sub(palette.item_indent.width() as u16);
    if child_cols == 0 {
        return tree;
    }

    // Collect hottest children
    let num_children = root.direct_children().count();
    let hottest_children = hottest_activities(
        root.direct_children(),
        |a| a.duration() * duration_norm,
        threshold,
    );
    let num_hottest = hottest_children.len();

    // Render children
    let make_child_tree = |child| {
        hierarchical_profile_tree(trace, palette, child, duration_norm, threshold, child_cols)
    };
    tree = if num_hottest == num_children {
        tree.with_leaves(hottest_children.into_vec().into_iter().map(make_child_tree))
    } else {
        let mut terminator = format!(
            "…{} callee(s) below {}%…",
            num_children - num_hottest,
            threshold * 100.0
        );
        if terminator.width() > child_cols.into() {
            terminator.clear();
            terminator.push('…');
        }
        tree.with_leaves(
            hottest_children
                .into_vec()
                .into_iter()
                .map(make_child_tree)
                .chain(std::iter::once(
                    Tree::new(terminator.into()).with_glyphs(palette),
                )),
        )
    };
    tree
}

/// Extract the hottest children from an activity iterator
fn hottest_activities<'activities>(
    activities: impl Iterator<Item = ActivityTrace<'activities>> + Clone,
    mut duration: impl FnMut(&ActivityTrace) -> Duration,
    threshold: Duration,
) -> Box<[ActivityTrace<'activities>]> {
    let mut children = activities
        .filter(|a| duration(a) >= threshold)
        .collect::<Box<[_]>>();
    children.sort_unstable_by(|a1, a2| duration(a2).partial_cmp(&duration(a1)).unwrap());
    children
}

/// Truncate a string so that it only eats up n columns, by eating up the middle
fn truncate_string(input: &str, max_cols: u16) -> String {
    // Make sure the request makes sense, set up common infrastructure
    debug_assert!(input.width() > max_cols.into());
    debug_assert!(max_cols >= 1);
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

/// Display a duration in a human-readable format
fn display_duration(
    mut output: impl io::Write,
    duration: Duration,
    hms: Option<HMS>,
) -> io::Result<()> {
    const MILLISECOND: Duration = 1000.0;
    const SECOND: Duration = 1000.0 * MILLISECOND;
    const MINUTE: Duration = 60.0 * SECOND;
    const HOUR: Duration = 60.0 * MINUTE;
    if duration >= HOUR {
        let hours = (duration / HOUR).floor();
        write!(output, "{}:", hours)?;
        display_duration(output, duration - hours * HOUR, Some(HMS::ForceMinute))
    } else if duration >= MINUTE || hms == Some(HMS::ForceMinute) {
        let minutes = (duration / MINUTE).floor();
        write!(output, "{}:", minutes)?;
        display_duration(output, duration - minutes * MINUTE, Some(HMS::ForceSecond))
    } else if duration >= SECOND || hms == Some(HMS::ForceSecond) {
        write!(output, "{:.2}", duration / SECOND)?;
        if hms != Some(HMS::ForceSecond) {
            write!(output, "s")?;
        }
        Ok(())
    } else if duration >= MILLISECOND {
        write!(output, "{:.2}ms", duration / MILLISECOND)
    } else {
        write!(output, "{duration}µs")
    }
}
//
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum HMS {
    ForceMinute,
    ForceSecond,
}

/// Display an activity trace
fn display_activity(
    mut output: impl io::Write,
    trace: &ClangTrace,
    activity_trace: &ActivityTrace,
    mut max_cols: u16,
    duration: Duration,
    percent: Duration,
) -> io::Result<()> {
    assert!(max_cols >= 1);
    let activity_name = activity_trace.activity().name();
    let activity_arg = activity_trace.activity().argument();

    // Can we display at least ActivityName(…)?
    if usize::from(max_cols) < activity_name.width() + 3 {
        // If not, display just an ellipsis
        return write!(output, "…");
    } else {
        // If so, display the activity name...
        write!(output, "{activity_name}")?;
    }
    // ...and account for the reserved space
    max_cols -= activity_name.width() as u16 + 2;

    // Do a test display of the trailing profiling numbers
    let mut trailer = Vec::<u8>::new();
    write!(trailer, " [")?;
    display_duration(&mut trailer, duration, None)?;
    write!(trailer, ", {percent:.2}%]")?;
    let trailer = std::str::from_utf8(&trailer[..]).unwrap();

    // Allocate space for the activity name and display it
    let arg_cols = max_cols.saturating_sub(trailer.width() as u16).max(1);
    max_cols -= arg_cols;
    match activity_arg {
        ActivityArgument::Nothing => {}
        ActivityArgument::String(s)
        | ActivityArgument::MangledSymbol(MangledSymbol::Demangled(s))
        | ActivityArgument::MangledSymbol(MangledSymbol::Mangled(s)) => {
            if s.width() <= arg_cols.into() {
                write!(output, "({s})")?;
            } else {
                write!(output, "({})", truncate_string(&s, arg_cols))?;
            }
        }
        ActivityArgument::FilePath(p) => {
            write!(
                output,
                "({})",
                path::truncate_path(&trace.file_path(p), arg_cols)
            )?;
        }
        ActivityArgument::CppEntity(e)
        | ActivityArgument::MangledSymbol(MangledSymbol::Parsed(e)) => {
            write!(output, "({})", trace.entity(e).bounded_display(arg_cols))?;
        }
    }

    // Display the trailer if there's room for it
    if usize::from(max_cols) >= trailer.width() {
        write!(output, "{trailer}")?;
    }
    Ok(())
}
