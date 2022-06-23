//! Easier C++ build profiling

#![deny(missing_docs)]

mod display;

use crate::display::{
    activity::{self, ActivityIdError},
    duration::display_duration,
};
use clang_time_trace::{ActivityTrace, ClangTrace, Duration};
use clap::Parser;
use std::{collections::HashMap, io, path::PathBuf};
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
    //
    // FIXME: After resolving the stdio-isms, add a CLI switch to flip between
    // stdio and tui, and start making a tui w/ cursive and cursive_table_view.
}

fn main() {
    // Set up infrastructure and process CLI arguments
    env_logger::init();
    let args = Args::parse();
    let max_cols = termion::terminal_size()
        .map(|(width, _height)| width.min(args.max_cols))
        .unwrap_or(args.max_cols);

    // Load the clang trace
    eprintln!("Processing input data...");
    let trace = ClangTrace::from_file(args.input).unwrap();

    println!("\nData from {}", trace.process_name());

    // Total clang execution time and associated normalization factor for
    // computing relative overheads.
    let root_duration = trace
        .root_activities()
        .map(|root| root.duration())
        .sum::<f64>();
    let duration_norm = 1.0 / root_duration;

    // Activity types by self-duration
    // FIXME: Extract this into a data extraction function & display function in display::stdio
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
        print!("- {name}");
        display_profile_info(std::io::stdout(), *duration, duration_norm).unwrap();
        println!();
    }

    // Flat activity profile by self-duration
    // FIXME: Extract this into a display function in display::stdio
    println!("\nHottest activities by self-duration:");
    let hottest = hottest_activities(
        trace.all_activities(),
        |a| a.self_duration() * duration_norm,
        args.self_threshold as Duration / 100.0,
    );
    let num_hottest = hottest.len();
    for activity_trace in hottest.iter() {
        let duration = activity_trace.self_duration();
        print!("- ");
        display_activity(
            std::io::stdout(),
            &trace,
            activity_trace,
            max_cols - 2,
            duration,
            duration_norm,
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
    // FIXME: Extract this into a display function in display::stdio that acts
    //        as the public frontend to hierarchical_profile_tree
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
// FIXME: Extract this into display::stdio
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
    display_activity(
        &mut root_display,
        trace,
        &root,
        max_cols,
        root.duration(),
        duration_norm,
    )
    .unwrap();
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

/// Display an activity trace
//
// FIXME: Extract this to display::stdio.
fn display_activity(
    mut output: impl io::Write,
    trace: &ClangTrace,
    activity_trace: &ActivityTrace,
    max_cols: u16,
    duration: Duration,
    duration_norm: Duration,
) -> io::Result<()> {
    assert!(max_cols >= 1);

    // Display the trailing profiling numbers in a private string to know its
    // display width and how many columns that leaves for the activity id.
    let mut trailer = Vec::<u8>::new();
    display_profile_info(&mut trailer, duration, duration_norm)?;
    let trailer = std::str::from_utf8(&trailer[..]).unwrap();
    let other_cols = max_cols.saturating_sub(trailer.width() as u16);

    // Try to display both the activity id and the profiling numbers
    match activity::display_activity_id(&mut output, trace, activity_trace, other_cols) {
        Ok(()) => {
            // Success, can just print out the profiling numbers
            write!(output, "{trailer}")
        }
        Err(ActivityIdError::NotEnoughCols(_)) => {
            // Not enough space for both, try to display activity ID alone
            match activity::display_activity_id(&mut output, trace, activity_trace, max_cols) {
                Ok(()) => Ok(()),
                Err(ActivityIdError::IoError(e)) => Err(e),
                Err(ActivityIdError::NotEnoughCols(_)) => {
                    // Seems the best we can do is an ellipsis placeholder...
                    write!(output, "…")
                }
            }
        }
        Err(ActivityIdError::IoError(e)) => Err(e),
    }
}

/// Display profiling information
//
// FIXME: Extract this to display::stdio.
fn display_profile_info(
    mut output: impl io::Write,
    duration: Duration,
    duration_norm: Duration,
) -> io::Result<()> {
    write!(output, " [")?;
    display_duration(&mut output, duration)?;
    let percent = duration * duration_norm * 100.0;
    write!(output, ", {percent:.2}%]")
}

/// Extract the hottest activities from an activity iterator
///
/// - `duration` is the sorting criterion (can be duration(), self_duration(),
///   or a normalized version thereof for percentages)
/// - `threshold` is the duration threshold below which activities are dropped
///
fn hottest_activities<'activities>(
    activities: impl Iterator<Item = ActivityTrace<'activities>>,
    mut duration: impl FnMut(&ActivityTrace) -> Duration,
    threshold: Duration,
) -> Box<[ActivityTrace<'activities>]> {
    let mut children = activities
        .filter(|a| duration(a) >= threshold)
        .collect::<Box<[_]>>();
    children.sort_unstable_by(|a1, a2| duration(a2).partial_cmp(&duration(a1)).unwrap());
    children
}
