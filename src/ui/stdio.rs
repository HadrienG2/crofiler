//! Display facilities which are specific to the non-interactive stdio display

use super::display::{
    activity::{self, ActivityIdError},
    duration::display_duration,
};
use crate::{profile, CliArgs};
use clang_time_trace::{ActivityTrace, ClangTrace, Duration};
use std::io;
use termtree::{GlyphPalette, Tree};
use unicode_width::UnicodeWidthStr;

/// Run the analysis using the stdio display
pub fn run(args: CliArgs) {
    // Determine column budget
    let max_cols = termion::terminal_size()
        .map(|(width, _height)| width.min(args.max_cols))
        .unwrap_or(args.max_cols);

    // Load the clang trace
    eprintln!("Processing input data...");
    let trace = ClangTrace::from_file(args.input).unwrap();

    // Display basic metadata
    println!("Data from {}", trace.process_name());

    // Use total clang execution time as a duration norm
    let duration_norm = profile::duration_norm(trace.root_activities());

    // Activity types by self-duration
    print_activity_type_profile(&trace, duration_norm);

    // Flat activity profile by self-duration
    print_flat_profile(
        &trace,
        duration_norm,
        args.self_threshold as Duration / 100.0,
        max_cols,
    );

    // Display hierarchical profile
    print_hierarchical_profile(
        &trace,
        duration_norm,
        args.hierarchical_threshold as Duration / 100.0,
        max_cols,
    );
}

/// Display the amount of time spent on various activity types
fn print_activity_type_profile(trace: &ClangTrace, duration_norm: Duration) {
    println!("\nSelf-duration breakdown by activity type:");
    let activity_type_breakdown = profile::activity_type_breakdown(&trace);
    for (name, duration) in activity_type_breakdown.iter() {
        print!("- {name}");
        display_profile_info(std::io::stdout(), *duration, duration_norm).unwrap();
        println!();
    }
}

/// Display the hottest activities by the self_duration metric
fn print_flat_profile(
    trace: &ClangTrace,
    duration_norm: Duration,
    threshold: Duration,
    max_cols: u16,
) {
    println!("\nHottest activities by self-duration:");
    let hottest = profile::hottest_activities(
        trace.all_activities(),
        |a| a.self_duration() * duration_norm,
        threshold,
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
        println!(
            "- ... and {} other activities below {:.2}% ...",
            num_activities - num_hottest,
            threshold * 100.0
        );
    }
}

/// Display a hierarchical profile
fn print_hierarchical_profile(
    trace: &ClangTrace,
    duration_norm: Duration,
    threshold: Duration,
    max_cols: u16,
) {
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
            threshold,
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
    let hottest_children = profile::hottest_activities(
        root.direct_children(),
        |a| a.duration() * duration_norm,
        threshold,
    );
    let num_hottest = hottest_children.len();

    // Render hottest children
    let make_child_tree = |child| {
        hierarchical_profile_tree(trace, palette, child, duration_norm, threshold, child_cols)
    };
    tree = if num_hottest == num_children {
        tree.with_leaves(hottest_children.into_vec().into_iter().map(make_child_tree))
    } else {
        // If there are more children, warn about it
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

/// Display an activity trace, ideally with associated profiling information
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

/// Display profiling information (absolute and relative duration)
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
