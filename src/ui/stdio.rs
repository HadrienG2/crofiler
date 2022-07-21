//! Display facilities which are specific to the non-interactive stdio display

use super::display::{
    activity::{self, ActivityDescError},
    duration::display_duration,
    metadata::metadata,
};
use crate::{profile, CliArgs};
use clang_time_trace::{ActivityArgument, ActivityId, ActivityTrace, ClangTrace, Duration};
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
    let trace = match ClangTrace::from_file(args.input) {
        Ok(trace) => trace,
        Err(e) => {
            return eprintln!("Failed to process input: {e}");
        }
    };

    // Display basic metadata
    println!("\n{}", metadata(&trace, max_cols));

    // Use total clang execution time as a duration norm
    let duration_norm = profile::duration_norm(trace.root_activities());

    // Activity types by self-duration
    let self_threshold = args.self_threshold as Duration / 100.0;
    print_activity_type_profile(&trace, duration_norm, self_threshold);

    // Flat activity profile by self-duration
    print_flat_profile(&trace, duration_norm, self_threshold, max_cols);

    // Display hierarchical profile
    print_hierarchical_profile(
        &trace,
        duration_norm,
        args.hierarchical_threshold as Duration / 100.0,
        max_cols,
    );
}

/// Display the amount of time spent on various activity types
fn print_activity_type_profile(trace: &ClangTrace, duration_norm: Duration, threshold: Duration) {
    println!("\nSelf-duration breakdown by activity type:");
    let activity_type_breakdown = profile::activity_type_breakdown(&trace);
    for (idx, (name, duration)) in activity_type_breakdown.iter().enumerate() {
        if duration * duration_norm < threshold {
            println!(
                "- ... and {} other activity types below {:.2}% ...",
                activity_type_breakdown.len() - idx,
                threshold * 100.0,
            );
            break;
        }
        print!("- {name}");
        display_profile_info(std::io::stdout(), *duration, duration_norm)
            .expect("Writing to stdout shouldn't fail");
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
            activity_trace.activity().id(),
            &activity_trace.activity().argument(trace),
            max_cols - 2,
            duration,
            duration_norm,
        )
        .expect("Writing to stdout shouldn't fail");
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
            trace
                .root_activities()
                .next()
                .expect("There should be one ExecuteCompiler root activity"),
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
        root.activity().id(),
        &root.activity().argument(trace),
        max_cols,
        root.duration(),
        duration_norm,
    )
    .expect("Writing to a collection shouldn't fail");
    let root_display = String::from_utf8(root_display)
        .expect("display_activity shouldn't produce non-UTF8 bytes")
        .into_boxed_str();
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
            "…{} callee(s) below {:.2}%…",
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
    activity_id: ActivityId,
    activity_arg: &ActivityArgument,
    max_cols: u16,
    duration: Duration,
    duration_norm: Duration,
) -> io::Result<()> {
    assert!(max_cols >= 1);

    // Display the trailing profiling numbers in a private string to know its
    // display width and how many columns that leaves for the activity id.
    let mut trailer = Vec::<u8>::new();
    display_profile_info(&mut trailer, duration, duration_norm)?;
    let trailer = std::str::from_utf8(&trailer[..])
        .expect("display_profile_info shouldn't produce non-UTF8 bytes");
    let other_cols = max_cols.saturating_sub(trailer.width() as u16);

    // Try to display both the activity id and the profiling numbers
    match activity::display_activity_desc(&mut output, activity_id, activity_arg, other_cols) {
        Ok(()) => {
            // Success, can just print out the profiling numbers
            write!(output, "{trailer}")
        }
        Err(ActivityDescError::NotEnoughCols(_)) => {
            // Not enough space for both, try to display activity ID alone
            match activity::display_activity_desc(&mut output, activity_id, activity_arg, max_cols)
            {
                Ok(()) => Ok(()),
                Err(ActivityDescError::IoError(e)) => Err(e),
                Err(ActivityDescError::NotEnoughCols(_)) => {
                    // Seems the best we can do is an ellipsis placeholder...
                    write!(output, "…")
                }
            }
        }
        Err(ActivityDescError::IoError(e)) => Err(e),
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
