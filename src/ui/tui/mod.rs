//! Interactive textual user interface

mod processing;

use self::processing::{ActivityInfo, ProcessingThread};

use super::display::duration::display_duration;
use crate::CliArgs;
use clang_time_trace::{ActivityTraceId, ClangTraceLoadError, Duration};
use cursive::{
    event::{Event, Key},
    view::{Nameable, Resizable},
    views::{Dialog, LinearLayout, OnEventView, TextView},
    Cursive, CursiveRunnable,
};
use cursive_table_view::{TableView, TableViewItem};
use std::{
    cmp::Ordering,
    path::Path,
    rc::Rc,
    sync::atomic::{self, AtomicUsize},
};
use unicode_width::UnicodeWidthStr;

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up the text user interface
    let mut cursive = setup_cursive();

    // Load the clang time trace
    let processing_thread = match load_input(&mut cursive, &args.input) {
        Ok(trace) => Rc::new(trace),
        Err(error) => {
            cursive.add_layer(
                Dialog::text(format!("Failed to process input: {error}"))
                    .button("Quit", |cursive| cursive.quit()),
            );
            cursive.run();
            return;
        }
    };

    // TODO: Add activity summary on S

    // Display the hierarchical profile
    let root_activities = processing_thread.get_root_activities();
    show_hierarchical_profile(&mut cursive, processing_thread, root_activities);

    // Start the cursive event loop
    cursive.run();
}

/// Perform basic cursive setup
fn setup_cursive() -> CursiveRunnable {
    let mut cursive = cursive::default();

    // Confirm before quitting
    let quit_callback = |cursive: &mut Cursive| {
        cursive.add_layer(
            Dialog::text("Ready to quit?")
                .button("Yes", |cursive| cursive.quit())
                .dismiss_button("No"),
        );
    };
    cursive.set_global_callback('q', quit_callback.clone());
    cursive.set_global_callback(Event::CtrlChar('c'), quit_callback.clone());

    // Set up help text
    // TODO: Update as the feature set increases
    cursive.set_global_callback('h', |cursive| {
        cursive.add_layer(Dialog::info(
            "Duration is the total time spent on an activity\n\
        Self is Duration minus time spent on callees\n\
        Activity is what clang was doing\n\
        + means an activity triggered other activities\n\
        \n\
        Available commands:\n\
        - Up/Down selects an activity\n\
        - Return zooms on an activity's callees\n\
        - Left/Right + Return adjusts sort\n\
        - Esc goes back to previous view\n\
        - Q quits this program",
        ))
    });
    cursive
}

/// Load the ClangTrace with a pretty loading screen
fn load_input(
    cursive: &mut CursiveRunnable,
    path: &Path,
) -> Result<ProcessingThread, ClangTraceLoadError> {
    // Set up the loading screen
    cursive.add_layer(Dialog::text("Processing input data...").button("Abort", |s| s.quit()));

    // Start the processing thread
    let mut processing_thread = ProcessingThread::start(path);

    // Initiate the cursive event loop
    let mut runner = cursive.runner();
    runner.refresh();
    loop {
        // Process TUI events
        runner.step();

        // Abort input processing if instructed to do so
        if !runner.is_running() {
            // FIXME: Replace by regular return once input processing is faster
            std::mem::drop(runner);
            std::process::abort()
        }

        // Otherwise check how the input processing is going
        match processing_thread.try_extract_load_result() {
            None => continue,
            Some(result) => break result.map(|()| processing_thread),
        }
    }
}

/// Current hierarchical profile recursion depth
static HIERARCHICAL_RECURSION_DEPTH: AtomicUsize = AtomicUsize::new(0);

/// Display a hierarchical profile
fn show_hierarchical_profile<'a>(
    cursive: &mut Cursive,
    processing_thread: Rc<ProcessingThread>,
    activity_infos: Box<[ActivityInfo]>,
) {
    // Set up the children activity table
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");
    let activity_width = terminal_width - 2 * (12 + 3) - 3;
    type HierarchicalView = TableView<HierarchicalData, HierarchicalColumn>;
    let mut table = HierarchicalView::new()
        .column(HierarchicalColumn::Duration, "Duration", |c| {
            c.width(12).ordering(Ordering::Greater)
        })
        .column(HierarchicalColumn::SelfDuration, "Self", |c| {
            c.width(12).ordering(Ordering::Greater)
        })
        .column(HierarchicalColumn::Description, "Activity", |c| {
            c.width(activity_width.into()).ordering(Ordering::Less)
        })
        .default_column(HierarchicalColumn::Duration);

    // Collect children activities into the table
    // FIXME: Honor thresholds
    let activity_descs = processing_thread.describe_activities(
        activity_infos.iter().map(|info| info.id).collect(),
        activity_width,
    );
    let items = activity_infos
        .iter()
        .zip(activity_descs.into_vec().into_iter())
        .map(|(activity_info, description)| {
            let mut buf = String::new();
            if activity_info.has_children {
                buf.push('+');
            } else {
                buf.push(' ');
            }
            buf.push_str(&description);
            HierarchicalData {
                id: activity_info.id,
                duration: activity_info.duration,
                self_duration: activity_info.self_duration,
                description: buf.into_boxed_str(),
            }
        })
        .collect();
    table.set_items(items);
    table.set_selected_row(0);

    // Recurse into an activity's children when an activity is selected
    let recursion_depth = HIERARCHICAL_RECURSION_DEPTH.fetch_add(1, atomic::Ordering::Relaxed);
    let profile_name = format!("hierarchical_profile{}", recursion_depth);
    let processing_thread_2 = processing_thread.clone();
    let profile_name2 = profile_name.clone();
    table.set_on_submit(move |cursive, _row, index| {
        let activity_trace_id = cursive
            .call_on_name(&profile_name2, |view: &mut HierarchicalView| {
                view.borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index")
                    .id
            })
            .expect("Failed to retrieve cursive layer");
        let activity_children = processing_thread_2.get_direct_children(activity_trace_id);
        if !activity_children.is_empty() {
            show_hierarchical_profile(cursive, processing_thread_2.clone(), activity_children)
        }
    });
    let table = table.with_name(profile_name);

    // Make the Escape key attempt to undo the last layer of recursion
    let table = OnEventView::new(table).on_event(Key::Esc, |cursive| {
        let current_depth = HIERARCHICAL_RECURSION_DEPTH.load(atomic::Ordering::Relaxed);
        if current_depth > 1 {
            HIERARCHICAL_RECURSION_DEPTH.fetch_sub(1, atomic::Ordering::Relaxed);
            cursive.pop_layer();
        }
    });
    // TODO: Add F shortcut for flat profile

    // Set up a footer with metadata and help instructions
    let mut footer = processing_thread.describe_trace(terminal_width);
    {
        let last_line = footer
            .lines()
            .last()
            .expect("There should be text in there");
        const HELP_TEXT: &str = "Press H for help.";
        if last_line.width() + 1 + HELP_TEXT.width() <= terminal_width as usize {
            footer.push(' ');
        } else {
            footer.push('\n');
        }
        footer.push_str(HELP_TEXT);
    }
    let footer_lines = footer.lines().count() as u16;

    // Show the table
    cursive.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(table.min_size((terminal_width, terminal_height - footer_lines)))
            .child(TextView::new(footer)),
    );
}

/// Hierarchical profile column identifier
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum HierarchicalColumn {
    /// Time spent processing this activity or one of its callees
    Duration,

    /// Time spent specificially processing this activity
    SelfDuration,

    /// Activity description
    Description,
}
//
/// Row of hierarchical profile data
#[derive(Clone, Debug)]
struct HierarchicalData {
    /// Activity identifier
    id: ActivityTraceId,

    /// Time spent processing this activity or one of its callees
    duration: Duration,

    /// Time spent specificially processing this activity
    self_duration: Duration,

    /// Activity description
    description: Box<str>,
}
//
impl TableViewItem<HierarchicalColumn> for HierarchicalData {
    fn to_column(&self, column: HierarchicalColumn) -> String {
        let format_duration = |duration| {
            let mut buffer = Vec::<u8>::new();
            display_duration(&mut buffer, duration).expect("Writing to a buffer shouldn't fail");
            String::from_utf8(buffer).expect("display_duration should produce UTF-8 data")
        };
        match column {
            // FIXME: Add a way to switch to percentage display, which will
            //        likely require use of a global variable. Default to %total
            HierarchicalColumn::Duration => format_duration(self.duration),
            HierarchicalColumn::SelfDuration => format_duration(self.self_duration),
            HierarchicalColumn::Description => self.description.clone().into(),
        }
    }

    fn cmp(&self, other: &Self, column: HierarchicalColumn) -> Ordering
    where
        Self: Sized,
    {
        let cmp_duration = |d1: Duration, d2: Duration| {
            d1.partial_cmp(&d2).expect("time-trace shouldn't emit NaNs")
        };
        match column {
            HierarchicalColumn::Duration => cmp_duration(self.duration, other.duration),
            HierarchicalColumn::SelfDuration => {
                cmp_duration(self.self_duration, other.self_duration)
            }
            HierarchicalColumn::Description => self.description.cmp(&other.description),
        }
    }
}
