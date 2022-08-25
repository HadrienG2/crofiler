//! Interactive textual user interface

mod processing;

use self::processing::{ActivityInfo, ProcessingThread};

use super::display::duration::display_duration;
use crate::CliArgs;
use clang_time_trace::{ActivityTraceId, ClangTraceLoadError, Duration};
use cursive::{
    event::{Event, EventResult, Key},
    view::{Nameable, Resizable},
    views::{Dialog, LinearLayout, OnEventView, SelectView, TextView},
    Cursive, CursiveRunnable,
};
use cursive_table_view::{TableView, TableViewItem};
use decorum::Finite;
use log::LevelFilter;
use std::{cell::Cell, cmp::Ordering};
use syslog::Facility;
use unicode_width::UnicodeWidthStr;

/// Compute the percentage norm associated with a set of activities
fn percent_norm<'a>(total_duration: Duration) -> Finite<Duration> {
    Finite::<Duration>::from_inner(100.0 / total_duration)
}

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up logging using syslog
    syslog::init(Facility::LOG_USER, LevelFilter::Info, None).expect("Failed to initialize syslog");

    // Start the processing thread and set up the text user interface
    let mut cursive = setup_cursive(State {
        processing_thread: ProcessingThread::start(&args.input),
        profile_stack: Vec::new(),
    });

    // Load the clang time trace
    if let Err(error) = wait_for_input(&mut cursive) {
        cursive.add_layer(
            Dialog::text(format!("Failed to process input: {error}")).button("Quit", Cursive::quit),
        );
        cursive.run();
        return;
    }

    // Query the list of root activities
    let root_activities = with_state(&mut cursive, |state| {
        state.processing_thread.get_root_activities()
    });

    // Compute the global percentage norm
    let global_percent_norm = percent_norm(
        root_activities
            .iter()
            .map(|activity| activity.duration)
            .sum::<Duration>(),
    );

    // TODO: Show activity summary on S

    // Display the hierarchical profile
    show_hierarchical_profile(
        &mut cursive,
        global_percent_norm,
        "<profile root>".into(),
        global_percent_norm,
        root_activities,
    );

    // Start the cursive event loop
    cursive.run();
}

/// General UI state available via cursive's user data mechanism
struct State {
    /// Handle to the processing thread
    processing_thread: ProcessingThread,

    /// Current stack of profiling UI layers, with associated names
    profile_stack: Vec<Box<str>>,
}

/// Run a closure on the UI state
fn with_state<R>(cursive: &mut Cursive, f: impl FnOnce(&mut State) -> R) -> R {
    cursive
        .with_user_data(f)
        .expect("Failed to access UI state")
}

/// Perform basic cursive setup
fn setup_cursive(state: State) -> CursiveRunnable {
    // Initialize cursive
    let mut cursive = cursive::default();

    // Set up user state
    cursive.set_user_data(state);

    // Esc always exits the current layer if there's another underneath
    cursive.set_global_callback(Key::Esc, move |cursive| {
        let num_layers = cursive.screen().len();
        if num_layers > 1 {
            with_state(cursive, |state| {
                if num_layers == state.profile_stack.len() {
                    state.profile_stack.pop();
                }
            });
            cursive.pop_layer();
        }
    });

    // All global dialogs have the same name, to avoid spawning multiple ones on
    // top of each other, which is jarring and has no current known use case
    fn set_global_dialog_callback(
        cursive: &mut Cursive,
        event: impl Into<Event>,
        mut dialog_factory: impl 'static + FnMut(&mut Cursive) -> Dialog,
    ) {
        cursive.set_global_callback(event, move |cursive| {
            const GLOBAL_DIALOG_NAME: &str = "Global dialog";
            if !cursive
                .screen_mut()
                .find_layer_from_name(GLOBAL_DIALOG_NAME)
                .is_some()
            {
                let dialog = dialog_factory(cursive).with_name(GLOBAL_DIALOG_NAME);
                cursive.add_layer(dialog);
            }
        });
    }

    // Q and Ctrl+C quit, after confirming that this is wanted
    fn quit_dialog_factory(_: &mut Cursive) -> Dialog {
        Dialog::text("Ready to quit?")
            .button("Yes", Cursive::quit)
            .dismiss_button("No")
    }
    set_global_dialog_callback(&mut cursive, 'q', quit_dialog_factory);
    set_global_dialog_callback(&mut cursive, Event::CtrlChar('c'), quit_dialog_factory);

    // B displays an interactive backtrace
    set_global_dialog_callback(&mut cursive, 'b', |cursive| {
        let mut select = SelectView::new();
        with_state(cursive, |state| {
            select.add_all(
                state
                    .profile_stack
                    .iter()
                    .enumerate()
                    .rev()
                    .map(|(idx, name)| {
                        let choice = if name.starts_with('+') {
                            String::from(&name[1..]).into()
                        } else {
                            name.clone()
                        };
                        (choice, idx)
                    }),
            )
        });
        select.set_on_submit(|cursive, idx| {
            let screen = cursive.screen_mut();
            while screen.len() > idx + 1 {
                screen.pop_layer();
            }
            with_state(cursive, |state| state.profile_stack.truncate(idx + 1));
        });
        Dialog::around(select)
            .title("Backtrace")
            .dismiss_button("Ok")
    });

    // Set up help text
    // TODO: Update as the feature set increases
    set_global_dialog_callback(&mut cursive, 'h', |_| {
        Dialog::info(
            "The first column is the time spent on an activity\n\
        Self is that minus the time spent on callees\n\
        Activity is what clang was doing\n\
        + means an activity triggered other activities\n\
        \n\
        Available commands:\n\
        - Up/Down selects an activity\n\
        - Return zooms on an activity's callees\n\
        - Left/Right + Return adjusts sort\n\
        - Esc exits dialogs and goes up the backtrace\n\
        - U switches between duration units\n\
        - B shows the backtrace to the current activity\n\
        - Q quits this program\n\
        \n\
        Logs are sent to syslog to avoid display issues",
        )
    });

    // Bubble up TUI state
    cursive
}

/// Load the ClangTrace with a pretty loading screen
fn wait_for_input(cursive: &mut CursiveRunnable) -> Result<(), ClangTraceLoadError> {
    // Set up the loading screen
    cursive.add_layer(Dialog::text("Processing input data...").button("Abort", Cursive::quit));

    // Initiate the cursive event loop
    let mut runner = cursive.runner();
    runner.refresh();
    let result = loop {
        // Process TUI events
        runner.step();

        // Abort input processing if instructed to do so
        if !runner.is_running() {
            // FIXME: Replace by regular return once input processing is faster
            std::mem::drop(runner);
            std::process::abort()
        }

        // Otherwise check how the input processing is going
        match with_state(&mut runner, |state| {
            state.processing_thread.try_extract_load_result()
        }) {
            None => continue,
            Some(result) => break result,
        }
    };

    // Clear screen layers before returning
    while !cursive.screen().is_empty() {
        cursive.pop_layer();
    }
    result
}

/// Display a hierarchical profile
fn show_hierarchical_profile<'a>(
    cursive: &mut Cursive,
    global_percent_norm: Finite<Duration>,
    parent_name: Box<str>,
    parent_percent_norm: Finite<Duration>,
    activity_infos: Box<[ActivityInfo]>,
) {
    // Set up the children activity table
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");
    let duration_width = 12;
    let activity_width = terminal_width as usize - 2 * (duration_width + 3) - 3;
    type HierarchicalView = TableView<HierarchicalData, HierarchicalColumn>;
    let duration_display =
        DurationDisplay::Percentage(global_percent_norm, PercentageReference::Global);
    let total_duration_col = HierarchicalColumn::Duration(DurationKind::Total, duration_display);
    let mut table = HierarchicalView::new()
        .column(total_duration_col, "%Total", |c| {
            c.width(duration_width).ordering(Ordering::Greater)
        })
        .column(
            HierarchicalColumn::Duration(DurationKind::Myself, duration_display),
            "Self",
            |c| c.width(duration_width).ordering(Ordering::Greater),
        )
        .column(HierarchicalColumn::Description, "Activity", |c| {
            c.width(activity_width.into()).ordering(Ordering::Less)
        })
        .default_column(total_duration_col);

    // Collect children activities into the table
    // FIXME: Honor thresholds
    let activity_descs = with_state(cursive, |state| {
        state.processing_thread.describe_activities(
            activity_infos.iter().map(|info| info.id).collect(),
            activity_width as u16,
        )
    });
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
    let parent_name2 = parent_name.clone();
    table.set_on_submit(move |cursive, _row, index| {
        let (activity_trace_id, activity_desc, activity_duration) = cursive
            .call_on_name(&parent_name2, |view: &mut OnEventView<HierarchicalView>| {
                let activity = view
                    .get_inner_mut()
                    .borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index");
                (activity.id, activity.description.clone(), activity.duration)
            })
            .expect("Failed to retrieve cursive layer");
        let activity_children = with_state(cursive, |state| {
            state
                .processing_thread
                .get_direct_children(activity_trace_id)
        });
        if !activity_children.is_empty() {
            show_hierarchical_profile(
                cursive,
                global_percent_norm,
                activity_desc,
                percent_norm(activity_duration),
                activity_children,
            )
        }
    });

    // Register the profile into our profile stack, and note if it is the first
    // layer of the stack
    with_state(cursive, |state| {
        state.profile_stack.push(parent_name.clone())
    });

    // Make the U key switch units
    // FIXME: Do this for all profiles in the stack
    let duration_display = Cell::new(duration_display);
    let table = OnEventView::new(table).on_event_inner('u', move |table, _event| {
        // Check the initial duration sort order, if any
        let (old_sort_key, old_sort_order) = table.order().expect("Table should be sorted");
        let duration_sort_order = if let HierarchicalColumn::Duration(_, _) = old_sort_key {
            old_sort_order
        } else {
            Ordering::Greater
        };

        // Switch to the next duration display
        let (new_duration_display, new_title) = match duration_display.get() {
            DurationDisplay::Percentage(_, PercentageReference::Global) => (
                DurationDisplay::Percentage(parent_percent_norm, PercentageReference::Parent),
                "Parent%",
            ),
            DurationDisplay::Percentage(_, PercentageReference::Parent) => {
                (DurationDisplay::Time, "Duration")
            }
            DurationDisplay::Time => (
                DurationDisplay::Percentage(global_percent_norm, PercentageReference::Global),
                "Total%",
            ),
        };
        duration_display.set(new_duration_display);

        // Recreate the duration columns accordingly
        table.remove_column(1);
        table.remove_column(0);
        let total_duration_col =
            HierarchicalColumn::Duration(DurationKind::Total, new_duration_display);
        let self_duration_col =
            HierarchicalColumn::Duration(DurationKind::Myself, new_duration_display);
        table.insert_column(0, total_duration_col, new_title, |c| {
            c.width(duration_width).ordering(duration_sort_order)
        });
        table.insert_column(1, self_duration_col, "Self", |c| {
            c.width(duration_width).ordering(duration_sort_order)
        });

        // Re-select a new duration column if its former self was selected
        match old_sort_key {
            HierarchicalColumn::Duration(DurationKind::Total, _) => {
                table.set_default_column(total_duration_col);
            }
            HierarchicalColumn::Duration(DurationKind::Myself, _) => {
                table.set_default_column(self_duration_col);
            }
            HierarchicalColumn::Description => {}
        }

        // Mark the event as consumed
        Some(EventResult::Consumed(None))
    });
    let table = table.with_name(parent_name);

    // TODO: Add F shortcut for flat profile

    // Set up a footer with metadata and help instructions
    let mut footer = with_state(cursive, |state| {
        state.processing_thread.describe_trace(terminal_width)
    });
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
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum HierarchicalColumn {
    /// Time spent doing something
    Duration(DurationKind, DurationDisplay),

    /// Activity description
    Description,
}
//
/// Kind of duration to be displayed
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DurationKind {
    /// Total time spent processing this activity and its callees
    Total,

    /// Time spent processing this activity specifically, excluding callees
    Myself,
}
//
/// Kind of display to be used for a Duration column
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DurationDisplay {
    /// Absolute time elapsed processing this activity
    Time,

    /// Relative time as a percentage of another activity's execution time
    ///
    /// The parameter to this enum is a norm that can be multiplied by the
    /// absolute duration to get a percentage for display.
    ///
    Percentage(Finite<Duration>, PercentageReference),
}
//
/// Reference durations with respect to which percentages can be computed
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum PercentageReference {
    /// Total time spent running clang
    Global,

    /// Time spent running the parent activity
    Parent,
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
            HierarchicalColumn::Duration(kind, display) => {
                let data = match kind {
                    DurationKind::Total => self.duration,
                    DurationKind::Myself => self.self_duration,
                };
                match display {
                    DurationDisplay::Time => format_duration(data),
                    DurationDisplay::Percentage(norm, _reference) => {
                        format!("{:.2}%", data * norm.into_inner())
                    }
                }
            }
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
            HierarchicalColumn::Duration(DurationKind::Total, _) => {
                cmp_duration(self.duration, other.duration)
            }
            HierarchicalColumn::Duration(DurationKind::Myself, _) => {
                cmp_duration(self.self_duration, other.self_duration)
            }
            HierarchicalColumn::Description => self.description.cmp(&other.description),
        }
    }
}
