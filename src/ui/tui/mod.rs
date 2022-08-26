//! Interactive textual user interface
//! FIXME: Split this into submodules

mod processing;

use self::processing::{ActivityInfo, ProcessingThread};

use super::display::duration::display_duration;
use crate::CliArgs;
use clang_time_trace::{ActivityTraceId, ClangTraceLoadError, Duration};
use cursive::{
    event::{Event, Key},
    view::{Nameable, Resizable},
    views::{Dialog, LinearLayout, SelectView, TextView, ViewRef},
    Cursive, CursiveRunnable,
};
use cursive_table_view::{TableView, TableViewItem};
use decorum::Finite;
use log::LevelFilter;
use std::cmp::Ordering;
use syslog::Facility;
use unicode_width::UnicodeWidthStr;

/// Width of duration columns
const DURATION_WIDTH: usize = 12;

/// Name of the self-duration column
const SELF_COLUMN_NAME: &str = "Self";

/// Compute the percentage norm associated with a set of activities
fn percent_norm(total_duration: Duration) -> Finite<Duration> {
    Finite::<Duration>::from_inner(100.0 / total_duration)
}

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up logging using syslog
    syslog::init(Facility::LOG_USER, LevelFilter::Info, None).expect("Failed to initialize syslog");

    // Start the processing thread and set up the text user interface
    let mut cursive = setup_cursive(State {
        processing_thread: ProcessingThread::start(&args.input),
        global_percent_norm: None,
        profile_stack: Vec::new(),
        sort_config: (
            [Ordering::Greater, Ordering::Greater, Ordering::Less],
            HierarchicalColumnName::TotalDuration,
        ),
        duration_display: None,
    });

    // Load the clang time trace
    if let Err(error) = wait_for_input(&mut cursive) {
        cursive.add_layer(
            Dialog::text(format!("Failed to process input: {error}")).button("Quit", Cursive::quit),
        );
        cursive.run();
        return;
    }

    // Query the list of root activities and deduce the global percentage norm
    let (root_activities, global_percent_norm) = with_state(&mut cursive, |state| {
        let root_activities = state.processing_thread.get_root_activities();
        let global_percent_norm = percent_norm(
            root_activities
                .iter()
                .map(|activity| activity.duration)
                .sum::<Duration>(),
        );
        (root_activities, global_percent_norm)
    });

    // TODO: Show activity summary on S

    // Display the hierarchical profile
    show_hierarchical_profile(
        &mut cursive,
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

    /// Norm to compute percentages of the full clang execution time
    ///
    /// Will be set when the first layer of hierarchical profiling is displayed.
    ///
    global_percent_norm: Option<Finite<Duration>>,

    /// Current stack of profiling UI layers
    profile_stack: Vec<ProfileLayer>,

    /// Current column sorting configuration
    ///
    /// Tells the ordering to be used for each column and default column
    /// (toplevel sort key).
    ///
    sort_config: ([Ordering; 3], HierarchicalColumnName),

    /// Current duration display configuration
    ///
    /// Will be set when the first layer of hierarchical profiling is displayed.
    ///
    duration_display: Option<DurationDisplay>,
}
//
/// Profiling layer name and percentage norm
type ProfileLayer = (Box<str>, Finite<Duration>);

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

    // U switches between duration units for all active profiles
    cursive.set_global_callback('u', switch_duration_unit);

    // We do not allow dialogs spawned by global keyboard shortcuts to stack on
    // top of each other as this is jarring and has no known use case.
    fn set_global_dialog_callback(
        cursive: &mut Cursive,
        event: impl Into<Event>,
        mut dialog_factory: impl 'static + FnMut(&mut Cursive) -> Dialog,
    ) {
        cursive.set_global_callback(event, move |cursive| {
            const GLOBAL_DIALOG_NAME: &str = "Global dialog";
            if cursive
                .screen_mut()
                .find_layer_from_name(GLOBAL_DIALOG_NAME)
                .is_none()
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
    set_global_dialog_callback(&mut cursive, 'b', backtrace_dialog);

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
fn show_hierarchical_profile(
    cursive: &mut Cursive,
    parent_name: Box<str>,
    parent_percent_norm: Finite<Duration>,
    activity_infos: Box<[ActivityInfo]>,
) {
    // Compute basic children table layout
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");
    let activity_width = terminal_width as usize - 2 * (DURATION_WIDTH + 3) - 3;

    // Update the TUI state and load required data from it
    let ((sort_order, sort_key), duration_display, activity_descs, mut footer) =
        with_state(cursive, |state| {
            // Reuse the display configuration used by previous profiling layers, or
            // set up the default configuration if this is the first layer
            let mut duration_display = *state.duration_display.get_or_insert_with(|| {
                // Check out the global percentage norm. If it's not been
                // initialized yet, it means we are the first (toplevel)
                // profile, and thus our local norm is the global norm.
                let global_percent_norm =
                    *state.global_percent_norm.get_or_insert(parent_percent_norm);

                // Default to a percentage of the clang execution time
                DurationDisplay::Percentage(global_percent_norm, PercentageReference::Global)
            });

            // In "relative to parent" duration display mode, set the percent
            // normalization factor that is appropriate for the active layer
            if let DurationDisplay::Percentage(ref mut norm, PercentageReference::Parent) =
                &mut duration_display
            {
                *norm = parent_percent_norm;
            }

            // Generate activity descriptions of the right width
            let activity_descs = state.processing_thread.describe_activities(
                activity_infos.iter().map(|info| info.id).collect(),
                activity_width as u16,
            );

            // Register this new layer in the profile stack
            state
                .profile_stack
                .push((parent_name.clone(), parent_percent_norm));

            // Set up the basic trace description footer
            let footer = state.processing_thread.describe_trace(terminal_width);

            // Bubble up useful data for following steps
            (state.sort_config, duration_display, activity_descs, footer)
        });

    // Query the configuration of the previous top layer, if any, or just use a
    // sensible default configuration if we're the first
    let total_duration_col = HierarchicalColumn::Duration(DurationKind::Total, duration_display);

    // Set up the children activity table
    let self_duration_col = HierarchicalColumn::Duration(DurationKind::Myself, duration_display);
    let mut table = HierarchicalView::new()
        .column(
            total_duration_col,
            total_column_name(duration_display),
            |c| c.width(DURATION_WIDTH).ordering(sort_order[0]),
        )
        .column(self_duration_col, SELF_COLUMN_NAME, |c| {
            c.width(DURATION_WIDTH).ordering(sort_order[1])
        })
        .column(HierarchicalColumn::Description, "Activity", |c| {
            c.width(activity_width).ordering(sort_order[2])
        });
    match sort_key {
        HierarchicalColumnName::TotalDuration => {
            table.set_default_column(total_duration_col);
        }
        HierarchicalColumnName::SelfDuration => {
            table.set_default_column(self_duration_col);
        }
        HierarchicalColumnName::Description => {
            table.set_default_column(HierarchicalColumn::Description)
        }
    }

    // Collect children activities into the table
    let items = activity_infos
        .into_vec()
        .into_iter()
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
            .call_on_name(&parent_name2, |view: &mut HierarchicalView| {
                let activity = view
                    .borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index");
                let activity_desc_str: &str = &activity.description;
                let activity_desc = if let Some(desc) = activity_desc_str.strip_prefix('+') {
                    String::from(desc).into()
                } else {
                    activity_desc_str.into()
                };
                (activity.id, activity_desc, activity.duration)
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
                activity_desc,
                percent_norm(activity_duration),
                activity_children,
            )
        }
    });

    // Propagate sort order changes to past and future profile views
    table.set_on_sort(|cursive, column, order| {
        // Update TUI state and extract required info from it
        let column_name = column.name();
        let (past_views, duration_display) = with_state(cursive, |state| {
            // Update sort config for future views
            let (sort_order, sort_key) = &mut state.sort_config;
            sort_order[column_name as usize] = order;
            *sort_key = column_name;

            // Get a list of past views
            let num_past_views = state.profile_stack.len() - 1;
            let past_views = state
                .profile_stack
                .iter()
                .cloned()
                .take(num_past_views)
                .collect::<Vec<_>>();

            // Get the duration display configuration
            let duration_display = state
                .duration_display
                .expect("Duration display configuration should be set by now");

            // Extract useful state
            (past_views, duration_display)
        });

        // Update sort order in past views
        for_each_profile_layer(
            cursive,
            &past_views,
            duration_display,
            |mut table, table_duration_display| {
                // Apply new sort
                let column = column_name.into_column(table_duration_display);
                table.sort_by(column, order);
            },
        );
    });

    // Name the table to allow retrieving it for further edits
    let table = table.with_name(parent_name);

    // TODO: Add F shortcut for flat profile

    // Add help instructions to the trace description footer
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

/// Switch to a different duration unit in all currently displayed profiles
fn switch_duration_unit(cursive: &mut Cursive) {
    // Update TUI state and extract required data from it
    let (new_duration_display, profile_stack, (sort_order, sort_key)) =
        match with_state(cursive, |state| {
            // Determine the next duration display or return None if no profile
            // is being displayed (data is still being loaded)
            let new_duration_display = match state.duration_display? {
                DurationDisplay::Percentage(_, PercentageReference::Global) => {
                    DurationDisplay::Percentage(
                        Finite::<Duration>::from_inner(0.0),
                        PercentageReference::Parent,
                    )
                }
                DurationDisplay::Percentage(_, PercentageReference::Parent) => {
                    DurationDisplay::Time
                }
                DurationDisplay::Time => DurationDisplay::Percentage(
                    state.global_percent_norm.expect(
                        "Global percent norm should be initialized before duration display",
                    ),
                    PercentageReference::Global,
                ),
            };
            state.duration_display = Some(new_duration_display);

            // Also bubble up a copy of the profile stack
            Some((
                new_duration_display,
                state.profile_stack.clone(),
                state.sort_config,
            ))
        }) {
            Some(tuple) => tuple,
            None => return,
        };

    // Update all the profile layers that are currently being displayed
    for_each_profile_layer(
        cursive,
        &profile_stack,
        new_duration_display,
        |mut table, table_duration_display| {
            // Recreate the duration columns accordingly
            table.remove_column(1);
            table.remove_column(0);
            let total_duration_col =
                HierarchicalColumn::Duration(DurationKind::Total, table_duration_display);
            let self_duration_col =
                HierarchicalColumn::Duration(DurationKind::Myself, table_duration_display);
            table.insert_column(
                0,
                total_duration_col,
                total_column_name(table_duration_display),
                |c| c.width(DURATION_WIDTH).ordering(sort_order[0]),
            );
            table.insert_column(1, self_duration_col, SELF_COLUMN_NAME, |c| {
                c.width(DURATION_WIDTH).ordering(sort_order[1])
            });

            // Re-select a new duration column if its former self was selected
            match sort_key {
                HierarchicalColumnName::TotalDuration => {
                    table.set_default_column(total_duration_col);
                }
                HierarchicalColumnName::SelfDuration => {
                    table.set_default_column(self_duration_col);
                }
                _ => {}
            }
        },
    );
}

/// Iterate over profile layers
fn for_each_profile_layer(
    cursive: &mut Cursive,
    layers: &[ProfileLayer],
    mut duration_display: DurationDisplay,
    mut operation: impl FnMut(ViewRef<HierarchicalView>, DurationDisplay),
) {
    for (table_name, parent_percent_norm) in layers {
        // In "relative to parent" duration display mode, set the norm
        // factor that is appropriate for the active layer
        if let DurationDisplay::Percentage(ref mut norm, PercentageReference::Parent) =
            &mut duration_display
        {
            *norm = *parent_percent_norm;
        }

        // Access the target table
        operation(
            cursive
                .find_name::<HierarchicalView>(&table_name)
                .expect("Every registered profile should exist"),
            duration_display,
        );
    }
}

/// Total duration column name associated with a certain duration display configuration
fn total_column_name(duration_display: DurationDisplay) -> &'static str {
    match duration_display {
        DurationDisplay::Percentage(_, PercentageReference::Global) => "%Total",
        DurationDisplay::Percentage(_, PercentageReference::Parent) => "%Parent",
        DurationDisplay::Time => "Duration",
    }
}

/// Interactive backtrace dialog
fn backtrace_dialog(cursive: &mut Cursive) -> Dialog {
    let mut select = SelectView::new();
    with_state(cursive, |state| {
        select.add_all(
            state
                .profile_stack
                .iter()
                .enumerate()
                .rev()
                .map(|(idx, (name, _norm))| (name.clone(), idx)),
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
impl HierarchicalColumn {
    /// Strip column styling information to get a column name
    fn name(&self) -> HierarchicalColumnName {
        match self {
            Self::Duration(DurationKind::Total, _) => HierarchicalColumnName::TotalDuration,
            Self::Duration(DurationKind::Myself, _) => HierarchicalColumnName::SelfDuration,
            Self::Description => HierarchicalColumnName::Description,
        }
    }
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
/// Simplified version of HierarchicalColumn to be used when identifying columns
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum HierarchicalColumnName {
    TotalDuration = 0,
    SelfDuration,
    Description,
}
//
impl HierarchicalColumnName {
    /// Add styling information to get a column name
    fn into_column(self, duration_display: DurationDisplay) -> HierarchicalColumn {
        match self {
            Self::TotalDuration => {
                HierarchicalColumn::Duration(DurationKind::Total, duration_display)
            }
            Self::SelfDuration => {
                HierarchicalColumn::Duration(DurationKind::Myself, duration_display)
            }
            Self::Description => HierarchicalColumn::Description,
        }
    }
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
//
/// TableView using the setup above
type HierarchicalView = TableView<HierarchicalData, HierarchicalColumn>;
