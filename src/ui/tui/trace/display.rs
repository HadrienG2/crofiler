//! Interactive UI for loading and displaying trace profiles

use crate::ui::{
    display::duration::display_duration,
    tui::{
        processing::{ActivityInfo, ProcessingThread},
        with_state, State,
    },
};
use clang_time_trace::{ActivityTraceId, Duration};
use cursive::{
    view::{Nameable, Resizable, View},
    views::{Dialog, LinearLayout, OnEventView, TextView, ViewRef},
    Cursive,
};
use cursive_table_view::{TableView, TableViewItem};
use decorum::Finite;
use std::{
    cell::RefCell,
    cmp::Ordering,
    num::NonZeroU32,
    path::Path,
    rc::Rc,
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
};
use unicode_width::UnicodeWidthStr;

/// Load and display a time-trace profile
pub fn show_loader(cursive: &mut Cursive, trace_path: impl AsRef<Path>) {
    // Load the trace, take note of how many other layers we have
    let (wait_state, canceled) = start_wait_for_input(cursive);
    let cb_sink = cursive.cb_sink().clone();
    with_state(cursive, |state| {
        state
            .processing_thread
            .start_load_trace(trace_path, move |result| {
                cb_sink
                    .send(Box::new(move |cursive| {
                        // Check if the load was canceled, if so do nothing
                        if canceled.load(atomic::Ordering::Relaxed) {
                            return;
                        }

                        // Restore cursive state altered by the loading screen
                        end_wait_for_input(cursive, wait_state);

                        // Handle trace loading errors
                        if let Err(error) = result {
                            cursive.add_layer(Dialog::info(format!(
                                "Failed to process input: {error}"
                            )));
                            return;
                        }

                        // Query the list of root activities and deduce the global percentage norm
                        let (root_activities, global_percent_norm) = with_state(cursive, |state| {
                            let root_activities = state.processing_thread.get_root_activities();
                            let global_percent_norm = percent_norm(
                                root_activities
                                    .iter()
                                    .map(|activity| activity.duration)
                                    .sum::<Duration>(),
                            );
                            state.global_percent_norm = Some(global_percent_norm);
                            state.display_config.duration_display = None;
                            (root_activities, global_percent_norm)
                        });

                        // TODO: Show activity summary on S

                        // Display the hierarchical profile
                        show_hierarchical_profile(
                            cursive,
                            "<profile root>".into(),
                            global_percent_norm,
                            root_activities,
                            |state| state.processing_thread.get_all_activities(),
                        );
                    }))
                    .expect("Failed to send callback to main thread");
            });
    });
}

/// Truth that profiling is in progress
pub fn is_profiling(cursive: &mut Cursive) -> bool {
    with_state(cursive, |state| !state.profile_stack.is_empty())
}

/// Switch to a different duration unit in all currently displayed profiles
pub fn switch_duration_unit(cursive: &mut Cursive) {
    // This does nothing when we're not doing profiling
    if !is_profiling(cursive) {
        return;
    }

    // Update TUI state and extract required data from it, or just return if no
    // profile is being displayed yet.
    let (new_duration_display, profile_stack, sort_config) = match with_state(cursive, |state| {
        // Determine the next duration display or return None if no profile
        // is being displayed (it means clang data is still being loaded)
        let new_duration_display = match state.display_config.duration_display? {
            DurationDisplay::Percentage(_, PercentageReference::Global) => {
                DurationDisplay::Percentage(
                    Finite::<Duration>::from_inner(0.0),
                    PercentageReference::Parent,
                )
            }
            DurationDisplay::Percentage(_, PercentageReference::Parent) => DurationDisplay::Time,
            DurationDisplay::Time => DurationDisplay::Percentage(
                state
                    .global_percent_norm
                    .expect("Global percent norm should be initialized before duration display is"),
                PercentageReference::Global,
            ),
        };
        state.display_config.duration_display = Some(new_duration_display);

        // Also bubble up a copy of the profile stack and sort configuration
        Some((
            new_duration_display,
            state.profile_stack.clone(),
            state.display_config.sort_config,
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
        |layer, mut table, table_duration_display| {
            // Recreate the duration columns with the new configuration
            match layer.kind {
                ProfileKind::Hierarchical => table.remove_column(1),
                ProfileKind::Flat => {}
            }
            table.remove_column(0);
            add_duration_cols_and_sort(
                &mut table,
                layer.kind,
                ProfileDisplay {
                    sort_config,
                    duration_display: Some(table_duration_display),
                },
            );
        },
    );
}

/// Information about a layer of the cursive TUI stack that contains a profile
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProfileLayer {
    /// Name of the profile data TableView = name of the parent entity
    table_name: Rc<str>,

    /// Norm used to compute percentages with respect to this profile's parent.
    parent_percent_norm: Finite<Duration>,

    /// What kind of profile this is
    kind: ProfileKind,
}
//
impl ProfileLayer {
    /// Name of the profile data TableView = name of the parent entity
    pub fn table_name(&self) -> &str {
        &self.table_name
    }
}
//
/// Kind of profile that this app can display
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ProfileKind {
    /// Hierarchical profile allow drilling down on the tasks that are
    /// transitively spawned by each task, following parent -> child relations
    Hierarchical,

    /// Flat profiles go across the entire set of tasks transitively spawned by
    /// a task and tell what the time contribution of each is.
    Flat,
}

/// Information about the TUI's current display configuration
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ProfileDisplay {
    /// Current column sorting configuration
    sort_config: SortConfig,

    /// Current duration display configuration
    duration_display: Option<DurationDisplay>,
}

/// Start displaying a pretty loading screen
///
/// Returns a WaitForInputState token that should be passed back to
/// end_wait_for_input at the end of the loading process along with an
/// AtomicBool that tells whether the user has canceled the loading process.
///
fn start_wait_for_input(cursive: &mut Cursive) -> (WaitForInputState, Arc<AtomicBool>) {
    // Set up loading screen state
    let layers_below_profile = cursive.screen().len();
    with_state(cursive, |state| {
        state.layers_below_profile = layers_below_profile;
        state.no_escape = true;
    });

    // Make sure Cursive promptly takes notice of the completion callback
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);
    let state = WaitForInputState { old_fps };

    // Set up a cancelation mechanism for this loading screen
    // We shouldn't abuse state.no_escape for this because then the following
    // race condition can occur:
    //
    // - User starts loading a trace
    // - User change their mind and cancel the load
    // - User starts loading another trace
    // - First load completes and displays on screen instead of being dropped
    //
    let canceled = Arc::new(AtomicBool::new(false));
    let canceled2 = canceled.clone();

    // Set up the loading screen and its cancelation mechanism
    //
    // Cancelation does not kill the processing thread, because that would
    // leak all system resources it's holding. Instead, a new processing thread
    // is started, and the old processing thread is left to terminate
    // gracefully in the background. This will admittedly waste a few seconds of
    // CPU time, but the alternative of making the trace loading process
    // interruptible would be much more complex at the code level.
    //
    cursive.add_layer(
        Dialog::text("Processing time trace...").button("Abort", move |cursive| {
            canceled2.store(true, atomic::Ordering::Relaxed);
            end_wait_for_input(cursive, state);
            if cursive.screen().is_empty() {
                cursive.quit();
            } else {
                with_state(cursive, |state| {
                    state.processing_thread = ProcessingThread::start();
                });
            }
        }),
    );

    // Return saved Cursive state
    (state, canceled)
}

/// Saved Cursive state returned by start_wait_for_input, to be passed back to
/// end_wait_for_input for a return to the original Cursive state.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct WaitForInputState {
    /// Cursive FPS setting at the beginning of the wait
    old_fps: Option<NonZeroU32>,
}

/// Callback to clean up after start_wait_for_input
///
/// This is to be called after the ClangTrace has been loaded by the processing
/// thread, taking as a parameter the saved Cursive state that was emitted by
/// start_wait_for_input. It will return to the Cursive state before
/// start_wait_for_input was called.
///
fn end_wait_for_input(cursive: &mut Cursive, state: WaitForInputState) {
    let old_layers = with_state(cursive, |state| {
        state.no_escape = false;
        state.layers_below_profile
    });
    cursive.set_fps(state.old_fps.map(u32::from).unwrap_or(0));
    while cursive.screen().len() > old_layers {
        cursive.pop_layer();
    }
}

/// Display a hierarchical profile
fn show_hierarchical_profile(
    cursive: &mut Cursive,
    table_name: Rc<str>,
    parent_percent_norm: Finite<Duration>,
    activity_infos: Box<[ActivityInfo]>,
    get_flat_activities: impl 'static + FnOnce(&mut State) -> Box<[ActivityInfo]>,
) {
    show_profile(
        ProfileKind::Hierarchical,
        cursive,
        table_name,
        parent_percent_norm,
        activity_infos,
        Box::new(get_flat_activities),
    );
}

/// Compute the percentage norm associated with a set of activities
fn percent_norm(total_duration: Duration) -> Finite<Duration> {
    Finite::<Duration>::from_inner(100.0 / total_duration)
}

/// Display a flat or hierarchical profile
fn show_profile(
    kind: ProfileKind,
    cursive: &mut Cursive,
    table_name: Rc<str>,
    parent_percent_norm: Finite<Duration>,
    activity_infos: Box<[ActivityInfo]>,
    get_other_activities: Box<dyn 'static + FnOnce(&mut State) -> Box<[ActivityInfo]>>,
) {
    // Check terminal dimensions
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");

    // All kinds of profile have at least a Self column and a scrollbar, which
    // eats up some space that is not available for the activity description.
    let mut non_description_width = DURATION_WIDTH + 5;
    non_description_width += match kind {
        // Hierarchical profiles additionally have a total duration column and
        // '+' indicators telling which activities can be zoomed.
        ProfileKind::Hierarchical => DURATION_WIDTH + 4,
        ProfileKind::Flat => 0,
    };
    let description_width = terminal_width as usize - non_description_width;

    // Update the TUI state and load required data from it
    let (display_config, activity_descs, mut footer) = register_profile(
        cursive,
        terminal_width,
        ProfileLayer {
            table_name: table_name.clone(),
            parent_percent_norm,
            kind,
        },
        &activity_infos[..],
        description_width as u16,
    );

    // Set up the profile view
    let table = make_profile_view(
        table_name,
        kind,
        activity_infos,
        get_other_activities,
        activity_descs,
        description_width,
        display_config,
    );

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

    // Show the profile
    cursive.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(table.min_size((terminal_width, terminal_height - footer_lines)))
            .child(TextView::new(footer)),
    );
}

/// Register a new profile in the TUI state and return the information needed to
/// display that profile in the TUI
///
/// Returns the current sorting and duration display configuration, a
/// description for all activities to be displayed, and a description of the
/// overall dataset that is contrained into available horizontal space.
///
fn register_profile(
    cursive: &mut Cursive,
    terminal_width: u16,
    layer: ProfileLayer,
    activity_infos: &[ActivityInfo],
    description_width: u16,
) -> (ProfileDisplay, Box<[Arc<str>]>, String) {
    with_state(cursive, |state| {
        // Reuse the display configuration used by previous profiling layers, or
        // set up the default configuration if this is the first layer
        let duration_display = state
            .display_config
            .duration_display
            .get_or_insert_with(|| {
                // Check out the global percentage norm
                let global_percent_norm = state
                    .global_percent_norm
                    .expect("Global percent norm should be set at this point");

                // Default to a percentage of the clang execution time
                DurationDisplay::Percentage(global_percent_norm, PercentageReference::Global)
            });

        // In "relative to parent" duration display mode, set the percent
        // normalization factor that is appropriate for the active layer
        if let DurationDisplay::Percentage(ref mut norm, PercentageReference::Parent) =
            duration_display
        {
            *norm = layer.parent_percent_norm;
        }

        // Generate activity descriptions of the right width
        let activity_descs = state.processing_thread.describe_activities(
            activity_infos.iter().map(|info| info.id).collect(),
            description_width,
        );

        // Register this new layer in the profile stack
        state.profile_stack.push(layer);

        // Set up the basic trace description footer
        let footer = state.processing_thread.describe_trace(terminal_width);

        // Bubble up useful data for following steps
        (state.display_config, activity_descs, footer)
    })
}

/// Set up the tabular view that is the heart of a profile
fn make_profile_view(
    table_name: Rc<str>,
    kind: ProfileKind,
    activity_infos: Box<[ActivityInfo]>,
    get_other_activities: Box<dyn 'static + FnOnce(&mut State) -> Box<[ActivityInfo]>>,
    activity_descs: Box<[Arc<str>]>,
    description_width: usize,
    display_config: ProfileDisplay,
) -> impl View {
    // Prepare the tabular data
    let items = activity_infos
        .iter()
        .zip(activity_descs.into_vec().into_iter())
        .map(|(activity_info, description)| {
            let description = match kind {
                // Hierarchical profiles have little "+" indicators that tell
                // which activities have children and can be zoomed
                ProfileKind::Hierarchical => {
                    let mut buf = String::new();
                    if activity_info.has_children {
                        buf.push('+');
                    } else {
                        buf.push(' ');
                    }
                    buf.push_str(&description);
                    buf.into_boxed_str()
                }

                // Flat profiles just display the raw activity description
                ProfileKind::Flat => {
                    let description_str: &str = &description;
                    description_str.into()
                }
            };
            HierarchicalData {
                id: activity_info.id,
                duration: activity_info.duration,
                self_duration: activity_info.self_duration,
                description,
            }
        })
        .collect();

    // Set up the children activity table
    let mut table = ProfileView::new()
        .items(items)
        .column(HierarchicalColumn::Description, "Activity", |c| {
            c.width(description_width)
                .ordering(display_config.sort_config.order[2])
        })
        .on_sort(sort_other_profiles);
    add_duration_cols_and_sort(&mut table, kind, display_config);
    table.set_selected_row(0);

    // Let user zoom in on child activities in hierarchical profiles
    match kind {
        ProfileKind::Hierarchical => table.set_on_submit(zoom(table_name.clone())),
        ProfileKind::Flat => {}
    }

    // Name the table to allow accessing it later on
    let table_name_str: &str = &table_name;
    let table = table.with_name(String::from(table_name_str));

    // Let F shortcut toggle between hierarchical and flat profile
    let once_state = RefCell::new(Some((activity_infos, get_other_activities)));
    OnEventView::new(table).on_event('f', move |cursive| {
        let kind = match kind {
            ProfileKind::Hierarchical => ProfileKind::Flat,
            ProfileKind::Flat => ProfileKind::Hierarchical,
        };
        let (old_activity_infos, get_other_activities) = once_state
            .borrow_mut()
            .take()
            .expect("This callback may only be called once, after that the view is destroyed");
        let (parent_percent_norm, new_activity_infos) = with_state(cursive, |state| {
            let layer = state
                .profile_stack
                .pop()
                .expect("There should be a profile if this shortcut works");
            (layer.parent_percent_norm, get_other_activities(state))
        });
        cursive.pop_layer();
        show_profile(
            kind,
            cursive,
            table_name.clone(),
            parent_percent_norm,
            new_activity_infos,
            Box::new(|_state| old_activity_infos),
        )
    })
}

/// on_submit callback for hierarchical profiles that recursively spawns another
/// hierarchical profile lookint at the selected activity's children
fn zoom(table_name: Rc<str>) -> impl Fn(&mut Cursive, usize, usize) + 'static {
    move |cursive, _row, index| {
        // Access the hierarchical profile's table to check the selected
        // activity and whether it has children / can be zoomed on.
        let (activity_trace_id, stripped_description, activity_duration) = match cursive
            .call_on_name(&table_name, |view: &mut ProfileView| {
                // Access the activity's HierarchicalData
                let activity = view
                    .borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index");

                // The activity description should start with a '+', that we
                // will want to strip. If it does not start with a '+', then
                // this activity has no children and cannot be zoomed on.
                let stripped_description = activity.description.strip_prefix('+')?;
                Some((activity.id, stripped_description.into(), activity.duration))
            })
            .expect("Failed to access trace profile view")
        {
            Some(tuple) => tuple,
            None => return,
        };

        // Query the activity's direct children
        let activity_children = with_state(cursive, |state| {
            state
                .processing_thread
                .get_direct_children(activity_trace_id)
        });
        assert!(
            !activity_children.is_empty(),
            "If the activity had no children, early exit should have occured"
        );

        // Show a hierarchical profile of this activity
        show_hierarchical_profile(
            cursive,
            stripped_description,
            percent_norm(activity_duration),
            activity_children,
            Box::new(move |state: &mut State| {
                state.processing_thread.get_all_children(activity_trace_id)
            }),
        )
    }
}

/// on_sort callback for profiles that propagates the sorting configuration
/// change to all past and future profiles
fn sort_other_profiles(cursive: &mut Cursive, column: HierarchicalColumn, order: Ordering) {
    // Update TUI state and extract required info from it
    let mut column_name = column.name();
    let (past_views, duration_display) = with_state(cursive, |state| {
        // Update sort config for future views
        state.display_config.sort_config.order[column_name as usize] = order;
        state.display_config.sort_config.key = column_name;

        // Get a list of past views
        let num_past_views = state.profile_stack.len() - 1;
        let past_views = state
            .profile_stack
            .iter()
            .take(num_past_views)
            .cloned()
            .collect::<Vec<_>>();

        // Get the duration display configuration
        let duration_display = state
            .display_config
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
        |layer, mut table, table_duration_display| {
            // Apply new sort
            match layer.kind {
                ProfileKind::Hierarchical => {}
                ProfileKind::Flat => column_name = column_name.into_flat(),
            }
            let column = column_name.into_column(table_duration_display);
            table.sort_by(column, order);
        },
    );
}

/// Iterate over profile layers and their display configurations
fn for_each_profile_layer(
    cursive: &mut Cursive,
    layers: &[ProfileLayer],
    mut duration_display: DurationDisplay,
    mut operation: impl FnMut(&ProfileLayer, ViewRef<ProfileView>, DurationDisplay),
) {
    for profile_layer in layers {
        // In "relative to parent" duration display mode, set the norm
        // factor that is appropriate for the active layer
        if let DurationDisplay::Percentage(ref mut norm, PercentageReference::Parent) =
            &mut duration_display
        {
            *norm = profile_layer.parent_percent_norm;
        }

        // Access the target table
        operation(
            profile_layer,
            cursive
                .find_name::<ProfileView>(&profile_layer.table_name)
                .expect("Every registered profile should exist"),
            duration_display,
        );
    }
}

/// Insert duration columns in a table as appropriate for a given profile kind
/// (assuming they were not present to begin with) then apply the requested
/// sorting key with proper adjustments for that profile type.
fn add_duration_cols_and_sort(
    table: &mut ProfileView,
    kind: ProfileKind,
    display_config: ProfileDisplay,
) {
    let mut sort_config = display_config.sort_config;
    let duration_display = display_config.duration_display.expect(
        "Duration display configuration should have been set \
         before creating/modifying table duration columns",
    );
    match kind {
        ProfileKind::Hierarchical => {
            // Add duration columns appropriate for hierarchical profiles
            table.insert_column(
                0,
                HierarchicalColumn::Duration(DurationKind::Total, duration_display),
                total_column_name(duration_display),
                |c| c.width(DURATION_WIDTH).ordering(sort_config.order[0]),
            );
            table.insert_column(
                1,
                HierarchicalColumn::Duration(DurationKind::Myself, duration_display),
                HIERARCHICAL_SELF_COLUMN_NAME,
                |c| c.width(DURATION_WIDTH).ordering(sort_config.order[1]),
            );
        }

        ProfileKind::Flat => {
            // Add duration columns appropriate for flat profiles
            table.insert_column(
                0,
                HierarchicalColumn::Duration(DurationKind::Myself, duration_display),
                flat_self_column_name(duration_display),
                |c| c.width(DURATION_WIDTH).ordering(sort_config.order[1]),
            );

            // Constrain sort key to flat column set in flat profiles
            sort_config.key = sort_config.key.into_flat();
        }
    }
    table.set_default_column(sort_config.key.into_column(duration_display));
    table.sort();
}

/// Hierarchical profile column
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
/// Hierarchical profile column identifier
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum HierarchicalColumnName {
    /// How much time was spent on an activity, overall
    TotalDuration = 0,

    /// How much time was spent on this activity specifically, excluding
    /// downstream activities
    SelfDuration,

    /// Description of the activity
    Description,
}
//
impl HierarchicalColumnName {
    /// Constrain this column name into the flat profile column name set
    fn into_flat(self) -> Self {
        match self {
            HierarchicalColumnName::TotalDuration => HierarchicalColumnName::SelfDuration,
            other => other,
        }
    }

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

/// TableView using the setup above
type ProfileView = TableView<HierarchicalData, HierarchicalColumn>;

/// Width of duration columns
const DURATION_WIDTH: usize = 11;

/// Name of the self-duration column
const HIERARCHICAL_SELF_COLUMN_NAME: &str = "Self";

/// Total duration column name associated with a certain duration display
/// configuration, for hierarchical profiles
fn total_column_name(duration_display: DurationDisplay) -> &'static str {
    match duration_display {
        DurationDisplay::Percentage(_, PercentageReference::Global) => "%Total",
        DurationDisplay::Percentage(_, PercentageReference::Parent) => "%Parent",
        DurationDisplay::Time => "Time",
    }
}

/// Self duration column name associated with a certain duration display
/// configuration, for flat profiles
fn flat_self_column_name(duration_display: DurationDisplay) -> &'static str {
    match duration_display {
        DurationDisplay::Percentage(_, PercentageReference::Global) => "Self%Tot",
        DurationDisplay::Percentage(_, PercentageReference::Parent) => "Self%Par",
        DurationDisplay::Time => "SelfTime",
    }
}

/// Information about the TUI's current profile sorting configuration
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SortConfig {
    /// Ordering to be used for each column of a hierarchical profile
    order: [Ordering; 3],

    /// Active sorting column (default column)
    key: HierarchicalColumnName,
}
//
impl Default for SortConfig {
    fn default() -> Self {
        Self {
            order: [Ordering::Greater, Ordering::Greater, Ordering::Less],
            key: HierarchicalColumnName::TotalDuration,
        }
    }
}