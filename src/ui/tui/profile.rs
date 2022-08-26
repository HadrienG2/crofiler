//! Profiling dialogs and related functionality

use super::processing::ActivityInfo;
use crate::ui::display::duration::display_duration;
use clang_time_trace::{ActivityTraceId, Duration};
use cursive::{
    view::{Nameable, Resizable},
    views::{LinearLayout, TextView, ViewRef},
    Cursive,
};
use cursive_table_view::{TableView, TableViewItem};
use decorum::Finite;
use std::{cmp::Ordering, rc::Rc};
use unicode_width::UnicodeWidthStr;

/// Information about a layer of the cursive TUI stack that contains a profile
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProfileLayer {
    /// Name of the profile data TableView = name of the parent entity
    pub table_name: Rc<str>,

    /// Norm used to compute percentages with respect to this profile's parent.
    pub parent_percent_norm: Finite<Duration>,
}

/// Display a hierarchical profile
pub fn show_hierarchical_profile(
    cursive: &mut Cursive,
    table_name: Rc<str>,
    parent_percent_norm: Finite<Duration>,
    activity_infos: Box<[ActivityInfo]>,
) {
    // Compute basic children table layout
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");
    // FIXME: Adapt for flat profiles
    let activity_width = terminal_width as usize - 2 * (DURATION_WIDTH + 3) - 3;

    // Update the TUI state and load required data from it
    let ((sort_order, sort_key), duration_display, activity_descs, mut footer) =
        super::with_state(cursive, |state| {
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
            state.profile_stack.push(ProfileLayer {
                table_name: table_name.clone(),
                parent_percent_norm,
            });

            // Set up the basic trace description footer
            let footer = state.processing_thread.describe_trace(terminal_width);

            // Bubble up useful data for following steps
            (state.sort_config, duration_display, activity_descs, footer)
        });

    // Set up the children activity table
    // FIXME: Adapt for flat profiles
    let total_duration_col = HierarchicalColumn::Duration(DurationKind::Total, duration_display);
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
                // FIXME: Adapt for flat profiles
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

    // Recurse into an activity's children when asked to do so
    table.set_on_submit(zoom(table_name.clone()));

    // Propagate sort order changes to past and future profile views
    table.set_on_sort(sort_other_profiles);

    // Name the table to allow accessing it later on
    let table_name_str: &str = &table_name;
    let table = table.with_name(String::from(table_name_str));

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
pub fn switch_duration_unit(cursive: &mut Cursive) {
    // Update TUI state and extract required data from it, or just return if no
    // profile is being displayed yet.
    let (new_duration_display, profile_stack, (sort_order, sort_key)) =
        match super::with_state(cursive, |state| {
            // Determine the next duration display or return None if no profile
            // is being displayed (it means clang data is still being loaded)
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
                        "Global percent norm should be initialized before duration display is",
                    ),
                    PercentageReference::Global,
                ),
            };
            state.duration_display = Some(new_duration_display);

            // Also bubble up a copy of the profile stack and sort configuration
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
            // FIXME: Adapt for flat profiles
            // Recreate the duration columns with the new configuration
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

/// on_submit callback for hierarchical profiles that recursively spawns another
/// hierarchical profile lookint at the selected activity's children
fn zoom(table_name: Rc<str>) -> impl Fn(&mut Cursive, usize, usize) + 'static {
    move |cursive, _row, index| {
        // Access the hierarchical profile's table to check the selected
        // activity and whether it has children / can be zoomed on.
        let (activity_trace_id, entity_name, activity_duration) = match cursive
            .call_on_name(&table_name, |view: &mut HierarchicalView| {
                // Access the activity's HierarchicalData
                let activity = view
                    .borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index");

                // The activity description should start with a '+', that we
                // will want to strip. If it does not start with a '+', then
                // this activity has no children and cannot be zoomed on.
                let entity_name = activity.description.strip_prefix('+')?;
                Some((activity.id, entity_name.into(), activity.duration))
            })
            .expect("Failed to retrieve cursive layer")
        {
            Some(tuple) => tuple,
            None => return,
        };

        // Query the activity's direct children
        let activity_children = super::with_state(cursive, |state| {
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
            entity_name,
            percent_norm(activity_duration),
            activity_children,
        )
    }
}

/// on_sort callback for profiles that propagates the sorting configuration
/// change to all past and future profiles
fn sort_other_profiles(cursive: &mut Cursive, column: HierarchicalColumn, order: Ordering) {
    // Update TUI state and extract required info from it
    let column_name = column.name();
    let (past_views, duration_display) = super::with_state(cursive, |state| {
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
            // FIXME: Adapt for flat profiles
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
    mut operation: impl FnMut(ViewRef<HierarchicalView>, DurationDisplay),
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
            cursive
                .find_name::<HierarchicalView>(&profile_layer.table_name)
                .expect("Every registered profile should exist"),
            duration_display,
        );
    }
}

/// Compute the percentage norm associated with a set of activities
pub fn percent_norm(total_duration: Duration) -> Finite<Duration> {
    Finite::<Duration>::from_inner(100.0 / total_duration)
}

/// Width of duration columns
const DURATION_WIDTH: usize = 12;

/// Name of the self-duration column
const SELF_COLUMN_NAME: &str = "Self";

/// Total duration column name associated with a certain duration display configuration
fn total_column_name(duration_display: DurationDisplay) -> &'static str {
    match duration_display {
        DurationDisplay::Percentage(_, PercentageReference::Global) => "%Total",
        DurationDisplay::Percentage(_, PercentageReference::Parent) => "%Parent",
        DurationDisplay::Time => "Duration",
    }
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
pub enum DurationDisplay {
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
pub enum PercentageReference {
    /// Total time spent running clang
    Global,

    /// Time spent running the parent activity
    Parent,
}
//
/// Hierarchical profile column identifier
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum HierarchicalColumnName {
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
type HierarchicalView = TableView<HierarchicalData, HierarchicalColumn>;
