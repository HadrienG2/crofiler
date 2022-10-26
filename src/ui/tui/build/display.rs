//! Display a full-build profile

use crate::{
    build::{
        commands::CompilationDatabase,
        profile::{self, BuildProfile},
    },
    ui::{
        display::path::truncate_path_iter,
        tui::{trace, with_state},
    },
    CliArgs,
};
use cursive::{
    traits::{Nameable, Resizable},
    views::{LinearLayout, TextView},
    CursiveRunnable,
};
use cursive_table_view::{TableView, TableViewItem};
use std::{cmp::Ordering, path::Path};

/// Display the full-build profile, let user explore it and analyze traces
pub fn display_profile(
    cursive: &mut CursiveRunnable,
    args: CliArgs,
    compilation_database: CompilationDatabase,
    clangpp: impl AsRef<str> + 'static,
    profile: BuildProfile,
) {
    // Determine if the profile contains wall-clock time information
    let has_walltime = profile
        .first()
        .map(|unit| unit.wall_time().is_some())
        .unwrap_or(false);

    // Check terminal dimensions
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");

    // Compute table column widths
    const MAX_RSS_WIDTH: usize = 10;
    const WALL_TIME_WIDTH: usize = 8;
    let mut non_file_width = MAX_RSS_WIDTH + 5;
    if has_walltime {
        non_file_width += WALL_TIME_WIDTH + 3;
    }
    let file_width = terminal_width as usize - non_file_width;

    // Set up table display
    type FullBuildProfileView = TableView<profile::Unit, ProfileColumn>;
    let mut table = FullBuildProfileView::new()
        .items(profile)
        .column(ProfileColumn::MaxRSS, "Memory", |c| {
            c.width(MAX_RSS_WIDTH).ordering(Ordering::Greater)
        })
        .default_column(ProfileColumn::MaxRSS);
    if has_walltime {
        table.add_column(ProfileColumn::WallTime, "Time", |c| {
            c.width(WALL_TIME_WIDTH).ordering(Ordering::Greater)
        });
    }
    table.add_column(
        ProfileColumn::RelPath(file_width as u16),
        "Source file",
        |c| c.width(file_width),
    );
    table.sort();
    table.set_selected_row(0);

    // Set up table interaction
    const TABLE_NAME: &str = "<full-build profile>";
    let time_trace_granularity = args.time_trace_granularity;
    table.set_on_submit(move |cursive, _row, index| {
        let rel_path: Box<Path> = cursive
            .call_on_name(TABLE_NAME, |view: &mut FullBuildProfileView| {
                view.borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index")
                    .rel_path()
                    .into()
            })
            .expect("Failed to access full-build profile view");
        trace::measure::show_wizard(
            cursive,
            &compilation_database,
            &rel_path,
            clangpp.as_ref(),
            time_trace_granularity,
        );
    });
    let table = table.with_name(TABLE_NAME);

    // Update UI state
    with_state(cursive, |state| {
        state.showing_full_build = true;
    });

    // Show the profile
    cursive.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(table.min_size((terminal_width, terminal_height - 1)))
            .child(TextView::new("Press H for help.").center()),
    );
    cursive.run();
}

/// Column in the build profile
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum ProfileColumn {
    /// Maximal RSS memory consumption
    MaxRSS,

    /// Elapsed CPU time
    WallTime,

    /// Relative file path (to be shrunk to a specified amount of term columns)
    RelPath(u16),
}
//
impl TableViewItem<ProfileColumn> for profile::Unit {
    fn to_column(&self, column: ProfileColumn) -> String {
        match column {
            ProfileColumn::MaxRSS => format!("{:.2} GB", self.max_rss_bytes() as f32 / 1e9),
            ProfileColumn::WallTime => format!(
                "{:.1}s",
                self.wall_time()
                    .expect("Wall-time should be present if this is probed")
                    .map(|dur| dur.as_secs_f32())
                    .unwrap_or_else(|raw_secs| raw_secs)
            ),
            ProfileColumn::RelPath(cols) => truncate_path_iter(
                self.rel_path()
                    .components()
                    .map(|s| s.as_os_str().to_string_lossy()),
                cols,
            )
            .into(),
        }
    }

    fn cmp(&self, other: &Self, column: ProfileColumn) -> Ordering
    where
        Self: Sized,
    {
        match column {
            ProfileColumn::MaxRSS => self.max_rss_bytes().cmp(&other.max_rss_bytes()),
            ProfileColumn::WallTime => self
                .wall_time()
                .zip(other.wall_time())
                .map(|(x, y)| x.partial_cmp(&y).expect("Failed to compare wall-time"))
                .expect("Wall-time should be present if this is probed"),
            ProfileColumn::RelPath(_cols) => self.rel_path().cmp(other.rel_path()),
        }
    }
}
