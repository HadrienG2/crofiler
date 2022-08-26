//! Interactive textual user interface
//! FIXME: Split this into submodules

mod init;
mod processing;
mod profile;

use self::{
    processing::ProcessingThread,
    profile::{DurationDisplay, HierarchicalColumnName, SortConfig},
};
use crate::ui::tui::profile::ProfileLayer;
use crate::CliArgs;
use clang_time_trace::Duration;
use cursive::{views::Dialog, Cursive};
use decorum::Finite;
use log::LevelFilter;
use std::cmp::Ordering;
use syslog::Facility;

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up logging using syslog
    syslog::init(Facility::LOG_USER, LevelFilter::Info, None).expect("Failed to initialize syslog");

    // Start the processing thread and set up the text user interface
    let mut cursive = init::setup_cursive(State {
        processing_thread: ProcessingThread::start(&args.input),
        global_percent_norm: None,
        profile_stack: Vec::new(),
        sort_config: SortConfig {
            order: [Ordering::Greater, Ordering::Greater, Ordering::Less],
            key: HierarchicalColumnName::TotalDuration,
        },
        duration_display: None,
    });

    // Wait for the clang time trace to finish loading
    if let Err(error) = init::wait_for_input(&mut cursive) {
        cursive.add_layer(
            Dialog::text(format!("Failed to process input: {error}")).button("Quit", Cursive::quit),
        );
        cursive.run();
        return;
    }

    // Query the list of root activities and deduce the global percentage norm
    let (root_activities, global_percent_norm) = with_state(&mut cursive, |state| {
        let root_activities = state.processing_thread.get_root_activities();
        let global_percent_norm = profile::percent_norm(
            root_activities
                .iter()
                .map(|activity| activity.duration)
                .sum::<Duration>(),
        );
        (root_activities, global_percent_norm)
    });

    // TODO: Show activity summary on S

    // Display the hierarchical profile
    profile::show_hierarchical_profile(
        &mut cursive,
        "<profile root>".into(),
        global_percent_norm,
        root_activities,
        |state| state.processing_thread.get_all_activities(),
    );

    // Start the cursive event loop
    cursive.run();
}

/// General UI state available via cursive's user data mechanism
pub struct State {
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
    sort_config: SortConfig,

    /// Current duration display configuration
    ///
    /// Will be set when the first layer of hierarchical profiling is displayed.
    ///
    duration_display: Option<DurationDisplay>,
}

/// Run a closure on the UI state
fn with_state<R>(cursive: &mut Cursive, f: impl FnOnce(&mut State) -> R) -> R {
    cursive
        .with_user_data(f)
        .expect("Failed to access UI state")
}

/// Help dialog
// TODO: Update as the feature set increases
fn help_dialog(_: &mut Cursive) -> Dialog {
    Dialog::info(
        "The first column is the time spent on an activity\n\
        Self is that minus the time spent on callees\n\
        Activity is what clang was doing\n\
        + means that there are callees to zoom on\n\
        \n\
        Available commands:\n\
        - Up/Down selects an activity\n\
        - Return zooms on an activity's callees\n\
        - Left/Right + Return adjusts sort\n\
        - Esc exits dialogs and goes up the backtrace\n\
        - U switches between duration units\n\
        - B shows the backtrace to the current activity\n\
        - F toggles between flat and hierarchical profiles\n\
        - Q quits this program\n\
        \n\
        Logs are sent to syslog to avoid display issues",
    )
}
