//! Interactive textual user interface

mod build;
mod init;
mod processing;
mod trace;

use self::{processing::ProcessingThread, trace::ProfileDisplay};
use crate::ui::tui::trace::ProfileLayer;
use crate::CliArgs;
use clang_time_trace::Duration;
use cursive::{views::Dialog, Cursive};
use decorum::Finite;
use log::LevelFilter;
use std::panic::{self, AssertUnwindSafe};
use syslog::Facility;

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up logging using syslog
    syslog::init(Facility::LOG_USER, LevelFilter::Info, None).expect("Failed to initialize syslog");

    // Start the processing thread and set up the text user interface
    let mut cursive = init::setup_cursive(State {
        processing_thread: ProcessingThread::start(),
        global_percent_norm: None,
        profile_stack: Vec::new(),
        layers_below_profile: 0,
        loading_trace: false,
        display_config: Default::default(),
    });

    // Set up the last-chance panic handler
    let res = panic::catch_unwind(AssertUnwindSafe(move || {
        if let Some(trace_path) = &args.input {
            trace::profile(&mut cursive, trace_path);
        } else {
            build::profile(&mut cursive, args);
        }
    }));

    // Last-chance panic handler. This runs after the cursive handle is dropped,
    // so hopefully the terminal should be in a correct state and the user
    // should see the message...
    if let Err(e) = res {
        eprintln!("The TUI crashed due to an unhandled panic.\n\
                   This is a bug, please report it at https://github.com/HadrienG2/crofiler/issues!\n\
                   The system logs may contain more information about what happened.");
        panic::resume_unwind(e);
    }
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

    /// Number of UI layers below the profile_stack
    layers_below_profile: usize,

    /// Truth that trace data is being loaded
    loading_trace: bool,

    /// Current profile display configuration
    display_config: ProfileDisplay,
}

/// Run a closure on the UI state
fn with_state<R>(cursive: &mut Cursive, f: impl FnOnce(&mut State) -> R) -> R {
    cursive
        .with_user_data(f)
        .expect("Failed to access UI state")
}

/// Help dialog
// TODO: Update as the feature set increases
fn help_dialog(cursive: &mut Cursive) -> Option<Dialog> {
    if !trace::is_profiling(cursive) {
        return None;
    }
    Some(Dialog::info(
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
        Logs go to syslog to avoid display corruption",
    ))
}
