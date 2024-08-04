//! Interactive textual user interface

mod build;
mod create;
mod init;
mod names;
mod processing;
mod trace;

use self::{
    processing::ProcessingThread,
    trace::display::{ProfileDisplay, ProfileLayer},
};
use crate::CliArgs;
use clang_time_trace::Duration;
use cursive::{views::Dialog, Cursive};
use decorum::Finite;
use log::{error, LevelFilter};
use std::{
    backtrace::Backtrace,
    fmt::Write,
    panic::{self, AssertUnwindSafe},
};
use syslog::Facility;

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up logging using syslog
    syslog::init(
        Facility::LOG_USER,
        if cfg!(debug_assertions) {
            LevelFilter::Debug
        } else {
            LevelFilter::Info
        },
        None,
    )
    .expect("Failed to initialize syslog");

    // Warn that logs will be emitted on syslog
    eprintln!("Since stderr is not usable inside of a TUI, logs will be emitted on syslog...");

    // Register a panic hook that logs as much info as possible
    enable_panic_logging();

    // Start the processing thread and set up the text user interface
    let mut cursive = init::setup_cursive(State {
        processing_thread: ProcessingThread::start(),
        global_percent_norm: None,
        profile_stack: Vec::new(),
        showing_full_build: false,
        layers_below_profile: 0,
        no_escape: false,
        display_config: Default::default(),
    });

    // Set up the last-chance panic handler and run
    let res = panic::catch_unwind(AssertUnwindSafe(move || {
        if let Some(trace_path) = &args.input {
            trace::load::show_loader(&mut cursive, trace_path);
            cursive.run();
        } else {
            build::profile(&mut cursive, args);
        }
    }));

    // Last-chance panic handler. This runs after the cursive handle is dropped,
    // so hopefully the terminal should be in a correct state and the user
    // should see the message...
    if let Err(e) = res {
        eprintln!("===\n\
                   The TUI crashed due to an unhandled panic.\n\
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

    /// Showing a full-build profile below the trace profiling layers
    showing_full_build: bool,

    /// Number of UI layers below the profile_stack (if any)
    ///
    /// Used to map from cursive.screen.len() to profile_stack indices.
    ///
    layers_below_profile: usize,

    /// Truth that Esc shouldn't just close the current dialog
    ///
    /// This is used for asynchronous processing dialogs where aborting the
    /// process requires fancier logic than the Esc handler can deal with. In
    /// that case, a dedicated Abort button is always provided.
    ///
    no_escape: bool,

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
    let help = if trace::display::is_profiling(cursive) {
        "The first column is the time spent on an activity\n\
        Self is that minus the time spent on callees\n\
        Activity is what clang was doing\n\
        + means that there are callees to zoom on\n\
        \n\
        Available commands:\n\
        - Up/Down selects an activity\n\
        - Tab switches focus between table and description\n\
        - Return zooms on an activity's callees\n\
        - Left/Right + Return adjusts sort\n\
        - Esc exits dialogs and goes up the backtrace\n\
        - U switches between duration units\n\
        - B shows the backtrace to the current activity\n\
        - F toggles between flat and hierarchical profiles\n\
        - Q quits this program\n\
        \n\
        Logs go to syslog to avoid display corruption"
    } else if build::is_profiling(cursive) {
        "Memory is the top RAM consumption (max-RSS)\n\
        Time is the (wall-clock) time spent compiling\n\
        Source file is the affected compilation unit\n\
        \n\
        Available commands:\n\
        - Up/Down selects a compilation unit\n\
        - Return zooms on a compilation unit's profile\n\
        - Left/Right + Return adjusts sort\n\
        - Q quits this program\n\
        \n\
        Logs go to syslog to avoid display corruption"
    } else {
        return None;
    };
    Some(Dialog::info(help))
}

/// Set up a panic hook that logs as much info as possible
fn enable_panic_logging() {
    let default_panic_hook = panic::take_hook();
    panic::set_hook(Box::new(move |panic_info| {
        // Start by telling what happened
        let mut message = String::from("The TUI crashed due to a panic");

        // Specify where, if the info is available
        if let Some(location) = panic_info.location() {
            write!(
                &mut message,
                " at {}:{}:{}",
                location.file(),
                location.line(),
                location.column()
            )
            .expect("Write to String can't fail");
        }

        // If the panic payload is a string message (common case), extract it
        let payload = panic_info.payload();
        let payload_str = payload
            .downcast_ref::<String>()
            .map(|s: &String| -> &str { s })
            .or_else(|| {
                payload
                    .downcast_ref::<&'static str>()
                    .map(|s: &&'static str| -> &str { s })
            });
        if let Some(payload_str) = payload_str {
            write!(&mut message, " with payload: {payload_str}")
                .expect("Write to String can't fail");
        }

        // Log previous message along with a backtrace
        error!("{message}\n{:?}", Backtrace::capture());

        // Leave the rest up to the default panic hook
        default_panic_hook(panic_info);
    }));
}
