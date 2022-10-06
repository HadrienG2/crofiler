//! Interactive textual user interface

mod build;
mod init;
mod processing;
mod trace;

use self::{
    processing::ProcessingThread,
    trace::display::{ProfileDisplay, ProfileLayer},
};
use crate::{build::commands::ProductFreshness, CliArgs};
use clang_time_trace::Duration;
use cursive::{views::Dialog, Cursive, CursiveRunnable};
use decorum::Finite;
use log::{error, LevelFilter};
use std::{
    cell::Cell,
    fmt::Write,
    panic::{self, AssertUnwindSafe, PanicInfo},
    rc::Rc,
};
use syslog::Facility;

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up logging using syslog
    syslog::init(Facility::LOG_USER, LevelFilter::Info, None).expect("Failed to initialize syslog");

    // Warn that logs will be emitted on syslog
    eprintln!("Since stderr is not usable inside of a TUI, logs will be emitted on syslog...");

    // Start the processing thread and set up the text user interface
    let mut cursive = init::setup_cursive(State {
        processing_thread: ProcessingThread::start(),
        global_percent_norm: None,
        profile_stack: Vec::new(),
        showing_full_build: false,
        layers_below_profile: 0,
        loading_trace: false,
        display_config: Default::default(),
    });

    // Register a panic hook that logs as much info as possible
    let default_panic_hook = panic::take_hook();
    panic::set_hook(Box::new(move |panic_info: &PanicInfo| {
        // Spell out what happened, and hopefully where
        let mut message = String::from("The TUI crashed due to a panic");
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

        // If the panic payload is a message (common case), extract it
        let payload = panic_info.payload();
        let payload_str = payload
            .downcast_ref::<String>()
            .map(|s: &String| -> &str { &s })
            .or_else(|| {
                payload
                    .downcast_ref::<&'static str>()
                    .map(|s| -> &str { &s })
            });

        // Log what we know
        if let Some(payload_str) = payload_str {
            error!("{message} with payload: {payload_str}");
        } else {
            error!("{message}");
        }

        // Leave the rest up to the default panic hook
        default_panic_hook(panic_info);
    }));

    // Set up the last-chance panic handler and run
    let res = panic::catch_unwind(AssertUnwindSafe(move || {
        if let Some(trace_path) = &args.input {
            trace::display::show_loader(&mut cursive, trace_path);
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
    let help = if trace::display::is_profiling(cursive) {
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

/// Prompt to be shown when a new profile or trace may need to be created
struct CreatePrompt {
    /// Question to be asked to the user
    question: String,

    /// Default reply
    default_reply: &'static str,

    /// Other reply
    other_reply: &'static str,

    /// Default reply means that a new profile should be created
    default_means_create: bool,
}
//
impl CreatePrompt {
    /// Given a profiling product's freshness, emit a (re)creation prompt if needed
    ///
    /// - manually_specified indicates that a profile was manually specified by
    ///   the user, which disabled "maybe stale" dialogs.
    /// - full_build indicates that this is about a full-build profile, as
    ///   opposed to a trace from a single compilation unit.
    ///
    pub fn from_freshness(
        freshness: ProductFreshness,
        manually_specified: bool,
        full_build: bool,
    ) -> Option<Self> {
        let mut result = match freshness {
            // No profile at the expected location
            ProductFreshness::Nonexistent => Some(CreatePrompt::new_build_profile()),

            // There is a profile but it is obviously stale
            ProductFreshness::Outdated => Some(CreatePrompt::stale_build_profile()),

            // There is a profile, and it does not look obviously stale, but
            // might still be because we are not omniscient. Prompt if it's older
            // than one minute and we're using the default path (not manual choice)
            ProductFreshness::MaybeOutdated(age) => {
                let age_mins = age.map(|d| d.as_secs() / 60).unwrap_or(u64::MAX);
                if age_mins > 0 && !manually_specified {
                    Some(CreatePrompt::maybe_stale_build_profile(Some(age_mins)))
                } else {
                    None
                }
            }
        }?;
        if full_build {
            result.question.push_str(Self::BUILD_PROFILE_TRAILER);
        }
        Some(result)
    }

    /// Ask whether a new profile should be created
    pub fn show(
        self,
        cursive: &mut Cursive,
        handle_reply: impl FnOnce(&mut Cursive, bool) + 'static,
    ) {
        let handle_reply = Cell::new(Some(handle_reply));
        let handle_reply = Rc::new(move |cursive: &mut Cursive, should_create| {
            cursive.pop_layer();
            let handle_reply = handle_reply.take().expect("This can only be called once");
            handle_reply(cursive, should_create)
        });
        let handle_reply_2 = handle_reply.clone();
        cursive.add_layer(
            Dialog::text(self.question)
                .button(self.default_reply, move |cursive| {
                    handle_reply(cursive, self.default_means_create)
                })
                .button(self.other_reply, move |cursive| {
                    handle_reply_2(cursive, !self.default_means_create)
                }),
        );
    }

    /// Same, but synchronously (requires CursiveRunnable)
    pub fn ask(self, cursive: &mut CursiveRunnable) -> bool {
        let should_create = Rc::new(Cell::default());
        let should_create_2 = should_create.clone();
        self.show(cursive, move |cursive, should_create| {
            should_create_2.set(should_create);
            cursive.quit();
        });
        cursive.run();
        should_create.get()
    }

    /// Build profile cration prompt when there is no existing build profile
    fn new_build_profile() -> Self {
        Self {
            question: "It looks like this build has not been profiled yet. \
                Ready to do so?"
                .to_owned(),
            default_reply: "Yes",
            other_reply: "No",
            default_means_create: true,
        }
    }

    /// Build profile cration prompt when the build profile is stale
    fn stale_build_profile() -> Self {
        Self::stale_build_profile_impl(
            "There is an existing build profile, but it is not up to date \
            with respect to current source code. Use it anyway?"
                .to_owned(),
        )
    }

    /// Build profile cration prompt when the build profile might be stale
    ///
    /// If the cpp files changed, we know that the profile is stale, but if
    /// other build dependencies like headers changed, we don't know about it
    /// because CMake won't tell us about those.
    ///
    /// Given that measuring a build profile can take more than an hour, it's
    /// best in any case to ask before overwriting the existing profile, which
    /// may still be good enough.
    ///
    fn maybe_stale_build_profile(age_mins: Option<u64>) -> Self {
        let mut question = "There is an existing build profile".to_owned();
        if let Some(age_mins) = age_mins {
            question.push_str(" from ");
            let age_hours = age_mins / 60;
            let age_days = age_hours / 24;
            if age_days > 0 {
                write!(question, "{age_days} day").expect("Write to String can't fail");
                if age_days > 1 {
                    write!(question, "s").expect("Write to String can't fail");
                }
            } else if age_hours > 0 {
                write!(question, "{age_hours}h").expect("Write to String can't fail");
            } else {
                write!(question, "{age_mins}min").expect("Write to String can't fail");
            }
            question.push_str(" ago");
        }
        question.push_str(". Do you consider it up to date?");
        Self::stale_build_profile_impl(question)
    }

    /// Commonalities between all stale_build_profile functions
    fn stale_build_profile_impl(question: String) -> Self {
        Self {
            question,
            default_reply: "Reuse",
            other_reply: "Measure",
            default_means_create: false,
        }
    }

    /// Common trailer for all build profile creation questions
    const BUILD_PROFILE_TRAILER: &'static str =
        "\n(Measuring a build profile requires a full single-core build, \
            during which you should minimize other system activity)";
}
