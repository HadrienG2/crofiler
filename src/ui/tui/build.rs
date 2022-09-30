//! Full-build profiling interface

use crate::{
    build::{
        self,
        commands::{CompilationDatabase, DatabaseLoadError},
        profile::{
            self,
            cmakeperf::{self, CollectStep},
        },
    },
    CliArgs,
};
use cursive::{
    views::{Dialog, LinearLayout, ProgressBar, TextView},
    Cursive, CursiveRunnable,
};
use std::{
    cell::Cell,
    fmt::Write,
    io,
    path::Path,
    rc::Rc,
    sync::{Arc, Mutex},
};

/// Perform full-build profiling, from which the user can go to trace profiling
pub fn profile(cursive: &mut CursiveRunnable, args: CliArgs) {
    // Load the compilation database
    let compilation_database = match build::commands::load() {
        Ok(database) => database,
        Err(e) => {
            let message = match e {
                DatabaseLoadError::FileNotFound => {
                    "No compilation database found. But it is needed for \
                    full-build profiling!\n\
                    Make sure you are in the build directory and/or re-run \
                    CMake with the -DCMAKE_EXPORT_COMPILE_COMMANDS=ON option."
                        .to_owned()
                }
                DatabaseLoadError::IoError(e) => {
                    format!("Failed to load compilation database: {e}")
                }
                DatabaseLoadError::ParseError(e) => {
                    format!("Failed to parse compilation database: {e}")
                }
            };
            error(cursive, message);
            return;
        }
    };

    // Determine the path to the build profile, (re)creating it if needed
    let profile_path =
        if let Some(profile_path) = update_profile(cursive, &args, &compilation_database) {
            profile_path
        } else {
            return;
        };

    // TODO: Load profile and display it
    // TODO: Use trace::profile where appropriate. Remember to correctly handle
    //       the case where a trace in the build profile isn't present in the
    //       compilation database, which can happen with stale profiles.
    // TODO: For traces, don't prompt for staleness, just compare input and
    //       output file age and recreate profile file as needed.
}

/// Update the build profile as needed as preparation for opening it
///
/// Returns the path to the build profile, which can be assumed to exist, or
/// None if we can't create a build profile and should stop here.
///
pub fn update_profile(
    cursive: &mut CursiveRunnable,
    args: &CliArgs,
    compilation_database: &CompilationDatabase,
) -> Option<Box<Path>> {
    // Determine full build profile path
    let default_profile_path = Path::new(profile::DEFAULT_LOCATION);
    let is_default_path = args.build_profile.is_none();
    let profile_path = args
        .build_profile
        .as_ref()
        .map(Path::new)
        .unwrap_or(default_profile_path);

    // Check if we should ask the user about full build profile (re)creation
    let profile_exists = profile_path.exists();
    let create_prompt = match (profile_exists, is_default_path) {
        // The specified build profile does not exist yet
        (false, _) => Some(CreatePrompt::new_build_profile()),

        // A build profile exists, but it may be stale. Since the user stuck
        // with the default build profile location, a warning may be in order.
        (true, true) => match profile::is_up_to_date(profile_path, compilation_database) {
            Ok(true) => {
                let profile_age = match build::file_age(profile_path) {
                    Ok(profile_age) => profile_age,
                    Err(e) => {
                        error(cursive, format!("Failed to check build profile age: {e}"));
                        return None;
                    }
                };
                let profile_age_mins = profile_age.as_secs() / 60;
                if profile_age_mins > 1 {
                    Some(CreatePrompt::stale_build_profile(Some(profile_age_mins)))
                } else {
                    None
                }
            }
            Ok(false) => Some(CreatePrompt::stale_build_profile(None)),
            Err(e) => {
                error(
                    cursive,
                    format!("Failed to check build profile staleness: {e}"),
                );
                return None;
            }
        },

        // The user manually requested using a certain build profile that exists,
        // so we're going to display that without unwelcome checks & questions
        (true, false) => None,
    };

    // Handle build profile creation requests
    if let Some(create_prompt) = create_prompt {
        let should_create = create_prompt.ask(cursive);
        if should_create {
            if !measure_profile(cursive, profile_path, compilation_database) {
                return None;
            }
        } else if !profile_exists {
            // If the user does not want to create a profile and none exists,
            // we have to stop there and terminate the program.
            return None;
        }
    }
    Some(profile_path.into())
}

/// Measure a build profile, storing the result in a specific location
///
/// Return true if everything worked out, false if that failed and the
/// application must terminate.
///
fn measure_profile(
    cursive: &mut CursiveRunnable,
    path: &Path,
    compilation_database: &CompilationDatabase,
) -> bool {
    // Check for cmakeperf availability
    let error_message = match cmakeperf::find() {
        Ok(status) if status.success() => None,
        Ok(bad_status) => Some(format!(
            "Found cmakeperf, but it exits with the error status {bad_status}.\n\
            Please repair your cmakeperf installation to enable build profiling."
        )),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Some(
            "cmakeperf was not found, please install it or bring it into PATH \
            to enable build profiling.\n\
            'pip3 install cmakeperf' should do this for you, but the use of a \
            virtual environment is recommended to avoid harmful interactions."
                .to_owned(),
        ),
        Err(e) => Some(format!(
            "Failed to run cmakeperf for build profiling due to an I/O error: {e}"
        )),
    };
    if let Some(message) = error_message {
        error(cursive, message);
        return false;
    }

    // Start cmakeperf
    let mut collect = match cmakeperf::Collect::start(path) {
        Ok(collect) => collect,
        Err(e) => {
            error(cursive, format!("Failed to start cmakeperf: {e}"));
            return false;
        }
    };

    // Set up progress dialog
    let cb_sink = cursive.cb_sink().clone();
    let outcome = Arc::new(Mutex::new(None));
    let outcome2 = outcome.clone();
    cursive.add_layer(
        Dialog::around(
            LinearLayout::vertical()
                .child(TextView::new(
                    "Profiling the build, please minimize system activity...",
                ))
                .child(
                    ProgressBar::new()
                        .max(compilation_database.len() + 1)
                        .with_task(move |counter| {
                            loop {
                                let outcome = match collect.wait_next_step() {
                                    Ok(CollectStep::FileCompiled) => {
                                        counter.tick(1);
                                        continue;
                                    }
                                    Ok(CollectStep::Finished) => Ok(()),
                                    Err(e) => Err(e),
                                };
                                *outcome2.lock().expect("Failed to acquire mutex") = Some(outcome);
                                break;
                            }
                            cb_sink
                                .send(Box::new(|cursive| cursive.quit()))
                                .expect("Failed to exit cursive");
                        }),
                ),
        )
        .button("Abort", Cursive::quit),
    );

    // Measure the profile, handle errors
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);
    cursive.run();
    cursive.set_fps(old_fps.map(|u| u32::from(u)).unwrap_or(0));
    let error_message_or_abort = match outcome.lock() {
        Ok(mut guard) => match guard.take() {
            Some(Ok(())) => return true,
            Some(Err(e)) => Some(format!("Failed to collect build profile: {e}")),
            None => None,
        },
        Err(e) => Some(format!("Build profile collection thread panicked: {e}")),
    };
    let should_abort = if let Some(message) = error_message_or_abort {
        error(cursive, message);
        false
    } else {
        true
    };

    // Try to delete partially generated files, it does not matter if that
    // cleanup operation fails (it may legitimately do so)
    std::mem::drop(std::fs::remove_file(path));
    if should_abort {
        std::process::abort()
    } else {
        return false;
    }
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
    /// Build profile cration prompt when there is no existing build profile
    fn new_build_profile() -> Self {
        Self {
            question: format!(
                "It looks like this build has not been profiled yet. \
                Ready to do so?\n{}",
                Self::BUILD_PROFILE_TRAILER
            ),
            default_reply: "Yes",
            other_reply: "No",
            default_means_create: true,
        }
    }

    /// Build profile cration prompt when the build profile might be or is stale
    ///
    /// If the cpp files changed, we know that the profile is stale, but if
    /// other build dependencies like headers changed, we don't know about it
    /// because CMake won't tell us about those.
    ///
    /// Given that measuring a build profile can take more than an hour, it's
    /// best in any case to ask before overwriting the existing profile, which
    /// may still be good enough.
    ///
    fn stale_build_profile(profile_age_mins: Option<u64>) -> Self {
        let mut question = String::new();
        match profile_age_mins {
            Some(age_mins) => {
                let age_hours = age_mins / 60;
                let age_days = age_hours / 24;
                question.push_str("There is an existing build profile from ");
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
                question.push_str(" ago. Would you consider it up to date?\n");
            }
            None => question.push_str(
                "There is an existing build profile, but it is not up to date \
                with respect to current source code. Use it anyway?\n",
            ),
        }
        question.push_str(Self::BUILD_PROFILE_TRAILER);
        Self {
            question,
            default_reply: "Reuse",
            other_reply: "Measure",
            default_means_create: false,
        }
    }

    /// Common trailer for all build profile creation questions
    const BUILD_PROFILE_TRAILER: &'static str =
        "(Measuring a build profile requires a full single-core build, \
            during which you should minimize other system activity)";

    /// Ask whether a new profile should be created
    fn ask(self, cursive: &mut CursiveRunnable) -> bool {
        let replied_default = Rc::new(Cell::default());
        let reply = |is_default| {
            let replied_default2 = replied_default.clone();
            move |cursive: &mut Cursive| {
                replied_default2.set(is_default);
                cursive.pop_layer();
                cursive.quit();
            }
        };
        cursive.add_layer(
            Dialog::text(self.question)
                .button(self.default_reply, reply(true))
                .button(self.other_reply, reply(false)),
        );
        cursive.run();
        return replied_default.get() ^ !self.default_means_create;
    }
}

/// Simple error dialog, to be followed by application exit
fn error(cursive: &mut CursiveRunnable, message: impl Into<String>) {
    cursive.add_layer(Dialog::text(message).button("Quit", Cursive::quit));
    cursive.run();
}
