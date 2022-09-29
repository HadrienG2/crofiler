//! Full-build profiling interface

use crate::{build, build::profile, CliArgs};
use cursive::{views::Dialog, Cursive, CursiveRunnable};
use std::{cell::Cell, fmt::Write, path::Path, rc::Rc};

/// Perform full-build profiling, from which the user can go to trace profiling
pub fn profile(cursive: &mut CursiveRunnable, args: CliArgs) {
    // Determine the path to the build profile, creating it if needed
    let profile_path = if let Some(profile_path) = update_profile(cursive, &args) {
        profile_path
    } else {
        return;
    };

    // TODO: Load profile and display it
    // TODO: Use trace::profile where appropriate
    // TODO: For traces, don't prompt for staleness, just compare input and
    //       output file age and recreate profile file as needed.
}

/// Update the build profile as needed as preparation for opening it
///
/// Returns the path to the build profile, which can be assumed to exist, or
/// None if we can't create a build profile and should stop here.
///
pub fn update_profile(cursive: &mut CursiveRunnable, args: &CliArgs) -> Option<Box<Path>> {
    // Determine full build profile path
    let default_profile_path: &Path = profile::DEFAULT_LOCATION.as_ref();
    let is_default_path = args.build_profile.is_none();
    let profile_path = args
        .build_profile
        .as_ref()
        .map(|p| -> &Path { p.as_ref() })
        .unwrap_or(default_profile_path);

    // Check if we should ask the user about full build profile (re)creation
    let profile_exists = profile_path.exists();
    let create_prompt = match (profile_exists, is_default_path) {
        // The specified build profile does not exist yet
        (false, _) => Some(CreatePrompt::new_build_profile()),

        // A build profile exists, but it may be stale. Since the user stuck
        // with the default build profile location, a warning may be in order.
        (true, true) => {
            let file_age =
                build::file_age(profile_path).expect("Failed to query build profile age");
            let file_age_mins = file_age.as_secs() / 60;
            if file_age_mins > 1 {
                Some(CreatePrompt::stale_build_profile(file_age_mins))
            } else {
                None
            }
        }

        // The user specified manually specified a build profile that exists,
        // so we're going to display that without extra questions.
        (true, false) => None,
    };

    // Handle build profile creation requests
    if let Some(create_prompt) = create_prompt {
        let should_create = create_prompt.ask(cursive);
        if should_create {
            measure_profile(profile_path);
        } else if !profile_exists {
            // If the user does not want to create a profile and none exists,
            // we have to stop there and terminate the program.
            return None;
        }
    }
    Some(profile_path.into())
}

/// Measure a build profile, storing the result in a specific location
fn measure_profile(path: &Path) {
    // TODO: Check for compilation database existence
    // TODO: Check for cmakeperf availability
    // TODO: Run cmakeperf
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
                "It looks like this build hasn't been profiled yet. \
                Ready to do so?\n{}",
                Self::BUILD_PROFILE_TRAILER
            ),
            default_reply: "Yes",
            other_reply: "No",
            default_means_create: true,
        }
    }

    /// Build profile cration prompt when there is an existing build profile
    /// but it is old enough that it might be considered stale.
    fn stale_build_profile(file_age_mins: u64) -> Self {
        let file_age_hours = file_age_mins / 60;
        let file_age_days = file_age_hours / 24;
        let mut question = "There is an existing build profile from ".to_owned();
        if file_age_days > 0 {
            write!(question, "{file_age_days} day").expect("Write to String can't fail");
            if file_age_days > 1 {
                write!(question, "s").expect("Write to String can't fail");
            }
        } else if file_age_hours > 0 {
            write!(question, "{file_age_hours}h").expect("Write to String can't fail");
        } else {
            write!(question, "{file_age_mins}min").expect("Write to String can't fail");
        }
        question.push_str(" ago. Is that fresh enough?\n");
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
