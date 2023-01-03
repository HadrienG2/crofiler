//! Full-build profiling interface

mod display;
mod measure;

use self::{display::display_profile, measure::measure_profile};
use crate::{clang, ui::tui::create::CreatePrompt, CliArgs};
use cmakeperf::{
    commands::{CompilationDatabase, DatabaseLoadError},
    output::ProfileLoadError,
};
use cursive::{views::Dialog, Cursive, CursiveRunnable};
use std::path::Path;

/// Perform full-build profiling, from which the user can go to trace profiling
pub fn profile(cursive: &mut CursiveRunnable, args: CliArgs) {
    // Load the compilation database
    let compilation_database = match CompilationDatabase::load() {
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

    // Check for availability of a suitable clang++ version
    let clangpp = match clang::find_clangpp() {
        Ok(program) => program,
        Err(e) => {
            error(
                cursive,
                format!("Did not find a clang version suitable for build profiling: {e}"),
            );
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

    // Load the full-build profile
    let profile = match cmakeperf::output::load(profile_path) {
        Ok(profile) => profile,
        Err(e) => {
            let message = match e {
                ProfileLoadError::FileNotFound => {
                    "Performance profile vanished before it could be loaded!".to_owned()
                }
                ProfileLoadError::ParseError(p) => {
                    format!("Failed to parse performance profile: {p}")
                }
            };
            error(cursive, message);
            return;
        }
    };

    // Display the build profile and let the user pick a trace to anaylze
    display_profile(cursive, args, compilation_database, clangpp, profile)
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
    let default_profile_path = Path::new(cmakeperf::output::DEFAULT_LOCATION);
    let manually_specified = args.build_profile.is_some();
    let profile_path = args
        .build_profile
        .as_ref()
        .map(Path::new)
        .unwrap_or(default_profile_path);

    // Check if we should ask the user about full build profile (re)creation
    let freshness = match compilation_database.profile_freshness(profile_path) {
        Ok(freshness) => freshness,
        Err(e) => {
            error(
                cursive,
                format!("Failed to check build profile freshness: {e}"),
            );
            return None;
        }
    };
    let create_prompt = CreatePrompt::from_freshness(freshness, manually_specified);

    // Handle build profile creation requests
    if let Some(create_prompt) = create_prompt {
        if create_prompt.ask(cursive)? {
            if !measure_profile(cursive, profile_path, compilation_database) {
                return None;
            }
        } else if !freshness.exists() {
            // If the user does not want to create a profile and none exists,
            // we have to stop there and terminate the program.
            return None;
        }
    }
    Some(profile_path.into())
}

/// Truth that a build profile is being displayed
pub fn is_profiling(cursive: &mut Cursive) -> bool {
    super::with_state(cursive, |state| state.showing_full_build)
}

/// Simple error dialog, to be followed by application exit
fn error(cursive: &mut CursiveRunnable, message: impl Into<String>) {
    cursive.add_layer(Dialog::text(message).button("Quit", Cursive::quit));
    cursive.run();
}
