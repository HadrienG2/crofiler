//! Full-build profiling interface

use crate::{build::profile, CliArgs};
use cursive::CursiveRunnable;
use std::path::Path;

/// Perform full-build profiling, from which the user can go to trace profiling
///
/// This will perform any of the following preparatory steps as necessary:
/// - Load a full-build profile, allowing the user to (re)generate it if need be
/// - Display sorted profile, let the user pick a compilation unit in it
/// - Pick a time-trace file, allowing the user to (re)generate it if needs be
///
pub fn profile(cursive: &mut CursiveRunnable, args: CliArgs) {
    // Load the full build profile, (re)creating it as needed
    let default_profile_path: &Path = profile::DEFAULT_LOCATION.as_ref();
    let is_default_path = args.build_profile.is_none();
    let profile_path = args
        .build_profile
        .as_ref()
        .map(|p| -> &Path { p.as_ref() })
        .unwrap_or(default_profile_path);
    /*match (profile_path.exists(), is_default_path) {
        (false, _) => // TODO: No build profile available yet, want to make one?
        (true, true) => // TODO: A build profile with age X exists, use or recreate?
        (true, false) => // TODO: Profile exists and user asked for it, use it
    }*/
    // TODO: Use trace::profile where appropriate
}
