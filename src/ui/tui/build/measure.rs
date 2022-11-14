//! Measure a full-build profile

use crate::ui::tui::with_state;
use cmakeperf::{
    commands::CompilationDatabase,
    measure::{self, BuildStep, Collect, CollectError},
};
use cursive::{
    views::{Dialog, LinearLayout, ProgressBar, TextView},
    Cursive, CursiveRunnable,
};
use std::{
    io,
    path::Path,
    sync::{Arc, Mutex},
};

/// Measure a build profile, storing the result in a specific location
///
/// Return true if everything worked out, false if that failed and the
/// application should terminate.
///
#[must_use]
pub fn measure_profile(
    cursive: &mut CursiveRunnable,
    output_path: &Path,
    compilation_database: &CompilationDatabase,
) -> bool {
    // Set up cmakeperf data collection process
    let (mut collect, mut output) = match CollectHandle::start(cursive, output_path) {
        Some(tuple) => tuple,
        None => return false,
    };

    // Set up and run through the progress dialog
    let outcome = {
        let cursive = collect.cursive();
        let layers_below_profile = cursive.screen().len();
        with_state(cursive, |state| {
            state.no_escape = true;
        });
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
                            .max(compilation_database.entries().count())
                            .with_task(move |counter| {
                                loop {
                                    let outcome = match output.next_step() {
                                        Ok(BuildStep::UnitDone) => {
                                            counter.tick(1);
                                            continue;
                                        }
                                        Ok(BuildStep::BuildDone) => Ok(()),
                                        Err(error) => Err(error),
                                    };
                                    *outcome2.lock().expect("Main thread has panicked") =
                                        Some(outcome);
                                    break;
                                }
                                cb_sink
                                    .send(Box::new(Cursive::quit))
                                    .expect("Failed to exit cursive");
                            }),
                    ),
            )
            .button("Abort", Cursive::quit),
        );
        let old_fps = cursive.fps();
        cursive.set_autorefresh(true);
        cursive.run();
        while cursive.screen().len() > layers_below_profile {
            cursive.pop_layer();
        }
        cursive.set_fps(old_fps.map(u32::from).unwrap_or(0));
        with_state(cursive, |state| {
            state.no_escape = false;
        });
        outcome
    };

    // Once we reach this point, the data collection process may either be done,
    // have errored out, or have been aborted by the user. We now need to
    // discriminate these various scenarios and react accordingly.
    let error_message = match outcome.lock() {
        Ok(mut guard) => match guard.take() {
            // Monitoring thread read process stdout through to the end, at this
            // point joining the process should not block.
            Some(Ok(())) => match collect.finish() {
                // Exit with successful status on success
                Ok(()) => return true,
                Err(e) => Some(format!("Failed to join cmakeperf process: {e}")),
            },

            // The monitoring thread errored out. I can't think of a sane
            // situation where this should happen, but let's handle it anyway...
            Some(Err(e)) => Some(format!("Failed to collect build profile: {e}")),

            // The user has requested that data collection be aborted
            None => None,
        },

        // The thread monitoring the data collection stdout has panicked
        Err(e) => Some(format!("Build profile monitoring thread panicked: {e}")),
    };
    if let Some(message) = error_message {
        super::error(collect.cursive(), message);
    }

    // Next, try to kill the data collection process. This may race with the
    // process completing normally in the background, which is fine, but other
    // errors are not fine.
    match collect.kill() {
        Ok(()) => {}
        Err(e)
            if e.kind() == io::ErrorKind::InvalidInput || e.kind() == io::ErrorKind::NotFound => {}
        Err(other) => super::error(
            collect.cursive(),
            format!("Failed to kill cmakeperf: {other}"),
        ),
    }

    // Finally, exit with an error status
    false
}

/// Handle to an ongoing cmakeperf data collection process
///
/// If an old profile already exists, a backup will be taken, and restored in
/// case this is dropped without seeing the whole data collection process
/// through. This provides RAII-based data collection error & cancelation handling.
///
struct CollectHandle<'cursive, 'output> {
    /// Cursive handle
    cursive: &'cursive mut CursiveRunnable,

    /// Path to the output file
    output_path: &'output Path,

    /// Path to the backup
    backup_path: Option<Box<Path>>,

    /// Live cmakeperf data collection process
    collect: Collect,
}
//
impl<'cursive, 'output> CollectHandle<'cursive, 'output> {
    /// Set up the cmakeperf data collection process
    fn start(
        cursive: &'cursive mut CursiveRunnable,
        output_path: &'output Path,
    ) -> Option<(Self, measure::Output)> {
        // Check for cmakeperf availability
        let error_message = match measure::find() {
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
            super::error(cursive, message);
            return None;
        }

        // If a cmakeperf output already exists, back it up
        let backup_path = output_path.with_extension("bak").into_boxed_path();
        match std::fs::rename(output_path, &backup_path) {
            Ok(()) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {}
            Err(other) => {
                super::error(
                    cursive,
                    format!("Failed to back up previous build profile: {other}"),
                );
                return None;
            }
        }

        // Start cmakeperf
        let (collect, output) = match Collect::start(output_path) {
            Ok(tuple) => tuple,
            Err(e) => {
                super::error(cursive, format!("Failed to start cmakeperf: {e}"));
                Self::restore_or_delete(cursive, &backup_path, output_path);
                return None;
            }
        };

        // Emit process and stdout handles
        Some((
            Self {
                cursive,
                output_path,
                backup_path: Some(backup_path),
                collect,
            },
            output,
        ))
    }

    /// Borrow the cursive handle for other uses
    fn cursive(&mut self) -> &mut CursiveRunnable {
        self.cursive
    }

    /// Wait for the cmakeperf data collection process to complete
    fn finish(&mut self) -> Result<(), CollectError> {
        let result = self.collect.finish();
        if result.is_ok() {
            // Disable the backup restore/output deletion process on successful run
            self.backup_path = None;
        }
        result
    }

    // Kill the data collection process
    fn kill(&mut self) -> io::Result<()> {
        self.collect.kill()
    }

    /// Attempt to restore a full build profile backup
    fn restore_or_delete(cursive: &mut CursiveRunnable, backup_path: &Path, output_path: &Path) {
        match std::fs::rename(backup_path, output_path)
            .or_else(|_| std::fs::remove_file(output_path))
        {
            Ok(()) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {}
            Err(other) => super::error(
                cursive,
                format!("Failed to clean up after cmakeperf: {other}"),
            ),
        };
    }
}
//
impl Drop for CollectHandle<'_, '_> {
    fn drop(&mut self) {
        if let Some(backup_path) = &self.backup_path {
            Self::restore_or_delete(self.cursive, backup_path, self.output_path);
        }
    }
}
