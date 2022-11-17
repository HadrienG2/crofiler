//! Measure a full-build profile

use crate::ui::tui::with_state;
use cmakeperf::{
    commands::CompilationDatabase,
    measure::{MeasureError, Measurement},
};
use cursive::{
    utils::Counter,
    views::{Dialog, LinearLayout, ProgressBar, TextView},
    Cursive, CursiveRunnable,
};
use std::{
    io,
    panic::UnwindSafe,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
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
    // Set up and run through the progress dialog
    let outcome = {
        // Set up shared state
        with_state(cursive, |state| {
            state.no_escape = true;
        });
        let layers_below_profile = cursive.screen().len();
        let cb_sink = cursive.cb_sink().clone();
        let outcome = Arc::new(Mutex::new(None));
        let outcome2 = outcome.clone();
        let progress_counter = Counter::new(0);
        let progress_counter_2 = progress_counter.clone();

        // Start data collection process
        let mut measurement = match MeasureHandle::start(
            cursive,
            output_path,
            compilation_database,
            move || progress_counter_2.tick(1),
            move |result| {
                *outcome2.lock().expect("Main thread has panicked") = Some(result);
                cb_sink
                    .send(Box::new(Cursive::quit))
                    .expect("Failed to exit cursive");
            },
        ) {
            Some(tuple) => tuple,
            None => return false,
        };

        // Display progress UI
        let cursive = measurement.cursive();
        cursive.add_layer(
            Dialog::around(
                LinearLayout::vertical()
                    .child(TextView::new(
                        "Profiling the build, please minimize system activity...",
                    ))
                    .child(
                        ProgressBar::new()
                            .max(compilation_database.entries().count())
                            .with_value(progress_counter),
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

    // Once we reach this point, the data collection process may either be done
    // or have errored out
    let error_message = match outcome.lock() {
        Ok(mut guard) => match guard.take() {
            // Successfully measured a build profile
            Some(Ok(())) => return true,

            // Build profiling errored out
            Some(Err(e)) => Some(format!("Failed to collect build profile: {e}")),

            // The user has requested that data collection be aborted
            None => None,
        },

        // The thread monitoring the data collection stdout has panicked
        Err(e) => Some(format!("Build profile monitoring thread panicked: {e}")),
    };
    if let Some(message) = error_message {
        super::error(cursive, message);
    }
    false
}

/// Handle to an ongoing cmakeperf data collection process
///
/// If an old profile already exists, a backup will be taken, and restored in
/// case this is dropped without seeing the whole data collection process
/// through. This provides RAII-based data collection error & cancelation handling.
///
struct MeasureHandle<'cursive, 'output> {
    /// Cursive handle
    cursive: &'cursive mut CursiveRunnable,

    /// Path to the output file
    output_path: &'output Path,

    /// Path to the backup
    backup_path: Box<Path>,

    /// Truth that the job has succeeded (in which case backups need not be restored)
    success: Arc<AtomicBool>,

    /// Live cmakeperf data collection process
    _measurement: Measurement,
}
//
impl<'cursive, 'output> MeasureHandle<'cursive, 'output> {
    /// Set up the cmakeperf data collection process
    fn start(
        cursive: &'cursive mut CursiveRunnable,
        output_path: &'output Path,
        compilation_database: &CompilationDatabase,
        step_done: impl FnMut() + UnwindSafe + Send + 'static,
        build_done: impl FnOnce(Result<(), MeasureError>) + Send + 'static,
    ) -> Option<Self> {
        // FIXME: Ask user what kind of profile they want (memory-only or wall-time too ? What concurrency ?)

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

        // Start data collection
        let success = Arc::new(AtomicBool::new(false));
        let success2 = success.clone();
        let measurement = Measurement::start(
            output_path,
            compilation_database,
            // FIXME: let user configure this
            false,
            // FIXME: Let user set this
            None,
            step_done,
            move |result| {
                if result.is_ok() {
                    success2.store(true, Ordering::Release);
                }
                build_done(result)
            },
        );

        // Emit process and stdout handles
        Some(Self {
            cursive,
            output_path,
            backup_path,
            success,
            _measurement: measurement,
        })
    }

    /// Borrow the cursive handle for other uses
    fn cursive(&mut self) -> &mut CursiveRunnable {
        self.cursive
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
impl Drop for MeasureHandle<'_, '_> {
    fn drop(&mut self) {
        if !self.success.load(Ordering::Acquire) {
            Self::restore_or_delete(self.cursive, &self.backup_path, self.output_path);
        }
    }
}
