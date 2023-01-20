//! Wizard that guides the user through the process of measuring a time-trace

use crate::ui::tui::{create::CreatePrompt, trace, with_state};
use cmakeperf::commands::CompilationDatabase;
use cursive::{views::Dialog, Cursive};
use std::{
    fmt::Write,
    io::{self, Read},
    num::NonZeroU32,
    path::Path,
    process::{Child, Command, ExitStatus, Stdio},
    sync::{
        atomic::{self, AtomicBool, Ordering},
        Arc,
    },
    time::Duration,
};
use wait_timeout::ChildExt;

/// Wizard guiding the user towards displaying a trace profile of a given file,
/// measuring it if need be.
pub fn show_wizard(
    cursive: &mut Cursive,
    compilation_database: &CompilationDatabase,
    rel_path: &Path,
    clangpp: &str,
    time_trace_granularity: Option<u64>,
) {
    // Find the absolute path to the input file
    let abs_path = match rel_path.canonicalize() {
        Ok(path) => path,
        Err(e) => {
            cursive.add_layer(Dialog::info(format!(
                "Failed to resolve relative path {}, perhaps that source file doesn't exist anymore? Error was: {}",
                rel_path.display(), e
            )));
            return;
        }
    };

    // Find the compilation database entry for the target file
    let entry = match compilation_database.entry(&abs_path) {
        Some(entry) => entry,
        None => {
            cursive.add_layer(Dialog::info(format!(
                "No compilation database entry for source file {}, you may need \
                to update the compilation database by re-running CMake...",
                rel_path.display()
            )));
            return;
        }
    };

    // Determine clang time-trace output file name
    let mut output = match entry.output() {
        Some(output) => output,
        None => {
            cursive.add_layer(Dialog::info(format!(
                "Failed to extract output file from compilation command {:?}",
                entry.raw_command()
            )));
            return;
        }
    };
    output.set_extension("json");

    // Check if we should ask the user about build profile (re)creation
    let freshness = match entry.derived_freshness(&output) {
        Ok(freshness) => freshness,
        Err(e) => {
            cursive.add_layer(Dialog::info(format!(
                "Failed to check build profile freshness: {e}"
            )));
            return;
        }
    };
    let create_prompt = CreatePrompt::from_freshness(freshness, false);

    // Handle build profile creation requests
    if let Some(create_prompt) = create_prompt {
        // Set up the clang command line (done early for ownership reasons)
        let mut command = Command::new(clangpp);
        command.current_dir(entry.current_dir()).arg("-ftime-trace");
        if let Some(granularity) = time_trace_granularity {
            command.arg(format!("-ftime-trace-granularity={granularity}"));
        }
        for arg in entry.args() {
            command.arg(arg.as_ref());
        }

        // Ask whether the profile should be (re)created
        create_prompt.show(cursive, move |cursive, should_create| {
            if should_create {
                // If so, measure a new one, then display
                start_measure(cursive, command, output.into_boxed_path());
            } else if freshness.exists() {
                // Otherwise, display existing profile if available
                trace::load::show_loader(cursive, output);
            }
        });
    } else {
        // If the existing profile is good, display it right away
        trace::load::show_loader(cursive, output);
    }
}

/// Start measuring a clang time-trace
pub fn start_measure(cursive: &mut Cursive, command: Command, output_path: Box<Path>) {
    // Back up the previous measurement, if any
    let backup = match TraceBackup::new(output_path) {
        Ok(backup) => backup,
        Err(error) => {
            cursive.add_layer(Dialog::info(error));
            return;
        }
    };

    // Start clang
    let (clang, cancel_trigger) = match CancelableClang::start(command) {
        Ok(tuple) => tuple,
        Err(error) => {
            cursive.add_layer(Dialog::info(error));
            if let Some(error) = backup.restore_or_delete() {
                cursive.add_layer(Dialog::info(error));
            }
            return;
        }
    };

    // Set up measurement screen state
    let layers_below_profile = cursive.screen().len();
    with_state(cursive, |state| {
        state.layers_below_profile = layers_below_profile;
        state.no_escape = true;
    });

    // Make sure Cursive promptly takes notice of the completion callback
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);
    let state = MeasureState { old_fps };

    // Set up the measurement screen
    cursive.add_layer(
        Dialog::text("Measuring time trace, please minimize system activity...")
            .button("Abort", move |_cursive| cancel_trigger.cancel()),
    );

    // Start a thread that measures a clang time-trace and arranges for the
    // next steps to be taken once that's done
    let cb_sink = cursive.cb_sink().clone();
    std::thread::spawn(move || {
        // Force-send a callback to the cursive thread with less verbosity
        let cb_sink = &cb_sink;
        fn callback(
            cb_sink: &cursive::CbSink,
            callback: impl FnOnce(&mut Cursive) + Send + 'static,
        ) {
            cb_sink
                .send(Box::new(callback))
                .expect("Failed to send cursive callback to main thread");
        }

        // Wait for clang to finish or for the cancelation signal to be sent
        let wait_result = clang.wait();
        callback(cb_sink, move |cursive| end_measure(cursive, state));

        // Handle success and failure
        let error_message = match wait_result {
            // Clang completed successfully
            Ok(false) => {
                // Delete the backup file
                let (output_path, backup_error) = backup.cleanup();

                // Show the trace loading screen
                callback(cb_sink, move |cursive| {
                    trace::load::show_loader(cursive, output_path)
                });

                // Report errors during the backup deletion process on top of it
                if let Some(error) = backup_error {
                    callback(cb_sink, move |cursive| {
                        cursive.add_layer(Dialog::info(error))
                    });
                }
                return;
            }

            // Clang execution was successfully canceled
            Ok(true) => None,

            // Failed to await or cancel clang
            Err(error) => Some(error),
        };

        // We're on the failure/abort path, so start by displaying any error messages
        if let Some(error) = error_message {
            callback(cb_sink, move |cursive| {
                cursive.add_layer(Dialog::info(error))
            });
        }

        // Bring back our backup or delete any partial output from clang
        if let Some(error) = backup.restore_or_delete() {
            callback(cb_sink, move |cursive| {
                cursive.add_layer(Dialog::info(error))
            });
        }
    });
}

/// Saved Cursive state returned by start_measure, to be passed back to
/// end_measure for a return to the original Cursive state.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct MeasureState {
    /// Cursive FPS setting at the beginning of the wait
    old_fps: Option<NonZeroU32>,
}

/// Callback to clean up after start_measure
///
/// This is to be called after the ClangTrace has been measured by the processing
/// thread or the process has errored out. It takes as a parameter the saved
/// Cursive state that was emitted by start_measure and attempts to return to
/// the Cursive state before start_measure was called.
///
fn end_measure(cursive: &mut Cursive, state: MeasureState) {
    let old_layers = with_state(cursive, |state| {
        state.no_escape = false;
        state.layers_below_profile
    });
    cursive.set_fps(state.old_fps.map(u32::from).unwrap_or(0));
    while cursive.screen().len() > old_layers {
        cursive.pop_layer();
    }
}

/// Backup of output file from a previous clang time-trace run
///
/// This backup may be restored if the time-trace measurement is unsuccessful or
/// aborted. Otherwise, it should be deleted, as time-trace files can get too
/// large to afford keeping around two copies of each of them.
///
#[must_use]
struct TraceBackup {
    /// Path to output produced by clang time-trace
    output: Box<Path>,

    /// Path to backup from previous clang time-trace run
    backup: Box<Path>,
}
//
impl TraceBackup {
    /// Make a backup of any previous output, report any fatal error
    fn new(output: Box<Path>) -> Result<Self, String> {
        // Back up the previous measurement, if any
        let backup = output.with_extension("bak").into_boxed_path();
        match std::fs::rename(&output, &backup) {
            Ok(()) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {}
            Err(other) => return Err(format!("Failed to back up previous build trace: {other}")),
        }
        Ok(Self { output, backup })
    }

    /// In case of clang failure, this restores the backup if there is one or
    /// deletes any incomplete output file if there is no backup.
    ///
    /// Return an error message to be displayed if something goes wrong
    ///
    #[must_use]
    fn restore_or_delete(self) -> Option<String> {
        match std::fs::rename(&self.backup, &self.output)
            .or_else(|_| std::fs::remove_file(&self.output))
        {
            Ok(()) => None,
            Err(e) if e.kind() == io::ErrorKind::NotFound => None,
            Err(other) => Some(format!("Failed to clean up after clang: {other}")),
        }
    }

    /// In case of clang success, this attempts to delete the backup and returns
    /// the output file path, along with an error message to be displayed if the
    /// deletion process failed
    #[must_use]
    fn cleanup(self) -> (Box<Path>, Option<String>) {
        match std::fs::remove_file(&self.backup) {
            Ok(()) => (self.output, None),
            Err(e) if e.kind() == io::ErrorKind::NotFound => (self.output, None),
            Err(other) => (
                self.output,
                Some(format!("Failed to remove clang trace backup: {other}")),
            ),
        }
    }
}

/// Interruptible clang command
struct CancelableClang {
    /// Child process
    child: Child,

    /// Cancelation flag
    canceled: Arc<AtomicBool>,
}
//
impl CancelableClang {
    /// Start clang and set up cancelation, emit an error message if it goes wrong
    fn start(mut command: Command) -> Result<(Self, CancelTrigger), String> {
        // Start clang process
        let child = match command
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(child) => child,
            Err(e) => {
                return Err(format!("Failed to start clang: {e}"));
            }
        };

        // Set up cancelation
        let canceled = Arc::new(AtomicBool::new(false));
        let cancel_trigger = CancelTrigger(canceled.clone());

        // Return all of that
        Ok((Self { child, canceled }, cancel_trigger))
    }

    /// Wait for the process to terminate or be canceled
    ///
    /// Returns truth that the clang process was canceled on success, emit an
    /// error message if something goes wrong
    ///
    fn wait(mut self) -> Result<bool, String> {
        // Wait for the process to terminate or be canceled
        let wait_result = loop {
            const RESPONSE_TIME: Duration = Duration::from_millis(1000 / 30);
            let wait_result = self.child.wait_timeout(RESPONSE_TIME);
            match wait_result {
                Ok(None) => {
                    if self.canceled.load(Ordering::Relaxed) {
                        atomic::fence(Ordering::Acquire);
                        break wait_result;
                    } else {
                        continue;
                    }
                }
                other => break other,
            }
        };

        // Handle success and failure
        match wait_result {
            // Clang completed successfully
            Ok(Some(status)) if status.success() => Ok(false),

            // Clang terminated with an error status
            Ok(Some(error_status)) => Err(child_failure_message(self.child, error_status)),

            // The work was canceled, kill clang and exit through the error path
            Ok(None) => match self.child.kill() {
                Ok(()) => Ok(true),
                Err(e)
                    if e.kind() == io::ErrorKind::InvalidInput
                        || e.kind() == io::ErrorKind::NotFound =>
                {
                    Ok(true)
                }
                Err(other) => Err(format!("Failed to kill clang: {other}")),
            },

            // Failed to await process, exit through the error path
            Err(e) => Err(format!("Failed to await clang: {e}")),
        }
    }
}
//
/// Cancelation trigger for CancelableClang
struct CancelTrigger(Arc<AtomicBool>);
//
impl CancelTrigger {
    /// Trigger cancelation
    fn cancel(&self) {
        self.0.store(true, Ordering::Release)
    }
}

/// Generate an error message for a failing Child process
///
/// Ignore failure to extract info from stdout and stderr as that's difficult
/// to handle and ultimately these are just nice-to-have details.
///
fn child_failure_message(mut child: Child, error_status: ExitStatus) -> String {
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    std::mem::drop(
        child
            .stdout
            .take()
            .expect("Failed to access stdout")
            .read_to_end(&mut stdout),
    );
    std::mem::drop(
        child
            .stderr
            .take()
            .expect("Failed to access stderr")
            .read_to_end(&mut stderr),
    );
    let mut error = format!("clang time-trace run failed ({})", error_status);
    let append_pipe = |error: &mut String, is_first, stream_name: &str, stream_data| {
        write!(
            error,
            "{} the following {stream_name} output:\n\
            ---\n\
            {}\
            ---",
            if is_first { " with" } else { "\nand" },
            String::from_utf8_lossy(stream_data),
        )
        .expect("Writing to String always succeeds");
    };
    if !stdout.is_empty() {
        append_pipe(&mut error, true, "stdout", &stdout[..]);
    }
    if !stderr.is_empty() {
        append_pipe(&mut error, stdout.is_empty(), "stderr", &stderr[..]);
    }
    error
}
