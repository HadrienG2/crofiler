//! Wizard that guides the user through the process of measuring a time-trace

use crate::{
    build::commands::CompilationDatabase,
    ui::tui::{trace, with_state, CreatePrompt},
};
use cursive::{views::Dialog, Cursive};
use std::{
    fmt::Write,
    io::{self, Read},
    num::NonZeroU32,
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus, Stdio},
    sync::{
        atomic::{AtomicBool, Ordering},
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
    rel_path: Box<Path>,
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
    let create_prompt = CreatePrompt::from_freshness(freshness, false, false);

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
        let output_2 = output.clone();
        create_prompt.show(cursive, move |cursive, should_create| {
            if should_create {
                // If so, measure a new one, then display
                start_measure(cursive, command, output_2);
            } else if freshness.exists() {
                // Otherwise, display existing profile if available
                trace::display::show_loader(cursive, output_2);
            }
        });
    } else {
        // If the existing profile is good, display it right away
        trace::display::show_loader(cursive, output);
    }
}

/// Start measuring a clang time-trace
pub fn start_measure(cursive: &mut Cursive, mut command: Command, output_path: PathBuf) {
    // Back up the previous measurement, if any
    let backup_path = output_path.with_extension("bak");
    match std::fs::rename(&output_path, &backup_path) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::NotFound => {}
        Err(other) => {
            cursive.add_layer(Dialog::info(format!(
                "Failed to back up previous build trace: {other}"
            )));
            return;
        }
    }

    // If clang fails, we'll restore the backup if there is one or delete
    // the incomplete output file if there is no backup
    // This returns an error message to be displayed if this went wrong.
    let restore_or_delete = move |backup_path: &Path, output_path: &Path| match std::fs::rename(
        backup_path,
        output_path,
    )
    .or_else(|_| std::fs::remove_file(output_path))
    {
        Ok(()) => None,
        Err(e) if e.kind() == io::ErrorKind::NotFound => None,
        Err(other) => Some(format!("Failed to clean up after clang: {other}")),
    };

    // Start clang
    let mut clang = match command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(clang) => clang,
        Err(e) => {
            cursive.add_layer(Dialog::info(format!("Failed to start clang: {e}")));
            if let Some(error) = restore_or_delete(&backup_path, &output_path) {
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

    // Set up a cancelation mechanism for this measurement screen
    // We shouldn't abuse state.no_escape for this because then the following
    // race condition can occur:
    //
    // - User starts loading a trace
    // - User change their mind and cancel the load
    // - User starts loading another trace
    // - First load completes and displays on screen instead of being dropped
    //
    let canceled = Arc::new(AtomicBool::new(false));
    let canceled2 = canceled.clone();

    // Set up the measurement screen
    cursive.add_layer(
        Dialog::text("Measuring time trace, please minimize system activity...")
            .button("Abort", move |_cursive| {
                canceled2.store(true, Ordering::Relaxed)
            }),
    );

    // Start a thread that measures a clang time-trace and arranges for the
    // next steps to be taken once that's done
    let cb_sink = cursive.cb_sink().clone();
    std::thread::spawn(move || {
        // Wait for clang to finish or for the cancelation signal to be sent
        let wait_result = loop {
            const RESPONSE_TIME: Duration = Duration::from_millis(1000 / 30);
            let wait_result = clang.wait_timeout(RESPONSE_TIME);
            match wait_result {
                Ok(None) => {
                    if canceled.load(Ordering::Relaxed) {
                        break wait_result;
                    } else {
                        continue;
                    }
                }
                other => break other,
            }
        };
        cb_sink
            .send(Box::new(move |cursive| end_measure(cursive, state)))
            .expect("Failed to send callback to main thread");

        // Handle success and failure
        let error_message = match wait_result {
            // Clang completed successfully
            Ok(Some(status)) if status.success() => {
                // Proceed with trace loading and display
                cb_sink
                    .send(Box::new(move |cursive| {
                        trace::display::show_loader(cursive, output_path);
                    }))
                    .expect("Failed to send callback to main thread");

                // Remove the backup file
                match std::fs::remove_file(backup_path) {
                    Ok(()) => {}
                    Err(e) if e.kind() == io::ErrorKind::NotFound => {}
                    Err(other) => {
                        cb_sink
                            .send(Box::new(move |cursive| {
                                cursive.add_layer(Dialog::info(format!(
                                    "Failed to remove clang trace backup: {other}"
                                )));
                            }))
                            .expect("Failed to send callback to main thread");
                    }
                }
                return;
            }

            // Clang terminated with an error status
            Ok(Some(error_status)) => Some(child_failure_message(clang, error_status)),

            // The work was canceled, kill clang and exit through the error path
            Ok(None) => match clang.kill() {
                Ok(()) => None,
                Err(e)
                    if e.kind() == io::ErrorKind::InvalidInput
                        || e.kind() == io::ErrorKind::NotFound =>
                {
                    None
                }
                Err(other) => Some(format!("Failed to kill clang: {other}")),
            },

            // Failed to await process, exit through the error path
            Err(e) => Some(format!("Failed to await clang: {e}")),
        };

        // We're on the failure/abort path, so start by displaying any error messages
        if let Some(message) = error_message {
            cb_sink
                .send(Box::new(move |cursive| {
                    cursive.add_layer(Dialog::info(message))
                }))
                .expect("Failed to send callback to main thread");
        }

        // Bring back our backup or delete any partial output from clang
        if let Some(error) = restore_or_delete(&backup_path, &output_path) {
            cb_sink
                .send(Box::new(|cursive| cursive.add_layer(Dialog::info(error))))
                .expect("Failed to send callback to main thread");
        }

        // Tell cursive to exit in case all windows were closed
        cb_sink
            .send(Box::new(move |cursive| {
                if cursive.screen().is_empty() {
                    cursive.quit();
                }
            }))
            .expect("Failed to send callback to main thread");
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
    cursive.set_fps(state.old_fps.map(|u| u32::from(u)).unwrap_or(0));
    while cursive.screen().len() > old_layers {
        cursive.pop_layer();
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
