//! Wizard that guides the user through the process of measuring a time-trace

use crate::{
    build::commands::CompilationDatabase,
    ui::tui::{trace, CreatePrompt},
};
use cursive::{views::Dialog, Cursive};
use std::{
    path::{Path, PathBuf},
    process::Command,
};

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
    // Enable autorefresh so Cursive promptly takes notice of the callback
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);

    // Set up the loading screen
    let old_layers = cursive.screen().len();
    cursive.add_layer(
        Dialog::text("Measuring time trace, please minimize system activity...")
            .button("Abort", Cursive::quit),
    );

    // Arrange for data to be loaded and for the dialog to be closed + cursive
    // state to be restored once this is done.
    let cb_sink = cursive.cb_sink().clone();
    let end_measure = move |cursive: &mut Cursive| {
        cursive.set_fps(old_fps.map(|u| u32::from(u)).unwrap_or(0));
        while cursive.screen().len() > old_layers {
            cursive.pop_layer();
        }
    };
    std::thread::spawn(move || {
        let output = match command.output() {
            Ok(output) => output,
            Err(e) => {
                cb_sink
                    .send(Box::new(move |cursive| {
                        end_measure(cursive);
                        cursive.add_layer(Dialog::info(format!(
                            "Failed to run clang time-trace: {e}"
                        )));
                    }))
                    .expect("Failed to send callback to main thread");
                return;
            }
        };
        if !output.status.success() {
            cb_sink
                .send(Box::new(move |cursive| {
                    end_measure(cursive);
                    let error = format!(
                        "cmakeperf run failed ({}) with the following stderr output:\n---\n{}---",
                        output.status,
                        String::from_utf8_lossy(&output.stderr[..]),
                    );
                    cursive.add_layer(Dialog::info(error));
                }))
                .expect("Failed to send callback to main thread");
            return;
        }
        cb_sink
            .send(Box::new(move |cursive| {
                end_measure(cursive);
                trace::display::show_loader(cursive, output_path);
            }))
            .expect("Failed to send callback to main thread");
    });
}
