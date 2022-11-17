//! Interactive UI for loading trace profiles

use crate::ui::tui::{processing::ProcessingThread, trace, with_state};
use clang_time_trace::Duration;
use cursive::{views::Dialog, Cursive};
use std::{
    num::NonZeroU32,
    path::Path,
    sync::{
        atomic::{self, AtomicBool, Ordering},
        Arc,
    },
};

/// Load and display a time-trace profile
pub fn show_loader(cursive: &mut Cursive, trace_path: impl AsRef<Path>) {
    // Load the trace, take note of how many other layers we have
    let (wait_state, canceled) = start_wait_for_input(cursive);
    let cb_sink = cursive.cb_sink().clone();
    with_state(cursive, |state| {
        state
            .processing_thread
            .start_load_trace(trace_path, move |result| {
                cb_sink
                    .send(Box::new(move |cursive| {
                        // Check if the load was canceled, if so do nothing
                        if canceled.load(Ordering::Relaxed) {
                            atomic::fence(Ordering::Acquire);
                            return;
                        }

                        // Restore cursive state altered by the loading screen
                        end_wait_for_input(cursive, wait_state);

                        // Handle trace loading errors
                        if let Err(error) = result {
                            cursive.add_layer(Dialog::info(format!(
                                "Failed to process input: {error}"
                            )));
                            return;
                        }

                        // Query the list of root activities and deduce the global percentage norm
                        let (root_activities, global_percent_norm) = with_state(cursive, |state| {
                            let root_activities = state.processing_thread.get_root_activities();
                            let global_percent_norm = super::percent_norm(
                                root_activities
                                    .iter()
                                    .map(|activity| activity.duration)
                                    .sum::<Duration>(),
                            );
                            state.global_percent_norm = Some(global_percent_norm);
                            state.display_config.reset_duration_display();
                            (root_activities, global_percent_norm)
                        });

                        // Display the hierarchical profile
                        trace::display::show_hierarchical_profile(
                            cursive,
                            "<profile root>".into(),
                            global_percent_norm,
                            root_activities,
                            |state| state.processing_thread.get_all_activities(),
                        );
                    }))
                    .expect("Failed to send callback to main thread");
            });
    });
}

/// Start displaying a pretty loading screen
///
/// Returns a WaitForInputState token that should be passed back to
/// end_wait_for_input at the end of the loading process along with an
/// AtomicBool that tells whether the user has canceled the loading process.
///
fn start_wait_for_input(cursive: &mut Cursive) -> (WaitForInputState, Arc<AtomicBool>) {
    // Set up loading screen state
    let layers_below_profile = cursive.screen().len();
    with_state(cursive, |state| {
        state.layers_below_profile = layers_below_profile;
        state.no_escape = true;
    });

    // Make sure Cursive promptly takes notice of the completion callback
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);
    let state = WaitForInputState { old_fps };

    // Set up a cancelation mechanism for this loading screen
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

    // Set up the loading screen and its cancelation mechanism
    //
    // Cancelation does not kill the processing thread, because that would
    // leak all system resources it's holding. Instead, a new processing thread
    // is started, and the old processing thread is left to terminate
    // gracefully in the background. This will admittedly waste a few seconds of
    // CPU time, but the alternative of making the trace loading process
    // interruptible would be much more complex at the code level.
    //
    cursive.add_layer(
        Dialog::text("Processing time trace...").button("Abort", move |cursive| {
            canceled2.store(true, atomic::Ordering::Release);
            end_wait_for_input(cursive, state);
            if cursive.screen().is_empty() {
                cursive.quit();
            } else {
                with_state(cursive, |state| {
                    state.processing_thread = ProcessingThread::start();
                });
            }
        }),
    );

    // Return saved Cursive state
    (state, canceled)
}

/// Saved Cursive state returned by start_wait_for_input, to be passed back to
/// end_wait_for_input for a return to the original Cursive state.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct WaitForInputState {
    /// Cursive FPS setting at the beginning of the wait
    old_fps: Option<NonZeroU32>,
}

/// Callback to clean up after start_wait_for_input
///
/// This is to be called after the ClangTrace has been loaded by the processing
/// thread, taking as a parameter the saved Cursive state that was emitted by
/// start_wait_for_input. It will return to the Cursive state before
/// start_wait_for_input was called.
///
fn end_wait_for_input(cursive: &mut Cursive, state: WaitForInputState) {
    let old_layers = with_state(cursive, |state| {
        state.no_escape = false;
        state.layers_below_profile
    });
    cursive.set_fps(state.old_fps.map(u32::from).unwrap_or(0));
    while cursive.screen().len() > old_layers {
        cursive.pop_layer();
    }
}
