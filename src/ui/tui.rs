//! Interactive textual user interface

use crate::CliArgs;
use clang_time_trace::ClangTrace;
use cursive::views::TextView;
use cursive::CursiveRunnable;
use scoped_threadpool::Pool;
use std::{
    path::Path,
    sync::{Mutex, TryLockError},
};

/// Run the analysis using the textual user interface
pub fn run(args: CliArgs) {
    // Set up processing thread, cursive context and exit key
    let mut pool = Pool::new(1);
    let mut cursive = cursive::default();
    cursive.add_global_callback('q', |cursive| cursive.quit());

    // Load the clang time trace
    let trace = load_clang_trace(&mut cursive, &mut pool, &args.input);
}

/// Load the clang trace
// TODO: Abstract away the general "loading screen" pattern
fn load_clang_trace(
    cursive: &mut CursiveRunnable,
    processing_thread: &mut Pool,
    path: &Path,
) -> ClangTrace {
    let trace_output = Mutex::new(None);
    processing_thread.scoped(|scope| {
        // Set up the loading screen
        cursive.add_layer(TextView::new("Processing input data...\nPress q to quit."));

        // Start processing the input data
        scope.execute(|| {
            let mut lock = trace_output.lock().unwrap();
            *lock = Some(ClangTrace::from_file(path).unwrap());
        });

        // Initiate the cursive event loop
        let mut runner = cursive.runner();
        runner.refresh();
        loop {
            // Process TUI events
            runner.step();

            // Abort input processing if instructed to do so
            if !runner.is_running() {
                // FIXME: Replace by regular return once input processing is faster
                std::mem::drop(runner);
                std::mem::drop(cursive);
                std::process::abort()
            }

            // Otherwise check how the input processing is going
            match trace_output.try_lock() {
                Ok(mut guard) => {
                    if let Some(trace) = guard.take() {
                        break trace;
                    } else {
                        unreachable!()
                    }
                }
                Err(TryLockError::WouldBlock) => continue,
                Err(TryLockError::Poisoned(e)) => panic!("The processing thread crashed ({e})"),
            }
        }
    })
}
