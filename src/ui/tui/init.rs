//! Initialization of the text user interface

use super::{profile, State};
use clang_time_trace::ClangTraceLoadError;
use cursive::{
    event::{Event, Key},
    view::Nameable,
    views::{Dialog, SelectView},
    Cursive, CursiveRunnable,
};

/// Perform basic cursive setup
pub fn setup_cursive(state: State) -> CursiveRunnable {
    // Initialize cursive
    let mut cursive = cursive::default();

    // Set up user state
    cursive.set_user_data(state);

    // Esc always exits the current layer if there's another underneath
    cursive.set_global_callback(Key::Esc, exit_current_layer);

    // U switches between duration units for all active profiles
    cursive.set_global_callback('u', profile::switch_duration_unit);

    // We do not allow dialogs spawned by global keyboard shortcuts to stack on
    // top of each other as this is jarring and has no known use case.
    fn set_global_dialog_callback(
        cursive: &mut Cursive,
        event: impl Into<Event>,
        mut dialog_factory: impl 'static + FnMut(&mut Cursive) -> Dialog,
    ) {
        cursive.set_global_callback(event, move |cursive| {
            // This name must not be a valid C++ entity name to avoid namespace
            // collisions with profile layers.
            const GLOBAL_DIALOG_NAME: &str = "Global dialog";
            if cursive
                .screen_mut()
                .find_layer_from_name(GLOBAL_DIALOG_NAME)
                .is_none()
            {
                let dialog = dialog_factory(cursive).with_name(GLOBAL_DIALOG_NAME);
                cursive.add_layer(dialog);
            }
        });
    }

    // Q and Ctrl+C quit, after confirming that this is wanted
    fn quit_dialog(_: &mut Cursive) -> Dialog {
        Dialog::text("Ready to quit?")
            .button("Yes", Cursive::quit)
            .dismiss_button("No")
    }
    set_global_dialog_callback(&mut cursive, 'q', quit_dialog);
    set_global_dialog_callback(&mut cursive, Event::CtrlChar('c'), quit_dialog);

    // B displays an interactive backtrace
    set_global_dialog_callback(&mut cursive, 'b', backtrace_dialog);

    // Set up help text
    set_global_dialog_callback(&mut cursive, 'h', super::help_dialog);

    // Bubble up TUI state
    cursive
}

/// Load the ClangTrace with a pretty loading screen
pub fn wait_for_input(cursive: &mut CursiveRunnable) -> Result<(), ClangTraceLoadError> {
    // Set up the loading screen
    cursive.add_layer(Dialog::text("Processing input data...").button("Abort", Cursive::quit));

    // Initiate the cursive event loop
    let mut runner = cursive.runner();
    runner.refresh();
    let result = loop {
        // Process TUI events
        runner.step();

        // Abort input processing if instructed to do so
        if !runner.is_running() {
            std::mem::drop(runner);
            std::process::abort()
        }

        // Otherwise check how the input processing is going
        match super::with_state(&mut runner, |state| {
            state.processing_thread.try_extract_load_result()
        }) {
            None => continue,
            Some(result) => break result,
        }
    };

    // Clear screen layers before returning
    while !cursive.screen().is_empty() {
        cursive.pop_layer();
    }
    result
}

/// Exit the current cursive layer if there's another one underneath, keep the
/// profile layer tracking up to date while doing so.
fn exit_current_layer(cursive: &mut Cursive) {
    let num_layers = cursive.screen().len();
    if num_layers > 1 {
        super::with_state(cursive, |state| {
            // Profile layers are at the bottom of the cursive layer stack,
            // with dialogs residing on top of them, as we don't allow
            // spawning a profiling layer on top of a dialog.
            if num_layers == state.profile_stack.len() {
                state.profile_stack.pop();
            }
        });
        cursive.pop_layer();
    }
}

/// Interactive backtrace dialog
fn backtrace_dialog(cursive: &mut Cursive) -> Dialog {
    let mut select = SelectView::new();
    super::with_state(cursive, |state| {
        select.add_all(
            state
                .profile_stack
                .iter()
                .enumerate()
                .rev()
                .map(|(idx, (name, _norm))| (name.clone(), idx)),
        )
    });
    select.set_on_submit(|cursive, idx| {
        let screen = cursive.screen_mut();
        while screen.len() > idx + 1 {
            screen.pop_layer();
        }
        super::with_state(cursive, |state| state.profile_stack.truncate(idx + 1));
    });
    Dialog::around(select)
        .title("Backtrace")
        .dismiss_button("Ok")
}
