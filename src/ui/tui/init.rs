//! Initialization of the text user interface

use super::{trace, State};
use cursive::{
    event::{Event, Key},
    traits::Scrollable,
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
    cursive.set_global_callback('u', trace::switch_duration_unit);

    // We do not allow dialogs spawned by global keyboard shortcuts to stack on
    // top of each other as this is jarring and has no known use case.
    fn set_global_dialog_callback(
        cursive: &mut Cursive,
        event: impl Into<Event>,
        mut dialog_factory: impl 'static + FnMut(&mut Cursive) -> Option<Dialog>,
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
                if let Some(dialog) = dialog_factory(cursive) {
                    cursive.add_layer(dialog.with_name(GLOBAL_DIALOG_NAME));
                }
            }
        });
    }

    // Q and Ctrl+C quit, after confirming that this is wanted
    fn quit_dialog(_: &mut Cursive) -> Option<Dialog> {
        Some(
            Dialog::text("Ready to quit?")
                .button("Yes", Cursive::quit)
                .dismiss_button("No"),
        )
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

/// Exit the current cursive layer if there's another one underneath, keep the
/// profile layer tracking up to date while doing so.
fn exit_current_layer(cursive: &mut Cursive) {
    let num_layers = cursive.screen().len();
    if num_layers > 1 {
        let should_pop = super::with_state(cursive, |state| {
            // The normal UI layer stack goes roughly like this...
            // - Full build profiling (optional)
            // - Trace profile layers
            // - Dialogs (help, backtrace, quit prompt...)
            // ...but to complicate matters a bit, in place of profile layers,
            // there are occasionally progress dialogs that Esc shouldn't close.
            //
            // This can actually only happen during trace loading because for
            // full build ops the progress dialog is the bottom layer, which
            // exit_current_layer would never pop anyway.
            //
            // We handle this by asserting that no dialog should come atop those
            // and tracking whether a trace is in the process of being loaded.
            //
            if state.loading_trace {
                return false;
            }
            if num_layers == state.profile_stack.len() + state.layers_below_profile {
                state.profile_stack.pop();
            }
            true
        });
        if should_pop {
            cursive.pop_layer();
        }
    }
}

/// Interactive backtrace dialog
fn backtrace_dialog(cursive: &mut Cursive) -> Option<Dialog> {
    let mut select = SelectView::new();
    super::with_state(cursive, |state| {
        select.add_all(
            state
                .profile_stack
                .iter()
                .enumerate()
                .rev()
                .map(|(idx, profile)| (String::from(profile.table_name()), idx)),
        )
    });
    if select.is_empty() {
        return None;
    }
    select.set_on_submit(|cursive, idx| {
        let screen = cursive.screen_mut();
        while screen.len() > idx + 1 {
            screen.pop_layer();
        }
        super::with_state(cursive, |state| state.profile_stack.truncate(idx + 1));
    });
    Some(
        Dialog::around(select.scrollable())
            .title("Backtrace")
            .dismiss_button("Ok"),
    )
}
