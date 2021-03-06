//! Easier C++ build profiling

#![deny(missing_docs)]

mod profile;
mod ui;

use atty::Stream::{Stdin, Stdout};
use clap::{ArgEnum, Parser};
use std::path::PathBuf;

/// Turn a clang time-trace dump into a profiler-like visualization
#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct CliArgs {
    /// Self-profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "0.5")]
    self_threshold: f32,

    /// Hierarchical profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "0.5")]
    hierarchical_threshold: f32,

    /// Maximal number of terminal columns to be used in the display
    ///
    /// Only used by the stdio display
    ///
    #[clap(short = 'c', long = "cols", default_value = "200")]
    max_cols: u16,

    /// Choice of user interface
    ///
    /// "tui" is an interactive TUI that requires a tty
    ///
    /// "stdio" is a non-interactive display that you can pipe to a file
    ///
    /// "auto" selectes the TUI if connected to a tty, otherwise "stdio"
    ///
    #[clap(short, long, default_value = "auto", arg_enum)]
    ui: UI,

    /// Clang time-trace file to be analyzed
    input: PathBuf,
}
//
/// Select desired user interface
#[derive(ArgEnum, Clone, Copy, Debug, Eq, PartialEq)]
pub enum UI {
    /// Use Tui if connected to a tty, otherwise stdio
    Auto,

    /// Use the interactive textual user interface
    Tui,

    /// Use the non-interactive stdio display
    Stdio,
}

fn main() {
    // Set up infrastructure and process CLI arguments
    env_logger::init();
    let args = CliArgs::parse();
    match args.ui {
        UI::Auto => {
            if atty::is(Stdin) && atty::is(Stdout) {
                ui::tui::run(args)
            } else {
                ui::stdio::run(args)
            }
        }
        UI::Tui => ui::tui::run(args),
        UI::Stdio => ui::stdio::run(args),
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use clang_time_trace::ClangTrace;
    use once_cell::sync::Lazy;
    use std::{str::FromStr, sync::Mutex};

    // Reference ClangTrace used by all tests which need one
    pub static TEST_TRACE: Lazy<Mutex<ClangTrace>> = Lazy::new(|| {
        Mutex::new(ClangTrace::from_str(include_str!("../tests/7-GMSTests_main.json")).unwrap())
    });
}
