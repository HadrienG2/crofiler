//! Easier C++ build profiling

#![deny(missing_docs)]

mod build;
mod profile;
mod ui;

use atty::Stream::{Stdin, Stdout};
use clap::{ArgEnum, Parser};
use std::path::PathBuf;

/// Analyze where your compilation time is spent in order to optimize it
///
/// If you run this program without an argument inside of your build directory,
/// it will offer you to measure the CPU and memory consumption of your entire
/// build and then individually study the heaviest compilation units.
///
/// Alternatively, if you already know which compilation unit you are interested
/// in, you can take a shortcut and directly point this program to the JSON file
/// produced by compiling it using clang with the `-ftime-trace` option enabled.
///
#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct CliArgs {
    /// Clang time-trace file to be analyzed
    ///
    /// If no input file is specified, will enter full-build profiling mode.
    /// This is only available when using the Text User Interface.
    ///
    input: Option<PathBuf>,

    /// Choice of user interface
    ///
    /// "tui" is an interactive Text User Interface (requires a terminal)
    ///
    /// "stdio" is a non-interactive display that you can pipe to a file
    ///
    /// "auto" selects the TUI if connected to a terminal, otherwise stdio
    ///
    #[clap(short, long, default_value = "auto", arg_enum)]
    ui: UI,

    /// Self-profile display threshold, as a percentage of total duration
    ///
    /// Only used by the stdio user interface, the TUI displays everything.
    ///
    #[clap(short, long, default_value = "0.5")]
    self_threshold: f32,

    /// Hierarchical profile display threshold, as a percentage of total duration
    ///
    /// Only used by the stdio user interface, the TUI displays everything.
    ///
    #[clap(short, long, default_value = "0.5")]
    hierarchical_threshold: f32,

    /// Maximal number of terminal columns to be used in the display
    ///
    /// Only used by the stdio user interface, the TUI detects the screen width.
    ///
    #[clap(short = 'c', long = "cols", default_value = "200")]
    max_cols: u16,

    /// Path to the full-build profile
    ///
    /// This is used in full-build profiling mode to directly display the build
    /// profile instead of measuring it first.
    ///
    /// If this is not specified, it will default to "cmakeperf.csv". And
    /// if the profile does not exist yet, you will be provided with the option
    /// to measure it.
    ///
    /// Full-build profiling is only available when using the TUI.
    ///
    #[clap(long)]
    build_profile: Option<PathBuf>,
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
    use once_cell::unsync::Lazy;
    use std::{cell::RefCell, str::FromStr};

    // Reference ClangTrace used by all tests which need one
    thread_local! {
        pub static TEST_TRACE: RefCell<Lazy<ClangTrace>> = RefCell::new(Lazy::new(|| {
            ClangTrace::from_str(include_str!("../tests/7-GMSTests_main.json")).unwrap()
        }));
    }
}
