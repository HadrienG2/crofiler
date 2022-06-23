//! Easier C++ build profiling

#![deny(missing_docs)]

mod display;
mod profile;

use crate::display::stdio;
use clap::Parser;
use std::path::PathBuf;

/// Turn a clang time-trace dump into a profiler-like visualization
#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Args {
    /// Maximal number of terminal columns to be used in the display
    ///
    /// Only used by the stdio display
    ///
    #[clap(short = 'c', long = "cols", default_value = "200")]
    max_cols: u16,

    /// Self-profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "0.5")]
    self_threshold: f32,

    /// Hierarchical profile display threshold, as a percentage of total duration
    #[clap(short, long, default_value = "0.5")]
    hierarchical_threshold: f32,

    /// Clang time-trace file to be analyzed
    input: PathBuf,
    //
    // FIXME: After resolving the stdio-isms, add a CLI switch to flip between
    // stdio and tui, and start making a tui w/ cursive and cursive_table_view.
}

fn main() {
    // Set up infrastructure and process CLI arguments
    env_logger::init();
    let args = Args::parse();
    stdio::run_on_stdio(args)
}
