//! User interface module

use clang_time_trace::{ActivityTraceId, ClangTrace, ParsedActivityArgument};

mod display;
pub mod stdio;
pub mod tui;

/// Try to parse an activity argument using its intended logic
///
/// If that fails, log an error, then treat it as a string argument
///
fn force_parse_arg(trace: &mut ClangTrace, id: ActivityTraceId) -> ParsedActivityArgument {
    let raw_arg = trace.activity_trace(id).activity().raw_argument().clone();
    let detail = raw_arg.detail();
    match raw_arg.parse(trace) {
        Ok(parsed) => parsed,
        Err(e) => {
            log::error!("Failed to parse activity argument {detail:?} (got error {e}). Will treat it as a string.");
            if let Some(detail) = detail {
                ParsedActivityArgument::String(detail)
            } else {
                ParsedActivityArgument::Nothing
            }
        }
    }
}
