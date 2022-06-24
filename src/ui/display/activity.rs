//! Utilities for displaying clang activities

use clang_time_trace::{ActivityArgument, ActivityTrace, ClangTrace, CustomDisplay, MangledSymbol};
use std::io;
use thiserror::Error;
use unicode_width::UnicodeWidthStr;

/// Try to display an activity's name and argument in finite space
///
/// Returns Err(NotEnoughCols) if not even the activity name can fit in that
/// space. You may want to retry after eliminating other display elements if
/// they are deemed less important, or just display "…".
///
pub fn display_activity_id(
    mut output: impl io::Write,
    trace: &ClangTrace,
    activity_trace: &ActivityTrace,
    mut max_cols: u16,
) -> Result<(), ActivityIdError> {
    let activity_name = activity_trace.activity().name();
    let activity_arg = activity_trace.activity().argument();

    // Can we display at least ActivityName(…)?
    if usize::from(max_cols) < activity_name.width() + 3 {
        // If not, error out
        return Err(ActivityIdError::NotEnoughCols(max_cols));
    } else {
        // If so, display the activity name...
        write!(output, "{activity_name}")?;
    }
    // ...and account for the reserved space
    max_cols -= activity_name.width() as u16 + 2;

    // Display the activity argument
    match activity_arg {
        ActivityArgument::Nothing | ActivityArgument::MangledSymbolOpt(None) => {}
        ActivityArgument::String(s)
        | ActivityArgument::MangledSymbol(MangledSymbol::Demangled(s))
        | ActivityArgument::MangledSymbol(MangledSymbol::Mangled(s))
        | ActivityArgument::MangledSymbolOpt(Some(MangledSymbol::Demangled(s)))
        | ActivityArgument::MangledSymbolOpt(Some(MangledSymbol::Mangled(s))) => {
            if s.width() <= max_cols.into() {
                write!(output, "({s})")?;
            } else {
                write!(output, "({})", super::truncate_string(&s, max_cols))?;
            }
        }
        ActivityArgument::FilePath(p) => {
            write!(
                output,
                "({})",
                super::path::truncate_path(&trace.file_path(p), max_cols)
            )?;
        }
        ActivityArgument::CppEntity(e)
        | ActivityArgument::MangledSymbol(MangledSymbol::Parsed(e))
        | ActivityArgument::MangledSymbolOpt(Some(MangledSymbol::Parsed(e))) => {
            write!(output, "({})", trace.entity(e).bounded_display(max_cols))?;
        }
    }
    Ok(())
}
//
/// Error that is emitted when an activity id cannot be displayed
#[derive(Debug, Error)]
pub enum ActivityIdError {
    /// Not enough space to display activity name
    #[error("cannot display activity name in {0} terminal column(s)")]
    NotEnoughCols(u16),

    /// Output device errored out
    #[error("failed to write to output device ({0})")]
    IoError(#[from] io::Error),
}
