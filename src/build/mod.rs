//! Build-wide analysis facilities

pub mod clang;
pub mod commands;
pub mod profile;

use std::{
    io,
    path::Path,
    time::{Duration, SystemTimeError},
};
use thiserror::Error;

/// Measure how log ago a file was last modified
pub fn file_age(path: impl AsRef<Path>) -> Result<Duration, FileAgeError> {
    let age = std::fs::metadata(path)?.modified()?.elapsed()?;
    Ok(age)
}

/// Error while looking up the age of a file
#[derive(Debug, Error)]
pub enum FileAgeError {
    /// Failed to access the file
    #[error("failed to access the file ({0})")]
    BadIo(#[from] io::Error),

    /// Queried system time looks wrong
    #[error("modification timestamp is from the future ({0})")]
    BadClock(#[from] SystemTimeError),
}
