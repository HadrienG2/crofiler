//! Interactions with clang

use std::{
    io,
    process::{Command, ExitStatus},
    string::FromUtf8Error,
};
use thiserror::Error;

/// Name of the clang executable
const CLANG_PROGRAM: &str = "clang++";

/// Minimal version of clang needed to support -ftime-trace and -ftime-trace-granularity
const MIN_VERSION: usize = 9;

/// Check for availability of a suitable version of clang++
///
/// If found, provide the program argument to be passed to std::Command::new()
/// when invoking clang. Otherwise, explain what went wrong.
///
pub fn find_clangpp() -> Result<impl AsRef<str>, ClangError> {
    let clang_version_output = match Command::new(CLANG_PROGRAM).arg("--version").output() {
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Err(ClangError::NotFound),
        other => other?,
    };
    if !clang_version_output.status.success() {
        return Err(ClangError::BadStatus(clang_version_output.status));
    }
    let stdout = String::from_utf8(clang_version_output.stdout)?;
    let version_line = match stdout.lines().next() {
        Some(line) => line,
        None => return Err(ClangError::EmptyVersion),
    };
    if let Some(version) = version_line.strip_prefix("clang version ") {
        if let Some((major_version, _)) = version.split_once('.') {
            if let Ok(major_version) = major_version.parse::<usize>() {
                if major_version >= MIN_VERSION {
                    return Ok(CLANG_PROGRAM);
                } else {
                    return Err(ClangError::AncientVersion(major_version));
                }
            }
        }
    }
    Err(ClangError::UnexpectedVersion(version_line.into()))
}

/// Error while looking up clang
#[derive(Debug, Error)]
pub enum ClangError {
    /// Could not find clang++
    #[error(
        "could not find clang++, please either make sure it is installed and \
        in PATH or point me to a pre-computed time-trace file"
    )]
    NotFound,

    /// Other I/O error during clang++ test run
    #[error("clang++ test run failed with an I/O error ({0})")]
    IoError(#[from] io::Error),

    /// Test run returned failing status
    #[error("clang++ test run returned failing status ({0})")]
    BadStatus(ExitStatus),

    /// Test run returned non-UTF-8 bytes
    #[error("clang++ test run returned non-UTF-8 bytes ({0})")]
    NotUtf8(#[from] FromUtf8Error),

    /// Version probe returned empty output
    #[error("clang++ --version returned empty output")]
    EmptyVersion,

    /// Version probe returned unexpected output
    #[error("clang++ --version returned unexpected output starting with {0:?}")]
    UnexpectedVersion(Box<str>),

    /// Clang version is too old
    #[error("clang++ is too old (need at least v9 for time-trace, got v{0})")]
    AncientVersion(usize),
}
