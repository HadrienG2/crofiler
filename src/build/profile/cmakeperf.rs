//! Measure full-build profile using cmakeperf tool
//!
//! This may eventually be replaced with custom code for the sake of improved
//! user experience (no need to install an extra Python package), so keep it
//! as encapsulated as possible.

use crate::build::commands::CompilationDatabase;

use std::{
    io::{self, BufRead, BufReader, Read},
    path::Path,
    process::{Child, ChildStdout, Command, ExitStatus, Stdio},
};
use thiserror::Error;

/// Check for cmakeperf's presence
pub fn find() -> io::Result<ExitStatus> {
    Command::new(PROGRAM)
        .arg("--help")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
}

/// Ongoing build profile collection process
pub struct Collect(Child);
//
impl Collect {
    /// Start collecting a build profile to a specified location
    pub fn start(path: impl AsRef<Path>) -> io::Result<(Self, Output)> {
        let mut child = Command::new(PROGRAM)
            .args(["collect", "--interval", POLLING_INTERVAL, "-o"])
            .arg(path.as_ref())
            .arg(CompilationDatabase::location())
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let stdout = BufReader::new(
            child
                .stdout
                .take()
                .expect("Should succeed because stdout is Stdio::piped()"),
        );
        Ok((
            Self(child),
            Output {
                stdout,
                line_buffer: String::new(),
                after_first: false,
            },
        ))
    }

    /// Abort the data collection process
    pub fn kill(&mut self) -> io::Result<()> {
        self.0.kill()
    }

    /// Wait for the process to complete and check out the final process status
    pub fn finish(&mut self) -> Result<(), CollectError> {
        let exit_status = self.0.wait()?;
        if !exit_status.success() {
            let mut stderr = self
                .0
                .stderr
                .take()
                .expect("Should succeed because we finish only once");
            let mut buf = String::new();
            stderr.read_to_string(&mut buf)?;
            return Err(CollectError::BadExit(exit_status, buf.into()));
        }
        Ok(())
    }
}
//
/// Error in the cmakeperf profile collection process
#[derive(Debug, Error)]
pub enum CollectError {
    /// General operating system error
    #[error("failed to communicate with cmakeperf process ({0})")]
    Io(#[from] io::Error),

    /// Process ran through with a failing exit status
    #[error("cmakeperf run failed ({0}) with the following stderr output:\n---\n{1}---")]
    BadExit(ExitStatus, Box<str>),
}

/// Output of the build profile colection process
pub struct Output {
    /// Standard output handle
    stdout: BufReader<ChildStdout>,

    /// Buffer to collect standard output lines
    line_buffer: String,

    /// Truth that the first line of output has been consumed
    ///
    /// The first line of cmakeperf output repeats configuration, it does not
    /// indicate that a compilation unit has been taken care of, unlike
    /// subsequent lines of output
    ///
    after_first: bool,
}
//
impl Output {
    /// Wait for the next compilation step
    pub fn next_step(&mut self) -> io::Result<BuildStep> {
        self.line_buffer.clear();
        match self.stdout.read_line(&mut self.line_buffer) {
            Ok(0) => Ok(BuildStep::BuildDone),
            Ok(_nonzero) => {
                log::info!("cmakeperf output: {}", self.line_buffer.trim());
                if self.after_first {
                    Ok(BuildStep::UnitDone)
                } else {
                    self.after_first = true;
                    self.next_step()
                }
            }
            Err(e) if e.kind() == io::ErrorKind::BrokenPipe => Ok(BuildStep::BuildDone),
            Err(other_error) => Err(other_error),
        }
    }
}
//
/// Elapsed build step
pub enum BuildStep {
    /// One compilation unit has been dealt with
    UnitDone,

    /// The full build profiling process is done
    BuildDone,
}

/// Command used for running cmakeperf
const PROGRAM: &str = "cmakeperf";

/// Polling interval in seconds
///
/// This is enough to get execution times with 0.1s precision, and seems
/// empirically to also be enough for good RAM measurements.
///
const POLLING_INTERVAL: &str = "0.03";
