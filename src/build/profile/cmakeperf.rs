//! Measure full-build profile using cmakeperf tool
//!
//! This may eventually be replaced with custom code for the sake of improved
//! user experience (no need to install an extra Python package), so keep it
//! as encapsulated as possible.

use crate::build;
use log::info;
use std::{
    io::{self, BufRead, BufReader, ErrorKind, Read},
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

/// Ongoing build profile collection
pub struct Collect {
    /// Child cmakeperf process
    child: Child,

    /// Buffering stdout reader
    stdout: BufReader<ChildStdout>,

    /// Line buffer
    buf: String,
}
//
impl Collect {
    /// Start collecting a build profile to a specified location
    pub fn start(path: impl AsRef<Path>) -> io::Result<Self> {
        let mut child = Command::new(PROGRAM)
            .args(["collect", "--interval", POLLING_INTERVAL, "-o"])
            .arg(path.as_ref())
            .arg(build::commands::LOCATION)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let stdout = BufReader::new(
            child
                .stdout
                .take()
                .expect("Failed to access piped child stdout"),
        );
        Ok(Self {
            child,
            stdout,
            buf: String::new(),
        })
    }

    /// Wait for the next step in the build profile collection process
    pub fn wait_next_step(&mut self) -> Result<CollectStep, CollectError> {
        self.buf.clear();
        match self.stdout.read_line(&mut self.buf) {
            Err(e) if e.kind() == ErrorKind::BrokenPipe => return self.finish(),
            Ok(0) => return self.finish(),
            other => other?,
        };
        info!("cmakeperf output: {}", self.buf.trim());
        Ok(CollectStep::FileCompiled)
    }

    /// An stdout read failed, check out the final process status
    pub fn finish(&mut self) -> Result<CollectStep, CollectError> {
        let exit_status = self.child.wait()?;
        if !exit_status.success() {
            let mut stderr = self
                .child
                .stderr
                .take()
                .expect("Failed to access piped child stderr");
            self.buf.clear();
            stderr.read_to_string(&mut self.buf)?;
            let stderr: &str = &self.buf;
            return Err(CollectError::BadExit(exit_status, stderr.into()));
        }
        Ok(CollectStep::Finished)
    }
}
//
/// Step in the cmakeperf profile collection process
pub enum CollectStep {
    /// Done compiling a source file
    FileCompiled,

    /// Successfully finished measuring the build profile
    Finished,
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

/// Command used for running cmakeperf
const PROGRAM: &str = "cmakeperf";

/// Polling interval in seconds
///
/// This is enough to get execution times with 0.1s precision, and seems
/// empirically to also be enough for good RAM measurements.
///
const POLLING_INTERVAL: &str = "0.03";
