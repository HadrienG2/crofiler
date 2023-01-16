//! Tests of the compilation profile measurement facility

mod log;

use self::log::{LogClient, LogKey, LogLevel, LogThread, MessageKey};
use assert_matches::assert_matches;
use cmakeperf::{
    commands::{CompilationDatabase, DatabaseEntry},
    measure::{MeasureError, Measurement},
    output::BuildProfile,
};
use regex::Regex;
use std::{
    fs::File,
    io::Write,
    panic::AssertUnwindSafe,
    path::PathBuf,
    sync::{
        mpsc::{self, Receiver, RecvTimeoutError},
        Once,
    },
    time::Duration,
};
use tempfile::{NamedTempFile, TempDir};

/// Test fixture for build profile measurement test
struct MeasurementTest {
    /// Temporary directory
    tmpdir: TempDir,

    /// List of compilation database entries
    entries: Vec<DatabaseEntry>,
}
//
impl MeasurementTest {
    /// Set up the compilation database builder and associated working directory
    ///
    /// A working directory must be imposed while running these tests because
    /// the measurement output uses relative file paths as job identifiers...
    ///
    pub fn new() -> Self {
        static SET_WORKDIR: Once = Once::new();
        SET_WORKDIR.call_once(|| {
            std::env::set_current_dir(Self::WORKDIR).expect("Failed to set working directory")
        });
        let tmpdir = TempDir::new_in(Self::WORKDIR).expect("Failed to create temporary directory");
        Self {
            tmpdir,
            entries: Vec::new(),
        }
    }

    /// Set up a mock compilation command performing certain actions
    ///
    /// Actions are described using the mini-language of the "mock" executable
    ///
    /// Returns the command's relative input file path, which will be used as an
    /// identifier in the measurement's output.
    ///
    // TODO: Specify expected wall time and RSS, and keep that in a hashmap so
    //       that SyncMeasurement can take care of more stuff.
    pub fn add_job(&mut self, actions: &str) -> PathBuf {
        // Name of the mock executable
        const MOCK_EXE: &'static str = env!("CARGO_BIN_EXE_mock");

        // Set up a command file, compute path relative to command workdir
        let (mut cmd_file, cmd_path) = self.make_tmpfile();
        writeln!(cmd_file, "{actions}").expect("Failed to write commands to file");
        let rel_cmd_path = pathdiff::diff_paths(&cmd_path, self.tmpdir.path())
            .expect("Failed to compute relative command file path");

        // Create a mock input file, compute path relative to main thread workdir
        let (_, input_path) = self.make_tmpfile();
        let rel_input_path = pathdiff::diff_paths(&input_path, Self::WORKDIR)
            .expect("Failed to compute relative input path");

        // Generate a compilation database entry running that command file
        let command = format!("{MOCK_EXE} {}", rel_cmd_path.display());
        self.entries
            .push(DatabaseEntry::new(self.tmpdir.path(), command, input_path));
        rel_input_path
    }

    /// Start measuring the build performance of the mock compilation commands
    /// that were previously added via `add_job()`.
    pub fn start(&mut self, measure_time: bool) -> SyncMeasurement {
        let (_, output_path) = self.make_tmpfile();
        let (events_in, events_out) = mpsc::channel();
        let events_in_1 = AssertUnwindSafe(events_in);
        let events_in_2 = events_in_1.clone();
        let measurement = Measurement::start(
            output_path.clone(),
            &self.make_db(),
            measure_time,
            None,
            move || {
                events_in_1
                    .send(MeasurementEvent::JobDone)
                    .expect("Test thread has panicked")
            },
            move |res| {
                events_in_2
                    .send(MeasurementEvent::MeasurementDone(res))
                    .expect("Test thread has panicked")
            },
        );
        SyncMeasurement {
            output_path,
            measurement: Some(measurement),
            events: events_out,
        }
    }

    /// Create a named temporary file
    //
    // Since self.tmpdif is temporary, we could use a regular file in it. We're
    // just reusing the tempfile crate here for its random filename picking
    // logic. Hence the use of `keep()`, as we don't need another layer of
    // automatic deletion.
    //
    fn make_tmpfile(&self) -> (File, PathBuf) {
        let tmpfile = NamedTempFile::new_in(&self.tmpdir).expect("Failed to create temporary file");
        let (file, tmppath) = tmpfile.into_parts();
        (
            file,
            tmppath.keep().expect("Failed to persistify temporary file"),
        )
    }

    /// Generate a compilation database with previously added commands
    fn make_db(&mut self) -> CompilationDatabase {
        CompilationDatabase::from_entries(self.entries.drain(..))
    }

    /// Imposed working directory when using `MeasurementTest`
    ///
    /// It is okay to impose a working directory in this multi-threaded test
    /// harness as long as all tests are actually agreeing on the same one.
    ///
    const WORKDIR: &'static str = env!("CARGO_TARGET_TMPDIR");
}

/// Like Measurement, but with a nicer synchronous steering API
struct SyncMeasurement {
    /// Output file path
    output_path: PathBuf,

    /// Ongoing measurement
    measurement: Option<Measurement>,

    /// Events from the main thread
    events: Receiver<MeasurementEvent<()>>,
}
//
#[derive(Debug)]
enum MeasurementEvent<Output> {
    /// Progress has been reported on one task
    JobDone,

    /// Done measurint (this should come last)
    MeasurementDone(Result<Output, MeasureError>),
}
//
impl SyncMeasurement {
    /// Wait for the measurement thread to make progress
    ///
    /// In this simplified integration testing scenario, we know how long
    /// jobs should take, so we can set a timeout that leads to test failure
    /// if exceeded to simplify the API.
    ///
    // TODO: Make this private, replace with a finish() that checks all waits and the final result
    pub fn wait(&mut self, timeout: Duration) -> Option<MeasurementEvent<BuildProfile>> {
        match self.events.recv_timeout(timeout) {
            Ok(MeasurementEvent::JobDone) => Some(MeasurementEvent::JobDone),
            Ok(MeasurementEvent::MeasurementDone(res)) => Some(MeasurementEvent::MeasurementDone(
                res.map(|()| self.load_results()),
            )),
            Err(RecvTimeoutError::Timeout) => panic!("Measurement thread is abnormally slow"),
            Err(RecvTimeoutError::Disconnected) => None,
        }
    }

    /// Send measurement the kill signal and wait for it to terminate
    pub fn kill(&mut self) {
        std::mem::drop(
            self.measurement
                .take()
                .expect("You can only kill the measurement once"),
        )
    }

    /// Load measurement results
    fn load_results(&self) -> BuildProfile {
        cmakeperf::output::load(&self.output_path).expect("Failed to load measurement results")
    }
}

/// Margin of error to be used when waiting for things that should be instant
pub const TIMEOUT_MARGIN: Duration = Duration::from_millis(100);

/// Margin of error to be used for process RSS (max base process overhead)
pub const RSS_BYTES_MARGIN: u64 = 10_000_000;

// TODO: Move the above into utility modules once API is stabilized

#[test]
fn basic_hog() {
    // Define test job and set expectations
    let mut test = MeasurementTest::new();
    let rel_path = test.add_job("hog 20M:0.2s 100M:0.2s 10M:0.2s");
    let min_duration = Duration::from_millis(600);
    let min_rss_bytes = 100_000_000;

    let output = {
        // Start the job
        let log_client = LogClient::new();
        let mut measurement = test.start(true);

        // Make sure it produces the expected events at the expected rate
        // TODO: Make a general utility for this that takes as input a set of
        //       expected job timings, keeps waiting for the max remaining durations,
        //       and makes sure the wait times make sense by keeping a sorted list
        //       of remaining expected timings and cutting the shortest time that matches.
        assert_matches!(
            measurement.wait(min_duration + TIMEOUT_MARGIN),
            Some(MeasurementEvent::JobDone)
        );
        log_client.expect_log(LogKey {
            level: LogLevel::Info,
            thread: LogThread::Main,
            message: MessageKey::Exact("Will use 1 worker process(es)"),
        });
        log_client.expect_log(LogKey {
            level: LogLevel::Info,
            thread: LogThread::Main,
            message: MessageKey::Regex(
                &Regex::new(&format!(
                    "^Compiled {} \\(\
                    max-RSS 0\\.1[0-9]GB, \
                    wall-time Some\\(Ok\\(6[0-9][0-9]\\.[0-9]+ms\\)\\)\\)",
                    rel_path.display()
                ))
                .expect("Failed to compile regex"),
            ),
        });
        //
        let output = assert_matches!(
            measurement.wait(TIMEOUT_MARGIN),
            Some(MeasurementEvent::MeasurementDone(Ok(output))) => output
        );
        assert_matches!(measurement.wait(TIMEOUT_MARGIN), None);
        output
    };

    // Check output build profile
    // TODO: Extract a general utility for this
    assert_eq!(output.len(), 1);
    assert_eq!(output[0].rel_path(), rel_path);
    assert!(output[0].max_rss_bytes() >= min_rss_bytes);
    let max_rss_bytes = min_rss_bytes + RSS_BYTES_MARGIN;
    assert!(output[0].max_rss_bytes() < max_rss_bytes);
    let wall_time = output[0]
        .wall_time()
        .expect("Missing wall-clock time data")
        .expect("Incorrect wall-clock time metadata");
    assert!(wall_time >= min_duration);
    let max_duration = min_duration + TIMEOUT_MARGIN;
    assert!(wall_time < max_duration);
}

// TODO: Use simplelog to capture and check logs into a global HashSet of
//       reproducible lines (e.g. no timestamps). Provide tests with a way to
//       notify when they start emitting logs and when they stop doing so.
//       Everytime there is no active test, check there is no remaining log.
//       Design APIs such that it's impossible to forget to check in and out of
//       the global logger (e.g. MeasurementTest::start checks in, and upcoming
//       SyncMeasurment::finish checks out after joining the thread.

// TODO: Test with multi-layered
//       process trees, exit codes, various memory consumption patterns and
//       stdout/stderr output, killing jobs before completion (check delay !)
