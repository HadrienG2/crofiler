use crate::log::{LogClient, LogKey, LogLevel, LogThread, MessageKey};
use cmakeperf::{
    commands::{CompilationDatabase, DatabaseEntry},
    measure::{MeasureError, Measurement},
    output::BuildProfile,
};
use std::{
    collections::HashMap,
    fs::File,
    io::Write,
    num::NonZeroUsize,
    panic::AssertUnwindSafe,
    path::PathBuf,
    sync::{
        mpsc::{self, Receiver, RecvTimeoutError},
        Once,
    },
    time::{Duration, Instant},
};
use tempfile::{NamedTempFile, TempDir};

/// Job behavior expectation
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct JobProperties {
    /// Expected max-RSS in megabytes
    pub max_rss_mb: u64,

    /// Expected job duration
    pub wall_time: Duration,
}
//
impl JobProperties {
    /// Compute range of max-RSS in gigabytes expected in logs
    fn max_rss_range_logs(&self) -> (f32, f32) {
        let to_log_gb = |mb| (mb as f32 / 10.0).round() / 100.0;
        (
            to_log_gb(self.lower_max_rss_mb()),
            to_log_gb(self.upper_max_rss_mb()),
        )
    }

    /// Compute range of max-RSS in bytes expected in final output
    fn max_rss_range_output(&self) -> (u64, u64) {
        let to_bytes = |mb| mb * 1_000_000;
        (
            to_bytes(self.lower_max_rss_mb()),
            to_bytes(self.upper_max_rss_mb()),
        )
    }

    /// Compute range of wall-clock time expected in logs & final output
    fn wall_time_range(&self) -> (Duration, Duration) {
        (self.wall_time, self.wall_time + Duration::from_millis(150))
    }

    /// Lower expected measurement of max-RSS in megabytes
    ///
    /// Adds a lower margin to account for measurement error
    ///
    fn lower_max_rss_mb(&self) -> u64 {
        let measurement_precision = if cfg!(debug_assertions) { 5 } else { 1 };
        self.max_rss_mb.saturating_sub(measurement_precision)
    }

    /// Maximal expected value of max-RSS in megabytes
    ///
    /// Adds an upper margin to account for OS process overheads
    ///
    fn upper_max_rss_mb(&self) -> u64 {
        self.max_rss_mb + 5
    }
}

/// Test fixture for build profile measurement test
pub struct MeasurementTest {
    /// Temporary directory
    tmpdir: TempDir,

    /// List of compilation database entries
    db: CompilationDatabase,

    /// Expected measurement output
    output: HashMap<PathBuf, JobProperties>,
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
            db: CompilationDatabase::new(),
            output: HashMap::new(),
        }
    }

    /// Set up a mock compilation command performing certain actions
    ///
    /// Actions are described using the mini-language of the "mock" executable
    ///
    /// Returns the command's relative input file path, which will be used as an
    /// identifier in the measurement's output.
    ///
    // FIXME: Expand JobProperties to handle more complex cases
    pub fn with_job(mut self, actions: &str, resource_usage: JobProperties) -> Self {
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
        self.db
            .push(DatabaseEntry::new(self.tmpdir.path(), command, input_path));

        // Record expected output in the compilation database
        self.output.insert(rel_input_path, resource_usage);
        self
    }

    /// Start measuring the build performance of the mock compilation commands
    /// that were previously added via `add_job()`.
    pub fn start(self, measure_time: bool) -> RunningMeasurementTest {
        cmakeperf::measure::assume_oversubscription();
        if cfg!(debug_assertions) {
            cmakeperf::measure::PollClock::set_polling_interval(Duration::from_millis(150));
        }
        RunningMeasurementTest::start(self, measure_time)
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

    /// Imposed working directory when using `MeasurementTest`
    ///
    /// It is okay to impose a working directory in this multi-threaded test
    /// harness as long as all tests are actually agreeing on the same one.
    ///
    const WORKDIR: &'static str = env!("CARGO_TARGET_TMPDIR");
}

/// Running measurement job under a test harness
pub struct RunningMeasurementTest {
    /// Temporary directory where test files are located
    _tmpdir: TempDir,

    /// Output file path
    output_path: PathBuf,

    /// Expected results
    expected_output: HashMap<PathBuf, JobProperties>,

    /// Truth that wall-clock time is being measured
    measure_time: bool,

    /// Ongoing measurement
    measurement: Option<Measurement>,

    /// Events from the main thread
    events: Receiver<MeasurementEvent>,

    /// Worst-case timeout for jobs
    job_timeout: Duration,

    /// Measurement log tracking
    log_client: LogClient,
}
//
#[derive(Debug)]
enum MeasurementEvent {
    /// Progress has been reported on one task
    JobDone,

    /// Done measurint (this should come last)
    MeasurementDone(Result<(), MeasureError>),
}
//
impl RunningMeasurementTest {
    /// Start a measurement with the jobs previously added to `MeasurrementTest`
    fn start(test: MeasurementTest, measure_time: bool) -> Self {
        // Set up measurement output file
        let (_, output_path) = test.make_tmpfile();

        // Configure concurrency so that even in time-less measurements the
        // number of workers is deterministic + reduce CPU oversubscription
        let concurrency = if measure_time {
            None
        } else {
            NonZeroUsize::new(2)
        };

        // Find an upper bound on how long we should wait for a job to finish
        let job_timeout = test
            .output
            .values()
            .map(|res| res.wall_time + Self::TIMEOUT_MARGIN)
            .max()
            .unwrap_or(Self::TIMEOUT_MARGIN);

        // Set up measurement progress reporting
        let (events_in, events_out) = mpsc::channel();
        let events_in_1 = AssertUnwindSafe(events_in);
        let events_in_2 = events_in_1.clone();
        let job_done = move || {
            events_in_1
                .send(MeasurementEvent::JobDone)
                .expect("Test thread has panicked")
        };
        let measurement_done = move |res| {
            events_in_2
                .send(MeasurementEvent::MeasurementDone(res))
                .expect("Test thread has panicked")
        };

        // Start measurement
        let log_client = LogClient::new();
        let measurement = Some(Measurement::start(
            output_path.clone(),
            &test.db,
            measure_time,
            concurrency,
            job_done,
            measurement_done,
        ));
        Self {
            _tmpdir: test.tmpdir,
            output_path,
            measure_time,
            measurement,
            expected_output: test.output,
            events: events_out,
            job_timeout,
            log_client,
        }
    }

    /// Wait for the measurement to terminate
    // FIXME: Handle more complex cases
    pub fn finish(mut self) {
        if self.num_jobs() > 0 {
            self.wait_for_job();
            self.check_header_log(&self.log_client);
        }
        for _ in 1..self.num_jobs() {
            self.wait_for_job();
        }
        self.check_job_logs(&self.log_client);
        let result = self.wait_for_end();
        if self.num_jobs() == 0 {
            self.check_header_log(&self.log_client);
        }
        result.expect("Measurement should have succeeded");
    }

    /// Kill the measurement and make sure it terminates normally
    pub fn kill(mut self) {
        std::mem::drop(
            self.measurement
                .take()
                .expect("You can only kill the measurement once"),
        );
        let killed = Instant::now();
        let mut jobs_done = 0;
        loop {
            match self.events.recv_timeout(self.job_timeout) {
                Ok(MeasurementEvent::JobDone) => {
                    jobs_done += 1;
                    assert!(jobs_done < self.num_jobs(), "Too many jobs");
                }
                Ok(MeasurementEvent::MeasurementDone(Err(MeasureError::Killed))) => break,
                Ok(MeasurementEvent::MeasurementDone(other_res)) => {
                    panic!("Unexpected measurement result: {other_res:?}")
                }
                Err(RecvTimeoutError::Timeout) => panic!("Measurement thread is abnormally slow"),
                Err(RecvTimeoutError::Disconnected) => {
                    panic!("Measurement main thread quit before final report")
                }
            }
        }
        self.check_header_log(&self.log_client);
        let delay = killed.elapsed();
        assert!(
            delay < Self::TIMEOUT_MARGIN,
            "Measurement was killed abnormally slowly"
        );
    }

    /// Wait for the measurement thread to process a job
    fn wait_for_job(&mut self) {
        match self.events.recv_timeout(self.job_timeout) {
            Ok(MeasurementEvent::JobDone) => {}
            Ok(MeasurementEvent::MeasurementDone(_res)) => {
                panic!("Measurement done before all jobs are done")
            }
            Err(RecvTimeoutError::Timeout) => panic!("Measurement thread is abnormally slow"),
            Err(RecvTimeoutError::Disconnected) => {
                panic!("Measurement main thread quit before end of jobs")
            }
        }
    }

    /// Wait for the measurement thread to terminate once all jobs are done
    fn wait_for_end(&mut self) -> Result<(), MeasureError> {
        match self.events.recv_timeout(Self::TIMEOUT_MARGIN) {
            Ok(MeasurementEvent::JobDone) => panic!("Too many jobs from measurement thread"),
            Ok(MeasurementEvent::MeasurementDone(res)) => res.map(|()| self.check_output()),
            Err(RecvTimeoutError::Timeout) => panic!("Measurement thread is abnormally slow"),
            Err(RecvTimeoutError::Disconnected) => {
                panic!("Measurement main thread quit before final report")
            }
        }
    }

    /// Check for the first log emitted by the main thread announcing concurrency
    fn check_header_log(&self, log_client: &LogClient) {
        log_client.expect_log(LogKey {
            level: LogLevel::Info,
            thread: LogThread::Main,
            message: MessageKey::Exact::<fn(&str) -> bool>(if self.measure_time {
                "Will use 1 worker process(es)"
            } else {
                "Will use 2 worker process(es)"
            }),
        });
    }

    /// Check for the logs emitted by the main thread as it gets through jobs
    fn check_job_logs(&self, log_client: &LogClient) {
        for (rel_path, resource_usage) in &self.expected_output {
            let (min_rss_gb, max_rss_gb) = resource_usage.max_rss_range_logs();
            let (min_time, max_time) = resource_usage.wall_time_range();
            let rel_path_str = format!("{}", rel_path.display());
            let matcher_opt = |mut remainder: &str| -> Option<()> {
                // Check input file path
                remainder = remainder.strip_prefix("Compiled ")?;
                remainder = remainder.strip_prefix(&rel_path_str)?;
                remainder = remainder.strip_prefix(" (max-RSS ")?.strip_suffix(")")?;

                // Check max-RSS
                let (rss_gb_str, remainder) = remainder.split_once("GB, wall-time ")?;
                let rss_gb: f32 = rss_gb_str.parse().expect("Failed to parse max-RSS");
                if rss_gb < min_rss_gb || rss_gb > max_rss_gb {
                    return None;
                }

                // Handle case where no wall clock time measurement was produced
                if !self.measure_time {
                    return (remainder == "None").then_some(());
                }

                // Check wall clock time
                let time_str = remainder.strip_prefix("Some(Ok(")?.strip_suffix("))")?;
                let time_secs = if let Some(time_msecs_str) = time_str.strip_suffix("ms") {
                    time_msecs_str
                        .parse::<f32>()
                        .expect("Failed to parse milliseconds counter")
                        / 1000.0
                } else if let Some(time_secs_str) = time_str.strip_suffix("s") {
                    time_secs_str
                        .parse()
                        .expect("Failed to parse seconds counter")
                } else {
                    panic!("Failed to parse job duration")
                };
                let time = Duration::from_secs_f32(time_secs);
                (time >= min_time && time <= max_time).then_some(())
            };
            log_client.expect_log(LogKey {
                level: LogLevel::Info,
                thread: LogThread::Main,
                message: MessageKey::Approx(|message| matcher_opt(message) == Some(())),
            });
        }
    }

    /// Expected number of jobs
    fn num_jobs(&self) -> usize {
        self.expected_output.len()
    }

    /// Load build profile (after successful measurement)
    fn load_output(&self) -> BuildProfile {
        cmakeperf::output::load(&self.output_path).expect("Failed to load measurement output_path")
    }

    /// Check generated build profile (after measurement)
    fn check_output(&self) {
        let output = self.load_output();
        assert_eq!(
            output.len(),
            self.expected_output.len(),
            "Number of build profile entries does not match"
        );
        for unit in output {
            let expected_unit = self
                .expected_output
                .get(unit.rel_path())
                .expect("No build profile for job");

            let rss = unit.max_rss_bytes();
            let (min_rss, max_rss) = expected_unit.max_rss_range_output();
            assert!(
                rss >= min_rss && rss <= max_rss,
                "Job memory consumption {rss} does not fall within expected range [{min_rss}, {max_rss}]"
            );

            if !self.measure_time {
                assert_eq!(
                    unit.wall_time(),
                    None,
                    "Unexpected job wall-clock time measurement"
                );
                continue;
            }

            let time = unit
                .wall_time()
                .expect("Missing expected wall-clock time measurement")
                .expect("Invalid job wall-clock duration");
            let (min_time, max_time) = expected_unit.wall_time_range();
            assert!(
                time >= min_time && time <= max_time,
                "Job wall-clock duration {time:?} does not fall within expected range [{min_time:?}, {max_time:?}]"
            )
        }
    }

    /// Margin of error to be used when waiting for things that should be instant
    const TIMEOUT_MARGIN: Duration = Duration::from_millis(1000);
}
