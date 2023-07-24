//! Checking logs from build performance measurements

use log::{LevelFilter, Log};
use once_cell::sync::Lazy;
use simplelog::{CombinedLogger, Config, SharedLogger, TestLogger};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    sync::{Mutex, MutexGuard},
};

pub use log::Level as LogLevel;

/// Client handle to the global log collector
///
/// Each integration test should set up one of these before starting a
/// measurement, notify it about any expected log message, and drop it when
/// the measurement is done. This will check that expected logs are indeed
/// produced, and detect production of unexpected logs whenever a quiescent
/// state where no measurement is running is reached.
///
/// As these quiescent states may only be reached some time after the accident
/// has actually happened in multi-threaded settings, you may find it useful
/// to re-run tests in sequential mode (`--test-threads=1`) when unexpected logs
/// are detected, for precise detection inside the test that produced the log.
///
pub struct LogClient;
//
impl LogClient {
    /// Notify the log collector that a new log-producing test is starting
    pub fn new() -> LogClient {
        LogCollector::register_client()
    }

    /// Notify the log collector that we expect a log to have been produced
    ///
    /// This will assert that a matching log has indeed been produced, and
    /// if so remove it from the list of observed logs.
    ///
    pub fn expect_log(&self, key: LogKey<impl FnMut(&str) -> bool>) {
        let mut lock = LogCollector::instance();
        match key.message {
            // Exact log match : find and remove a matching log in LogCollector
            MessageKey::Exact(message) => lock.remove(LogData {
                level: key.level,
                thread: key.thread,
                message: message.to_owned(),
            }),

            // Approximate log match using a regex of the message
            MessageKey::Approx(mut matcher) => {
                // Iterate over logs matching this query
                let key_matches = |data: &&LogData| {
                    data.level == key.level && data.thread == key.thread && matcher(&data.message)
                };
                let mut matching_keys = lock.logs.keys().filter(key_matches);

                // There should be at least one, otherwise the test failed
                let Some(matching_key) = matching_keys.next() else {
                    panic!(
                        "No log matching expectation inside log data {:#?}",
                        lock.logs
                    );
                };

                // The current implementation does not support regexes with
                // multiple matches as ambiguity resolution would require
                // significant extra code complexity.
                if matching_keys.next().is_some() {
                    unimplemented!("Message ambiguity resolution is not implemented");
                } else {
                    let exact_key = LogData {
                        level: key.level,
                        thread: key.thread,
                        message: matching_key.message.to_owned(),
                    };
                    lock.remove(exact_key);
                }
            }
        }
    }
}
//
impl Drop for LogClient {
    fn drop(&mut self) {
        LogCollector::drop_client()
    }
}

/// Search query to look up a log
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LogKey<'a, Matcher: FnMut(&str) -> bool = fn(&str) -> bool> {
    /// Verbosity level of the message
    pub level: LogLevel,

    /// What kind of thread emitted the message
    pub thread: LogThread,

    /// Message string matching criterion
    pub message: MessageKey<'a, Matcher>,
}

/// Identity of the thread that emitted a log message
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogThread {
    /// Shared facility from root module
    Root,

    /// Main thread
    Main,

    /// Worker thread
    Worker,
}

/// Log message search query
#[derive(Clone, Debug)]
pub enum MessageKey<'a, Matcher: FnMut(&str) -> bool = fn(&str) -> bool> {
    /// Exact match
    Exact(&'a str),

    /// Approximate match
    ///
    /// Used for cases where the message cannot be exactly predicted, typically
    /// for resource usage numbers like execution times. Beware that ambiguity
    /// resolution is not supported, so if multiple logs match this query, the
    /// test will fail. Be as specific as possible and make tests varied!
    ///
    Approx(Matcher),
}
//
impl<Matcher: PartialEq + FnMut(&str) -> bool> PartialEq for MessageKey<'_, Matcher> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MessageKey::Exact(e1), MessageKey::Exact(e2)) => e1.eq(e2),
            (MessageKey::Approx(a1), MessageKey::Approx(a2)) => a1.eq(a2),
            _ => false,
        }
    }
}
//
impl<Matcher: Eq + FnMut(&str) -> bool> Eq for MessageKey<'_, Matcher> {}

/// Global log collector
///
/// Tracks how many integration tests should currently be producing measurement
/// logs, and which logs they have produced so far. Makes sure that every
/// produced measurement log, at the exclusion of any other log, is expected via
/// the `LogClient` interface.
///
struct LogCollector {
    /// Number of integration tests performing measurements (that may produce logs)
    active_clients: usize,

    /// Logs produced by threads and waiting to be read out, duplicates are counted
    logs: HashMap<LogData, usize>,
}
/// Recorded data about a previously emitted log
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct LogData {
    /// Verbosity level of the message
    level: LogLevel,

    /// What kind of thread emitted the message
    thread: LogThread,

    /// What was the message string
    message: String,
}
//
static LOG_COLLECTOR: Lazy<Mutex<LogCollector>> = Lazy::new(|| {
    // Set up a global logger that logs to stderr and also feeds logs into the
    // global LogCollector
    CombinedLogger::init(vec![
        TestLogger::new(LevelFilter::Debug, Config::default()),
        Box::new(LogCollectorHandle),
    ])
    .expect("Failed to initialize logger");

    // Initial log collector state
    Mutex::new(LogCollector {
        active_clients: 0,
        logs: HashMap::new(),
    })
});
//
impl LogCollector {
    /// Take note that a new integration test may be producing logs
    fn register_client() -> LogClient {
        let mut lock = Self::instance();
        lock.active_clients += 1;
        LogClient
    }

    /// Take note that an integration test is done, should not produce logs
    /// anymore, and should have checked all of its logs.
    fn drop_client() {
        let mut lock = Self::instance();
        assert_ne!(
            lock.active_clients, 0,
            "Current test was not registered properly"
        );
        lock.active_clients -= 1;
        if lock.active_clients == 0 {
            lock.reached_quiescent_state();
        }
    }

    /// Call this in quiescent states where no measurement is running
    ///
    /// In quiescent states, all logs should have been expected by clients. We
    /// detect unexpected logs, or conversely expected logs that weren't detected.
    ///
    fn reached_quiescent_state(&mut self) {
        assert!(
            self.logs.is_empty(),
            "Integration test(s) produced unexpected logs: {:#?}",
            self.logs
        );
    }

    /// Remove one occurence of a log from the record
    fn remove(&mut self, key: LogData) {
        match self.logs.entry(key) {
            Entry::Occupied(mut entry) => {
                let new_refcount = *entry.get() - 1;
                if new_refcount == 0 {
                    entry.remove();
                } else {
                    *entry.get_mut() = new_refcount;
                }
            }
            Entry::Vacant(v) => panic!("No log matching expectation {:#?}", v.key()),
        }
    }

    /// Acquire access to the global log collector, setting it up as needed
    fn instance() -> MutexGuard<'static, Self> {
        LOG_COLLECTOR.lock().expect("Log collector was poisoned")
    }
}

/// Logger that catches measurement logs and feeds them to the LogCollector
struct LogCollectorHandle;
//
impl Log for LogCollectorHandle {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        if !metadata.target().starts_with("cmakeperf::measure") {
            return false;
        }
        // Change this AND SharedLogger::level() if more verbose log levels end
        // up being produced by build profile measurements someday
        assert!(metadata.level() <= LogLevel::Info);
        true
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            // record.target() is currently redundant with module paths and
            // therefor we only need to consider one.
            assert_eq!(record.module_path(), Some(record.target()));

            // Extract the test-friendly subset of log information
            let level = record.level();
            let submodule_path = record
                .target()
                .strip_prefix("cmakeperf::measure")
                .expect("Should have been filtered out by enabled()");
            let thread = match submodule_path {
                "" => LogThread::Root,
                "::main" => LogThread::Main,
                "::worker" => LogThread::Worker,
                _ => unreachable!("No other module should emit logs"),
            };
            let message = format!("{}", record.args());

            // Access the log collector
            let mut lock = LogCollector::instance();

            // Check that a log was expected
            assert_ne!(
                lock.active_clients, 0,
                "No measurement logs expected at this time"
            );

            // Increment this log's appearance count
            *lock
                .logs
                .entry(LogData {
                    level,
                    thread,
                    message,
                })
                .or_insert(0) += 1;
        }
    }

    fn flush(&self) {}
}
//
impl SharedLogger for LogCollectorHandle {
    fn level(&self) -> LevelFilter {
        LevelFilter::Info
    }

    fn config(&self) -> Option<&Config> {
        None
    }

    fn as_log(self: Box<Self>) -> Box<dyn Log> {
        self as _
    }
}
