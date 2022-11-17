//! Measure full-build profile

mod main;
mod worker;

use self::{main::MonitorServer, worker::MonitorClient};
use crate::commands::{CompilationDatabase, DatabaseEntry};
use crossbeam_deque::{Injector, Stealer, Worker};
use crossbeam_utils::CachePadded;
use std::{
    num::NonZeroUsize,
    panic::UnwindSafe,
    path::Path,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, RwLock,
    },
    thread::JoinHandle,
    time::Duration,
};
use sysinfo::{System, SystemExt};

pub use self::main::MeasureError;

/// Ongoing full-build profile measurement process
pub struct Measurement {
    /// Handle to the main measurement thread
    main_thread: Option<JoinHandle<()>>,

    /// Mechanism to tell all threads to stop
    kill: Arc<AtomicBool>,
}
//
impl Measurement {
    /// Start measuring a full-build profile
    ///
    /// `path` indicates where the profile should be saved.
    ///
    /// Measuring elapsed time per compilation unit is optional, as is
    /// specifying the build concurrency. The default build concurrency is
    /// 1 process (sequential build) if elapsed time is requested, one process
    /// per CPU hyperthread if .
    ///
    pub fn start(
        path: impl Into<Box<Path>>,
        database: &CompilationDatabase,
        measure_time: bool,
        concurrency: Option<NonZeroUsize>,
        step_done: impl FnMut() + UnwindSafe + Send + 'static,
        build_done: impl FnOnce(Result<(), MeasureError>) + Send + 'static,
    ) -> Self {
        let path = path.into();
        let jobs = database.entries().cloned().collect::<Vec<_>>();
        let kill = Arc::new(AtomicBool::new(false));
        let kill2 = kill.clone();
        let main_thread = Some(std::thread::spawn(move || {
            build_done(main::run(
                path,
                jobs,
                measure_time,
                concurrency,
                step_done,
                kill2,
            ))
        }));
        Self { main_thread, kill }
    }
}
//
impl Drop for Measurement {
    fn drop(&mut self) {
        self.kill.store(true, Ordering::Relaxed);
        for main_thread in self.main_thread.take() {
            std::mem::drop(main_thread.join());
        }
    }
}

/// Work queue from which worker threads fetch work
struct WorkQueue {
    /// Global work input
    global: Injector<DatabaseEntry>,

    /// Interface to steal from each worker
    stealers: Vec<Stealer<DatabaseEntry>>,
}
//
impl WorkQueue {
    /// Construct work queue
    pub fn new(jobs: Vec<DatabaseEntry>) -> Self {
        let global = Injector::new();
        for job in jobs {
            global.push(job);
        }
        Self {
            global,
            stealers: Vec::new(),
        }
    }

    /// Generate worker interfaces
    pub fn local_queues(
        &mut self,
        concurrency: usize,
    ) -> impl Iterator<Item = Worker<DatabaseEntry>> {
        assert!(concurrency > 0, "There should be at least one worker");
        let local_queues = std::iter::repeat_with(Worker::new_lifo)
            .take(concurrency)
            .collect::<Vec<_>>();
        self.stealers
            .extend(local_queues.iter().map(Worker::stealer));
        local_queues.into_iter()
    }

    /// Reinject a job
    pub fn push(&self, job: DatabaseEntry) {
        self.global.push(job);
    }
}

/// Shared state for out-of-memory handling
struct Monitor {
    /// Shared system monitor state
    system: RwLock<System>,

    /// Time spent by workers on their current job (arbitrary unit)
    elapsed: Vec<CachePadded<AtomicUsize>>,

    /// Request for workers to stop what they are doing
    stopped: Vec<AtomicBool>,
}
//
impl Monitor {
    /// Set up the out-of-memory handler
    pub fn new(concurrency: usize) -> Self {
        Self {
            system: RwLock::new(System::new()),
            elapsed: std::iter::repeat_with(|| CachePadded::new(AtomicUsize::new(0)))
                .take(concurrency)
                .collect(),
            stopped: std::iter::repeat_with(|| AtomicBool::new(false))
                .take(concurrency)
                .collect(),
        }
    }

    /// Get back access to the concurrency
    pub fn concurrency(&self) -> usize {
        debug_assert_eq!(self.elapsed.len(), self.stopped.len());
        self.elapsed.len()
    }

    /// Generate main thread interface
    pub fn server(&self) -> MonitorServer {
        MonitorServer::new(self)
    }

    /// Generate worker interfaces
    pub fn clients(&self) -> impl Iterator<Item = MonitorClient> {
        self.elapsed
            .iter()
            .zip(self.stopped.iter())
            .map(|(elapsed, stop)| MonitorClient::new(&self.system, &elapsed, stop))
    }
}

/// Polling interval in seconds
///
/// This is enough to get execution times with 0.1s precision, and seems
/// empirically to also be enough for good RAM measurements.
///
const POLLING_INTERVAL: Duration = Duration::from_millis(33);

/// Out-of-memory threshold, in bytes/thread
///
/// Out-of-memory handling will be triggered when the amount of available RAM
/// becomes lower than this times the number of worker threads, resulting in one
/// of the worker threads being killed.
///
/// This should be tuned a little higher than the RAM monitoring resolution
/// (maximum RSS increase observed from one process poll to the next). It will
/// be auto-tuned upwards if it turns out to be too optimistic.
///
static OOM_MARGIN_PER_THREAD: AtomicUsize = AtomicUsize::new(110_000_000);

// FIXME: Add unit tests ? Could do that by having a dummy binary that consumes
//        X memory during Y time.
