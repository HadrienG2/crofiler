//! Measure full-build profile

use crate::{
    commands::{CompilationDatabase, DatabaseEntry},
    output::UnitProfile,
};
use crossbeam_deque::{Injector, Stealer, Worker};
use crossbeam_utils::CachePadded;
use std::{
    any::Any,
    fs::File,
    io::{self, Read},
    num::NonZeroUsize,
    panic::UnwindSafe,
    path::Path,
    process::{Child, Command, ExitStatus, Stdio},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        mpsc::{self, RecvTimeoutError, Sender},
        Arc, RwLock,
    },
    thread::JoinHandle,
    time::{Duration, Instant},
};
use sysinfo::{
    Pid, PidExt, Process, ProcessExt, ProcessRefreshKind, RefreshKind, Signal, System, SystemExt,
};
use thiserror::Error;
use wait_timeout::ChildExt;

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
            build_done(main_thread(
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

/// Main thread of the full-build profiling process
fn main_thread(
    path: Box<Path>,
    jobs: Vec<DatabaseEntry>,
    measure_time: bool,
    concurrency_opt: Option<NonZeroUsize>,
    mut step_done: impl FnMut() + UnwindSafe,
    kill: Arc<AtomicBool>,
) -> Result<(), MeasureError> {
    std::panic::catch_unwind(move || {
        // Open the output file
        let mut writer = csv::Writer::from_writer(File::create(path)?);

        // Determine how many worker threads should be used
        let concurrency = concurrency(measure_time, concurrency_opt);
        log::info!("Will use {concurrency} worker process(es)");

        // Set up communication between the main thread and workers
        let mut work_queue = WorkQueue::new(jobs);
        let (results_sender, results_receiver) = mpsc::channel();
        let monitor = Monitor::new(concurrency);

        // Run the worker threads
        std::thread::scope(|s| {
            // Spawn worker threads
            for (local, monitor_client) in
                work_queue.local_queues(concurrency).zip(monitor.clients())
            {
                let results_sender = results_sender.clone();
                let work_queue = &work_queue;
                s.spawn(move || {
                    worker(
                        WorkReceiver::new(local, work_queue),
                        results_sender,
                        monitor_client,
                        measure_time,
                    )
                });
            }

            // Wait for results while monitoring free RAM
            let mut last_poll = Instant::now();
            let mut next_poll = last_poll + POLLING_INTERVAL;
            let mut monitor_server = monitor.server();
            loop {
                // Handle external request to terminate the job
                if kill.load(Ordering::Relaxed) {
                    monitor_server.kill_all();
                    return Err(MeasureError::Killed);
                }

                // TODO: Move to recv_deadline once available
                match results_receiver
                    .recv_timeout(next_poll.saturating_duration_since(Instant::now()))
                {
                    // A worker had to abort its current job as a result of running
                    // out of memory, send it back to other jobs.
                    Ok(Err(JobError::Killed(job))) => {
                        work_queue.push(job);
                    }

                    // A worker finished processing a job for another reason
                    Ok(other_result) => {
                        let unit_profile = other_result?;
                        log::info!(
                            "Compiled {} (max-RSS {:.2}GB, wall-time {:?})",
                            unit_profile.rel_path().display(),
                            unit_profile.max_rss_bytes() as f32 / 1e9,
                            unit_profile.wall_time()
                        );
                        writer.serialize(unit_profile)?;
                        step_done();
                    }

                    // System monitor wakeup
                    Err(RecvTimeoutError::Timeout) => {
                        // FIXME: It seems Disconnected is broken for some
                        //        reason, try offloading that job to Monitor
                        if let Err(NoThreadLeft) = monitor_server.handle_oom() {
                            return Err(MeasureError::OutOfMemory);
                        }
                        last_poll = Instant::now();
                        next_poll = last_poll + POLLING_INTERVAL;
                    }

                    // No worker threads left, we are done
                    Err(RecvTimeoutError::Disconnected) => break,
                }
            }

            // Finish writing the CSV file and exit
            writer.flush()?;
            Ok(())
        })
    })?
}
//
/// Error while measuring a build profile
#[derive(Debug, Error)]
pub enum MeasureError {
    /// Failed to profile a compilation unit
    #[error("Failed to profile a compilation unit ({0})")]
    Job(#[from] JobError),

    /// Cannot complete any job without running out of memory
    #[error("Not enough memory to compile even in sequential mode")]
    OutOfMemory,

    /// Failed to serialize a compilation profile to CSV
    #[error("Failed to serialize unit profile to CSV ({0})")]
    Csv(#[from] csv::Error),

    /// Failed to write build profile to disk
    #[error("Failed to write build profile to disk ({0})")]
    Io(#[from] io::Error),

    /// A thread panicked
    #[error("A thread panicked")]
    Panicked(Box<dyn Any + Send + 'static>),

    /// Killed from the outside
    #[error("The measurement process was killed by the user")]
    Killed,
}
//
impl From<Box<dyn Any + Send + 'static>> for MeasureError {
    fn from(inner: Box<dyn Any + Send + 'static>) -> Self {
        MeasureError::Panicked(inner)
    }
}

/// Determine the number of worker threads to use from the configuration
fn concurrency(measure_time: bool, concurrency: Option<NonZeroUsize>) -> usize {
    concurrency.map(usize::from).unwrap_or(if measure_time {
        1
    } else {
        std::thread::available_parallelism()
            .map(usize::from)
            .unwrap_or(1)
    })
}

/// Worker thread of the full-build profiling process
fn worker(
    input: WorkReceiver,
    output: Sender<Result<UnitProfile, JobError>>,
    mut monitor_client: MonitorClient,
    measure_time: bool,
) {
    let mut tree = ProcessTree::new();
    for job in input {
        let result = process_job(&mut tree, job, &mut monitor_client, measure_time);
        let failed = result.is_err();
        output.send(result).expect("Main thread has crashed");
        if failed {
            break;
        }
    }
}

/// Processing of a single job
fn process_job(
    tree: &mut ProcessTree,
    job: DatabaseEntry,
    monitor_client: &mut MonitorClient,
    measure_time: bool,
) -> Result<UnitProfile, JobError> {
    // Start job
    if let Err(MustStop) = monitor_client.switch_job() {
        return Err(JobError::Killed(job));
    }
    let start = measure_time.then(Instant::now);
    let mut process = start_job(&job)?;

    // Monitor the job to completion or error
    tree.set_root(Pid::from_u32(process.id()));
    let mut max_rss_bytes = 0;
    let exit_status = loop {
        match process.wait_timeout(POLLING_INTERVAL) {
            Ok(None) => {
                let system = monitor_client
                    .system
                    .read()
                    .expect("System monitor has been poisoned");
                tree.refresh(&system);
                if let Err(MustStop) = monitor_client.keep_job() {
                    tree.kill(&system);
                    return Err(JobError::Killed(job));
                } else {
                    max_rss_bytes = max_rss_bytes.max(tree.memory(&system));
                }
            }
            Ok(Some(exit_status)) => break exit_status,
            Err(e) => return Err(JobError::WaitFailed(e)),
        }
    };
    let wall_time = start.map(|start| start.elapsed());

    // Decide if job has succeeded or failed, log output accordingly
    report_process(process, exit_status)?;

    // Compute relative path to the input, if possible
    let rel_path = std::env::current_dir()
        .ok()
        .and_then(|workdir| pathdiff::diff_paths(job.input(), workdir))
        .unwrap_or_else(|| job.input().to_owned());

    // Emit successful job results
    Ok(UnitProfile::new(rel_path, max_rss_bytes, wall_time))
}
//
/// Error while processing a job
#[derive(Debug, Error)]
pub enum JobError {
    /// Compilation database job does not contain a program argument
    #[error("Job did not contain a program to run")]
    NoProgram,

    /// Failed to spawn process
    #[error("Failed to spawn job ({0})")]
    SpawnFailed(io::Error),

    /// Ran out of memory, must repawn job
    #[error("Job killed, likely because it ran out of memory")]
    Killed(DatabaseEntry),

    /// Failed to await process
    #[error("Failed to await job ({0})")]
    WaitFailed(io::Error),

    /// Failing status
    #[error("Job process failed")]
    JobFailed,
}

/// Start a compilation process
fn start_job(job: &DatabaseEntry) -> Result<Child, JobError> {
    let Some(program) = job.program() else {
        return Err(JobError::NoProgram);
    };
    let mut command = Command::new(program.as_ref());
    command
        .current_dir(job.current_dir())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    for arg in job.args() {
        command.arg(arg.as_ref());
    }
    match command.spawn() {
        Ok(process) => Ok(process),
        Err(e) => Err(JobError::SpawnFailed(e)),
    }
}

/// Process tree monitor
struct ProcessTree {
    /// Buffer to collect the full process tree
    tree: Vec<Pid>,

    /// Buffer to collect the current generation of parent processes
    /// Used during refresh(), empty outside of this method
    parents: Vec<Pid>,

    /// Buffer to collect the current generation of child processes
    /// Used during refresh(), empty outside of this method
    children: Vec<Pid>,
}
//
impl ProcessTree {
    /// Prepare to monitor processes
    fn new() -> Self {
        Self {
            tree: Vec::new(),
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    /// Switch to a new process tree
    fn set_root(&mut self, root: Pid) {
        self.tree.clear();
        self.tree.push(root);
    }

    /// Refresh process tree
    fn refresh(&mut self, system: &System) {
        // Seed parent list with tree root, then reset tree
        self.parents.push(
            *self
                .tree
                .first()
                .expect("You must set a root Pid with set_root() first"),
        );
        self.tree.clear();

        // As long as there are parents to be investigated...
        while !self.parents.is_empty() {
            // ...iterate over then, emptying parent list in the process...
            for parent_pid in self.parents.drain(..) {
                // Look for children of each parent...
                for (&child_pid, child) in system.processes() {
                    if child.parent() == Some(parent_pid) {
                        // ...and collect them in children list
                        self.children.push(child_pid);
                    }
                }
                // Add parent to tree
                self.tree.push(parent_pid);
            }
            // Now investigate children as possible parents
            std::mem::swap(&mut self.parents, &mut self.children);
        }
    }

    // Iterate over processes in the tree
    fn processes<'a>(&'a self, system: &'a System) -> impl Iterator<Item = &'a Process> {
        self.tree.iter().flat_map(|pid| system.process(*pid))
    }

    // Compute memory consumption of the process tree in bytes
    fn memory(&self, system: &System) -> u64 {
        self.processes(system).map(|process| process.memory()).sum()
    }

    // Terminate or kill every process in the process tree
    fn kill(&self, system: &System) -> bool {
        let mut global_result = true;
        for process in self.processes(system) {
            global_result &= process
                .kill_with(Signal::Term)
                .unwrap_or_else(|| process.kill());
        }
        global_result
    }
}

/// Report on process failure or unexpected output
fn report_process(process: Child, exit_status: ExitStatus) -> Result<(), JobError> {
    let stdpipe_report = report_stdpipe(process);
    if !exit_status.success() {
        let mut message = format!("Job failed with exit status {exit_status}");
        if !stdpipe_report.is_empty() {
            message = format!(", it had {stdpipe_report}");
        }
        log::error!("{message}");
        return Err(JobError::JobFailed);
    }
    if !stdpipe_report.is_empty() {
        log::warn!("Job had {stdpipe_report}");
    }
    Ok(())
}

/// Report on non-empty stdout and/or stderr from a compilation process
fn report_stdpipe(process: Child) -> String {
    fn read_stdpipe(pipe: Option<impl Read>) -> String {
        let mut buf = String::new();
        pipe.expect("Process output/error should have been piped and available")
            .read_to_string(&mut buf)
            .expect("Reading from pipes should be infaillible");
        buf
    }
    let stdout = read_stdpipe(process.stdout);
    let stderr = read_stdpipe(process.stderr);
    let mut report = String::new();
    let has_stdout = !stdout.is_empty();
    if has_stdout {
        report = format!(
            "non-empty standard output\n\
            ---\n\
            {stdout}\
            ---"
        );
    }
    if !stderr.is_empty() {
        if has_stdout {
            report.push_str("\nand ");
        }
        report = format!(
            "{report}non-empty standard error\n\
            ---\n\
            {stderr}\
            ---"
        );
    }
    report
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
//
/// Worker interface to the work queue
struct WorkReceiver<'queue> {
    local: Worker<DatabaseEntry>,
    global: &'queue Injector<DatabaseEntry>,
    stealers: &'queue [Stealer<DatabaseEntry>],
}
//
impl<'queue> WorkReceiver<'queue> {
    /// Set up the worker interface
    fn new(local: Worker<DatabaseEntry>, queue: &'queue WorkQueue) -> Self {
        Self {
            local,
            global: &queue.global,
            stealers: &queue.stealers,
        }
    }
}
//
impl Iterator for WorkReceiver<'_> {
    type Item = DatabaseEntry;

    fn next(&mut self) -> Option<DatabaseEntry> {
        // Start from our local queue
        self.local.pop().or_else(|| {
            // Otherwise, we need to look for a task elsewhere.
            std::iter::repeat_with(|| {
                // Try stealing a batch of tasks from the global queue or other threads
                self.global.steal_batch_and_pop(&self.local).or_else(|| {
                    self.stealers
                        .iter()
                        .map(|s| s.steal_batch_and_pop(&self.local))
                        .collect()
                })
            })
            // Loop while no task was stolen and any steal operation needs to be retried.
            .find(|s| !s.is_retry())
            // Extract the stolen task, if there is one.
            .and_then(|s| s.success())
        })
    }
}

/// Shared state for out-of-memory handling
struct Monitor {
    /// Shared system monitor state
    system: RwLock<System>,

    /// Time spent by workers on their current job (arbitrary unit)
    elapsed: Vec<CachePadded<AtomicUsize>>,

    /// Request for workers to stop what they are doing
    stop: Vec<AtomicBool>,
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
            stop: std::iter::repeat_with(|| AtomicBool::new(false))
                .take(concurrency)
                .collect(),
        }
    }

    /// Get back access to the concurrency
    pub fn concurrency(&self) -> usize {
        debug_assert_eq!(self.elapsed.len(), self.stop.len());
        self.elapsed.len()
    }

    /// Generate main thread interface
    pub fn server(&self) -> MonitorServer {
        MonitorServer {
            monitor: self,
            oom_threshold: OOM_MARGIN_PER_THREAD.load(Ordering::Relaxed) as u64
                * self.concurrency() as u64,
            refresh_kind: RefreshKind::new()
                .with_memory()
                .with_processes(ProcessRefreshKind::new()),
            last_available_memory: None,
        }
    }

    /// Generate worker interfaces
    pub fn clients(&self) -> impl Iterator<Item = MonitorClient> {
        self.elapsed
            .iter()
            .zip(self.stop.iter())
            .map(|(elapsed, stop)| MonitorClient {
                system: &self.system,
                elapsed: 0,
                elapsed_shared: &elapsed,
                stop,
            })
    }
}
//
/// Main thread interface for out of memory handling
struct MonitorServer<'monitor> {
    /// Shared monitor state
    monitor: &'monitor Monitor,

    /// Out of memory threshold in bytes
    ///
    /// A worker will be killed if free memory gets below this threshold.
    ///
    oom_threshold: u64,

    /// System monitor refresh settings
    refresh_kind: RefreshKind,

    /// Last observed available_memory value
    last_available_memory: Option<u64>,
}
//
impl MonitorServer<'_> {
    /// Check for out-of-memory conditions and handle them
    pub fn handle_oom(&mut self) -> Result<(), NoThreadLeft> {
        // Check available memory
        let available_memory = {
            let mut guard = self
                .monitor
                .system
                .write()
                .expect("System monitor has been poisoned");
            guard.refresh_specifics(self.refresh_kind);
            guard.available_memory()
        };

        // Update OOM threshold if the former one was too optimistic
        let last_available_memory = self.last_available_memory.unwrap_or(available_memory);
        self.last_available_memory = Some(available_memory);
        let newly_used_memory = last_available_memory.saturating_sub(available_memory);
        if newly_used_memory > self.oom_threshold {
            let margin_per_thread = OOM_MARGIN_PER_THREAD.load(Ordering::Relaxed) as u64;
            let concurrency = self.oom_threshold / margin_per_thread;
            let new_margin_per_thread = newly_used_memory / concurrency + 1;
            log::warn!(
                "OOM threshold of {}MB/thread is too low, observed +{}MB/thread across monitor tick. Moving to that.",
                margin_per_thread / 1_000_000,
                new_margin_per_thread / 1_000_000,
            );
            OOM_MARGIN_PER_THREAD.store(new_margin_per_thread as usize, Ordering::Relaxed);
            self.oom_threshold = new_margin_per_thread * concurrency;
        }

        // Handle OOM conditions
        if available_memory < self.oom_threshold {
            let youngest_alive_worker = self
                .monitor
                .stop
                .iter()
                .zip(self.monitor.elapsed.iter())
                .enumerate()
                .filter_map(|(idx, (stopped, elapsed))| {
                    if stopped.load(Ordering::Relaxed) {
                        None
                    } else {
                        Some((idx, elapsed))
                    }
                })
                .min_by_key(|(_idx, elapsed)| elapsed.load(Ordering::Relaxed))
                .expect("There should be at least one worker thread")
                .0;
            self.monitor.stop[youngest_alive_worker].store(true, Ordering::Relaxed);
            self.oom_threshold -= OOM_MARGIN_PER_THREAD.load(Ordering::Relaxed) as u64;
            log::warn!(
                "Killed worker thread #{youngest_alive_worker} due to out-of-memory condition"
            );
            if self.oom_threshold == 0 {
                return Err(NoThreadLeft);
            }
        }
        Ok(())
    }

    /// Kill all jobs
    pub fn kill_all(&self) {
        for stop in &self.monitor.stop {
            stop.store(true, Ordering::Relaxed);
        }
    }
}
//
/// Error returned when all threads have been killed due to OOM
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NoThreadLeft;
//
/// Worker interface for out-of-memory handling
struct MonitorClient<'monitor> {
    /// System monitor
    system: &'monitor RwLock<System>,

    /// Time spent by this worker on its current job
    elapsed: usize,

    /// Main thread visible version of `elapsed`
    elapsed_shared: &'monitor AtomicUsize,

    /// Request from the main thread to stop this thread
    stop: &'monitor AtomicBool,
}
//
impl MonitorClient<'_> {
    /// Signal that the same job keeps being processed
    pub fn keep_job(&mut self) -> Result<(), MustStop> {
        self.update_elapsed(|elapsed| elapsed + 1)
    }

    /// Signal that a new job is being processed
    pub fn switch_job(&mut self) -> Result<(), MustStop> {
        self.update_elapsed(|_| 0)
    }

    /// Update the value of the elapsed counter
    fn update_elapsed(&mut self, update: impl FnOnce(usize) -> usize) -> Result<(), MustStop> {
        // Handle out-of-memory stop signal
        let stop = self.stop.load(Ordering::Relaxed);
        if stop {
            return Err(MustStop);
        }

        // If no OOM occured, updated elapsed time counter as scheduled
        self.elapsed = update(self.elapsed);
        self.elapsed_shared.store(self.elapsed, Ordering::Relaxed);
        Ok(())
    }
}
//
/// Signal that a worker thread must stop due to an OOM condition
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct MustStop;

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
