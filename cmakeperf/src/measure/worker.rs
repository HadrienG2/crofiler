//! Worker threads measuring build performance

use super::{Monitor, WorkQueue, POLLING_INTERVAL};
use crate::{commands::DatabaseEntry, output::UnitProfile};
use crossbeam_deque::{Steal, Worker};
use std::{
    io::{self, Read},
    panic::AssertUnwindSafe,
    process::{Child, Command, ExitStatus, Stdio},
    sync::{
        atomic::{self, AtomicUsize, Ordering},
        mpsc::Sender,
        RwLockReadGuard,
    },
    time::Instant,
};
use sysinfo::{Pid, PidExt, Process, ProcessExt, Signal, System, SystemExt};
use thiserror::Error;
use wait_timeout::ChildExt;

/// Worker thread of the full-build profiling process
pub(super) fn run(
    input: WorkReceiver,
    output: Sender<Result<UnitProfile, JobError>>,
    mut monitor_client: MonitorClient,
    measure_time: bool,
) {
    let output_ref = AssertUnwindSafe(&output);
    let mut monitor_client = AssertUnwindSafe(&mut monitor_client);
    let maybe_panic = std::panic::catch_unwind(move || {
        let mut tree = ProcessTree::new();
        for job in input {
            let result = process_job(&mut tree, job, &mut monitor_client, measure_time);
            let failed = result.is_err();
            output_ref.send(result).expect("Main thread has crashed");
            if failed {
                break;
            }
        }
    });
    if let Err(panic) = maybe_panic {
        output
            .send(Err(JobError::WorkerPanicked))
            .expect("Main thread has crashed");
        std::panic::resume_unwind(panic);
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
                let system = monitor_client.system();
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

    /// Worker panicked
    #[error("Worker thread panicked")]
    WorkerPanicked,
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
        assert!(self.parents.is_empty());
        assert!(self.children.is_empty());
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

/// Worker interface to the work queue
pub(super) struct WorkReceiver<'queue> {
    /// Thread-local work queue
    local: Worker<DatabaseEntry>,

    /// Facilities shared with other threads
    shared: &'queue WorkQueue,

    /// Last observed WorkQueue futex value
    last_futex_value: u32,
}
//
impl<'queue> WorkReceiver<'queue> {
    /// Set up the worker interface
    pub fn new(local: Worker<DatabaseEntry>, shared: &'queue WorkQueue) -> Self {
        let last_futex_value = shared.futex.load(Ordering::Acquire);
        assert_eq!(last_futex_value, WorkQueue::FUTEX_INITIAL);
        Self {
            local,
            shared,
            last_futex_value,
        }
    }

    /// Wait for work or a termination signal
    pub fn wait(&mut self) -> WaitOutcome {
        // If we last observed the "finished" signal and checked the queue one
        // last time for new jobs after that, we're done: no more jobs can come.
        if self.last_futex_value == WorkQueue::FUTEX_FINISHED {
            return WaitOutcome::Finished;
        }

        // Otherwise, wait for activity from the main thread, then check again
        let futex = &self.shared.futex;
        self.last_futex_value = loop {
            let new_futex_value = futex.load(Ordering::Acquire);
            if new_futex_value != self.last_futex_value {
                break new_futex_value;
            }
            atomic_wait::wait(futex, new_futex_value);
        };
        WaitOutcome::Continue
    }
}
//
/// Outcome of waiting for the main thread's signal
pub(super) enum WaitOutcome {
    /// New job might have arrived
    Continue,

    /// No job will be coming anymore
    Finished,
}
//
impl Iterator for WorkReceiver<'_> {
    type Item = DatabaseEntry;

    fn next(&mut self) -> Option<DatabaseEntry> {
        loop {
            // Look for work, starting with our thread-local queue
            let pop_result = self.local.pop().or_else(|| {
                // If no work is found locally, we need to look elsewhere.
                std::iter::repeat_with(|| {
                    // Try stealing tasks from other workers...
                    self.shared
                        .stealers
                        .iter()
                        .map(|s| s.steal_batch_and_pop(&self.local))
                        .collect::<Steal<_>>()
                        // ...or if that fails, try the global injector
                        .or_else(|| self.shared.global.steal())
                })
                // Loop while no task was stolen and any steal operation needs to be retried.
                .find(|s| !s.is_retry())
                // Extract the stolen task, if there is one.
                .and_then(|s| s.success())
            });

            // If a task was extracted, return it
            if let Some(result) = pop_result {
                return Some(result);
            }

            // Otherwise, wait for work or a termination signal
            match self.wait() {
                WaitOutcome::Continue => continue,
                WaitOutcome::Finished => break None,
            }
        }
    }
}

/// Worker interface for out-of-memory handling
pub(super) struct MonitorClient<'monitor> {
    /// System monitor
    monitor: &'monitor Monitor,

    /// Time spent by this worker on its current job
    elapsed: usize,

    /// Main thread visible version of `elapsed`
    elapsed_shared: &'monitor AtomicUsize,

    /// Request from the main thread to stop this thread
    stopped: StopClient<'monitor>,
}
//
impl<'monitor> MonitorClient<'monitor> {
    /// Set up the worker interface
    pub fn new(
        monitor: &'monitor Monitor,
        elapsed: &'monitor AtomicUsize,
        stopped: StopClient<'monitor>,
    ) -> Self {
        Self {
            monitor,
            elapsed: 0,
            elapsed_shared: elapsed,
            stopped,
        }
    }

    /// Check if the main thread has asked us to stop
    pub fn must_stop(&self) -> bool {
        self.stopped.must_stop()
    }

    /// Signal that the same job keeps being processed
    pub fn keep_job(&mut self) -> Result<(), MustStop> {
        self.update_elapsed(|elapsed| elapsed + 1)
    }

    /// Signal that a new job is being processed
    pub fn switch_job(&mut self) -> Result<(), MustStop> {
        self.update_elapsed(|_| 0)
    }

    /// Access the system monitor
    pub fn system(&self) -> RwLockReadGuard<'monitor, System> {
        self.monitor
            .system
            .read()
            .expect("System monitor has been poisoned")
    }

    /// Update the value of the elapsed counter
    fn update_elapsed(&mut self, update: impl FnOnce(usize) -> usize) -> Result<(), MustStop> {
        // Handle stop signal
        if self.must_stop() {
            return Err(MustStop);
        }

        // If no OOM occured, updated elapsed time counter as scheduled
        // The write can be Relaxed since we're not sending any other information
        self.elapsed = update(self.elapsed);
        self.elapsed_shared.store(self.elapsed, Ordering::Relaxed);
        Ok(())
    }
}
//
/// Signal that the main thread asked a worker thread to stop
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct MustStop;

/// Notification channel for the main thread to stop us
pub(super) struct StopClient<'word> {
    /// Word within which the flag is located
    word: &'word AtomicUsize,

    /// Bit mask to test or set the flag
    mask: usize,
}
//
impl<'word> StopClient<'word> {
    /// Create a flag accessor
    pub fn new(word: &'word AtomicUsize, mask: usize) -> Self {
        debug_assert_eq!(mask.count_ones(), 1, "Not a valid flag mask");
        Self { word, mask }
    }

    /// Check if we've been asked to stop
    pub fn must_stop(&self) -> bool {
        let raised = (self.word.load(Ordering::Relaxed) & self.mask) == 0;
        if raised {
            atomic::fence(Ordering::Acquire);
        }
        raised
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::measure::BITS_PER_USIZE;
    use quickcheck::TestResult;
    use quickcheck_macros::quickcheck;

    #[quickcheck]
    fn stop_client(init: usize, bit_idx: u8) -> TestResult {
        // Ignore invalid bit indices
        if bit_idx as usize >= BITS_PER_USIZE {
            return TestResult::discard();
        }

        // Test stop client
        let word = AtomicUsize::new(init);
        let mask = 1 << bit_idx;
        let flag = StopClient::new(&word, mask);
        assert_eq!(word.load(Ordering::Relaxed), init);
        assert_eq!(flag.mask, mask);
        assert_eq!(flag.must_stop(), (init & flag.mask) == 0);
        TestResult::passed()
    }
}
