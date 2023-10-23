//! Main thread of the build profiling process

use super::{
    worker::{self, JobError, WorkReceiver},
    Monitor, StopFlags, WorkQueue, BITS_PER_USIZE,
};
use crate::{commands::DatabaseEntry, measure::PollClock};
use std::{
    any::Any,
    fs::File,
    io,
    num::NonZeroUsize,
    panic::UnwindSafe,
    path::Path,
    sync::{
        atomic::{self, AtomicBool, AtomicUsize, Ordering},
        mpsc::{self, RecvTimeoutError},
        Arc,
    },
};
use sysinfo::{ProcessRefreshKind, RefreshKind, SystemExt};
use thiserror::Error;

/// Main thread of the full-build profiling process
pub(super) fn run(
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
        let mut remaining_jobs = jobs.len();
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
                    worker::run(
                        WorkReceiver::new(local, work_queue),
                        results_sender,
                        monitor_client,
                        measure_time,
                    )
                });
            }

            // Wait for results while monitoring free RAM
            let mut monitor_server = monitor.server();
            let mut work_sender = work_queue.sender();
            work_sender.start();
            let mut poll_clock = PollClock::start();
            while remaining_jobs > 0 {
                // Handle external request to terminate the job
                if kill.load(Ordering::Relaxed) {
                    atomic::fence(Ordering::Acquire);
                    monitor_server.stop_all();
                    return Err(MeasureError::Killed);
                }

                // Wait for a job to be complete or a timeout
                // TODO: Move to recv_deadline once available
                match results_receiver.recv_timeout(poll_clock.poll_delay()) {
                    // A worker had to abort its current job as a result of running
                    // out of memory, send it back to other workers.
                    Ok(Err(JobError::Killed(job))) => {
                        work_sender.push(job);
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
                        remaining_jobs -= 1;
                    }

                    // System monitor wakeup
                    Err(RecvTimeoutError::Timeout) => match monitor_server.update() {
                        MonitorStatus::Running => {}
                        MonitorStatus::KilledEveryone => return Err(MeasureError::OutOfMemory),
                    },

                    // No worker threads left to send job results
                    //
                    // This should not happen because worker threads should wait
                    // for the main thread to drop the WorkSender before they
                    // terminate, in case the main thread sends in more work.
                    //
                    Err(RecvTimeoutError::Disconnected) => {
                        unreachable!("Worker threads should wait for us")
                    }
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
        available_parallelism()
    })
}

/// Infaillible version of std::thread::available_parallelism that takes a
/// guess at the amount of parallelism when it's not known
fn available_parallelism() -> usize {
    std::thread::available_parallelism()
        .map(usize::from)
        // Minimal amount of CPU threads expected on a dev machine in the
        // unlikely scenario where we have no OS-level CPU enumeration API.
        .unwrap_or(4)
}

/// Main thread interface to send work to worker threads
pub(super) struct WorkSender<'queue> {
    /// Shared work queue state
    queue: &'queue WorkQueue,

    /// Last set value of queue.futex
    last_futex_value: u32,
}
//
impl<'queue> WorkSender<'queue> {
    /// Set up main thread interface
    pub fn new(queue: &'queue WorkQueue) -> Self {
        // No need to synchronize yet since only this thread can update the futex
        let last_futex_value = queue.futex.load(Ordering::Relaxed);
        assert_eq!(last_futex_value, WorkQueue::FUTEX_INITIAL);
        Self {
            queue,
            last_futex_value,
        }
    }

    /// Allow workers to start
    ///
    /// This should only be done once the main thread is ready to start
    /// system monitoring on a short notice.
    ///
    pub fn start(&mut self) {
        self.update_futex(|last_value| {
            assert_eq!(last_value, WorkQueue::FUTEX_INITIAL);
            WorkQueue::FUTEX_INITIAL + 1
        });
        atomic_wait::wake_all(&self.queue.futex);
    }

    /// (Re)submit a job to workers
    pub fn push(&mut self, job: DatabaseEntry) {
        self.queue.global.push(job);
        self.update_futex(|last_value| {
            assert_ne!(
                last_value,
                WorkQueue::FUTEX_FINISHED,
                "Closed WorkQueue can't accept more jobs"
            );
            last_value
                .checked_add(1)
                .unwrap_or(WorkQueue::FUTEX_INITIAL + 1)
        });
        atomic_wait::wake_one(&self.queue.futex);
    }

    /// Tell workers we won't be sending work anymore
    fn close(&mut self) {
        self.update_futex(|_| WorkQueue::FUTEX_FINISHED);
        atomic_wait::wake_all(&self.queue.futex);
    }

    /// Sync up `queue.futex` with `last_futex_value`
    fn update_futex(&mut self, updater: impl FnOnce(u32) -> u32) {
        self.last_futex_value = updater(self.last_futex_value);
        self.queue
            .futex
            .store(self.last_futex_value, Ordering::Release);
    }
}
//
impl Drop for WorkSender<'_> {
    /// Automatically close work queue on drop
    fn drop(&mut self) {
        self.close()
    }
}

/// Main thread interface for system monitoring and out of memory handling
pub(super) struct MonitorServer<'monitor> {
    /// Shared monitor state
    monitor: &'monitor Monitor,

    /// Interface to ask threads to stop
    stop_server: StopServer<'monitor>,

    /// Assumed number of background threads
    assumed_bkg_threads: usize,

    /// Out-of-memory killer margin
    ///
    /// A worker will be killed if free memory gets below this threshold.
    ///
    oom_margin: u64,

    /// System monitor refresh settings
    refresh_kind: RefreshKind,

    /// Last observed available_memory value
    last_available_memory: Option<u64>,
}
//
impl<'monitor> MonitorServer<'monitor> {
    /// Set up main thread interface
    pub fn new(monitor: &'monitor Monitor) -> Self {
        let assumed_bkg_threads = OOM_ASSUMED_BKG_THREADS.load(Ordering::Relaxed);
        let mut result = Self {
            monitor,
            stop_server: monitor.stop.server(),
            assumed_bkg_threads,
            oom_margin: OOM_MARGIN_PER_THREAD
                * (monitor.concurrency() + assumed_bkg_threads) as u64,
            refresh_kind: RefreshKind::new()
                .with_memory()
                .with_processes(ProcessRefreshKind::new()),
            last_available_memory: None,
        };
        result.last_available_memory = Some(result.refresh_and_check_memory());
        result
    }

    /// Update system monitor state and handle job lifecycle events
    pub fn update(&mut self) -> MonitorStatus {
        let available_memory = self.refresh_and_check_memory();
        self.update_oom_threshold(available_memory);
        self.handle_oom_and_termination(available_memory)
    }

    /// Ask all workers to terminate
    pub fn stop_all(&mut self) {
        self.stop_server.stop_all();
    }

    /// Refresh system monitor and check available memory
    fn refresh_and_check_memory(&mut self) -> u64 {
        let mut guard = self
            .monitor
            .system
            .write()
            .expect("System monitor has been poisoned");
        guard.refresh_specifics(self.refresh_kind);
        guard.available_memory()
    }

    /// Tighten OOM threshold if new measurements call for it
    fn update_oom_threshold(&mut self, available_memory: u64) {
        let last_available_memory = self
            .last_available_memory
            .expect("Initialized at end of new()");
        self.last_available_memory = Some(available_memory);
        let newly_used_memory = last_available_memory.saturating_sub(available_memory);
        if newly_used_memory > self.oom_margin {
            let assumed_threads = self.assumed_threads() as u64;
            let old_margin_per_thread = self.oom_margin / assumed_threads;
            let new_margin_per_thread = newly_used_memory / assumed_threads + 1;
            log::warn!(
                "OOM threshold of {:.6}MB/thread is too low, observed +{:.6}MB/thread across monitor tick. Moving to that.",
                old_margin_per_thread as f32 / 1_000_000.0,
                new_margin_per_thread as f32 / 1_000_000.0,
            );
            self.oom_margin = new_margin_per_thread * assumed_threads;
        }
    }

    /// Handle out-of-memory and worker termination conditions
    fn handle_oom_and_termination(&mut self, available_memory: u64) -> MonitorStatus {
        // Handle out-of-memory conditions
        while available_memory < self.oom_margin {
            let old_assumed_threads = self.assumed_threads();
            let youngest_alive_worker = self
                .stop_server
                .enumerate_active()
                // Relaxed is fine here since we're not reading `elapsed` to synchronize
                .min_by_key(|&idx| self.monitor.elapsed[idx].load(Ordering::Relaxed))
                .expect("There should be at least one active worker thread");
            self.stop_server.stop(youngest_alive_worker);
            let new_assumed_threads = old_assumed_threads - 1;
            self.oom_margin =
                self.oom_margin / old_assumed_threads as u64 * new_assumed_threads as u64;
            log::warn!(
                "Killed worker thread #{youngest_alive_worker} due to out-of-memory condition"
            );
            if self.stop_server.num_active() == 0 {
                atomic::fence(Ordering::Acquire);
                return MonitorStatus::KilledEveryone;
            }
        }

        // Make sure there still are threads left
        MonitorStatus::Running
    }

    /// Assumed number of live threads, including background OS threads
    fn assumed_threads(&self) -> usize {
        self.stop_server.num_active() + self.assumed_bkg_threads
    }
}
//
/// Error returned when all threads have been killed due to OOM
#[must_use]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MonitorStatus {
    /// There still are running threads
    Running,

    /// We just killed the last thread, which means some jobs won't be done
    KilledEveryone,
}

/// Default out-of-memory threshold, in bytes/thread
///
/// Out-of-memory handling will be triggered when the amount of available RAM
/// becomes lower than this times the number of worker threads, resulting in one
/// of the worker threads being killed.
///
/// This should be tuned a little higher than the RAM monitoring resolution
/// (maximum RSS increase observed from one process poll to the next). It will
/// be auto-tuned upwards if it turns out to be too optimistic.
///
const OOM_MARGIN_PER_THREAD: u64 = 100_000_000;

/// Assume existence of one thread per system CPU core hogging memory
///
/// This will drastically strengthen the safety margin of the out-of-memory
/// killer and as a result will result in more false-positive OOM kills, but
/// can be convenient in scenarios like integration testing.
///
/// Previously started measurements will not be affected, you need to call this
/// before starting measurements. You can safely call it multiple times.
///
pub fn assume_oversubscription() {
    OOM_ASSUMED_BKG_THREADS.store(available_parallelism(), Ordering::Relaxed);
}

/// Out-of-memory killer marging, accounting for background system load
static OOM_ASSUMED_BKG_THREADS: AtomicUsize = AtomicUsize::new(1);

/// Main thread interface to StopFlags
pub(super) struct StopServer<'flags>(&'flags StopFlags);
//
impl<'flags> StopServer<'flags> {
    /// Set up the main thread interface
    pub fn new(flags: &'flags StopFlags) -> Self {
        Self(flags)
    }

    /// Count how many workers have not been stopped yet
    pub fn num_active(&self) -> usize {
        self.word_values()
            .map(|word| word.count_ones() as usize)
            .sum()
    }

    /// Iterate over workers that haven't been stopped yet, return their indices
    pub fn enumerate_active(&self) -> impl Iterator<Item = usize> + '_ {
        self.word_values()
            .enumerate()
            .flat_map(|(word_idx, mut word_value)| {
                let mut bit_offset = word_idx * BITS_PER_USIZE;
                std::iter::from_fn(move || {
                    if word_value == 0 {
                        return None;
                    }
                    let offset_after_first_one = word_value.trailing_zeros() as usize + 1;
                    bit_offset += offset_after_first_one;
                    word_value >>= offset_after_first_one;
                    Some(bit_offset - 1)
                })
            })
    }

    /// Ask a worker to stop
    pub fn stop(&self, idx: usize) {
        let (word, mask) = self.word_and_mask(idx);
        // Relaxed load/store cycle is fine since only the main thread writes
        let old_word = word.load(Ordering::Relaxed);
        assert!(old_word & mask != 0, "Attempted to stop a worker twice");
        word.store(old_word & !mask, Ordering::Release);
    }

    /// Ask all workers to stop
    pub fn stop_all(&self) {
        atomic::fence(Ordering::Release);
        for word in &self.0 .0 {
            word.store(0, Ordering::Relaxed);
        }
    }

    /// Iterate over the value of inner atomic words
    fn word_values(&self) -> impl Iterator<Item = usize> + '_ {
        // Relaxed is fine since only the main thread can write to this
        self.0 .0.iter().map(|a| a.load(Ordering::Relaxed))
    }

    /// Determine the word and bitmask associated with a certain bit
    fn word_and_mask(&self, idx: usize) -> (&AtomicUsize, usize) {
        let word = &self.0 .0[idx / BITS_PER_USIZE];
        let mask = 1 << (idx % BITS_PER_USIZE);
        (word, mask)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn concurrency(measure_time: bool, concurrency: Option<NonZeroUsize>) {
            if let Some(val) = concurrency {
                prop_assert_eq!(
                    super::concurrency(measure_time, concurrency),
                    usize::from(val)
                );
            } else if measure_time {
                prop_assert_eq!(super::concurrency(measure_time, concurrency), 1);
            } else if let Ok(par) = std::thread::available_parallelism() {
                prop_assert_eq!(
                    super::concurrency(measure_time, concurrency),
                    usize::from(par)
                );
            } else {
                // Last-chance default should use at least one core and not
                // oversubscribe lower-end CPUs too much
                let result = super::concurrency(measure_time, concurrency);
                prop_assert!((1..=4).contains(&result));
            }
        }
    }
}
