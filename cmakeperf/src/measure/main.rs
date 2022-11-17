//! Main thread of the build profiling process

use super::{
    worker::{self, JobError, WorkReceiver},
    Monitor, WorkQueue, OOM_MARGIN_PER_THREAD, POLLING_INTERVAL,
};
use crate::commands::DatabaseEntry;
use std::{
    any::Any,
    fs::File,
    io,
    num::NonZeroUsize,
    panic::UnwindSafe,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, RecvTimeoutError},
        Arc,
    },
    time::Instant,
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

/// Main thread interface for out of memory handling
pub(super) struct MonitorServer<'monitor> {
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
impl<'monitor> MonitorServer<'monitor> {
    /// Set up main thread interface
    pub(super) fn new(monitor: &'monitor Monitor) -> Self {
        Self {
            monitor,
            oom_threshold: OOM_MARGIN_PER_THREAD.load(Ordering::Relaxed) as u64
                * monitor.concurrency() as u64,
            refresh_kind: RefreshKind::new()
                .with_memory()
                .with_processes(ProcessRefreshKind::new()),
            last_available_memory: None,
        }
    }

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
                .stopped
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
            self.monitor.stopped[youngest_alive_worker].store(true, Ordering::Relaxed);
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
        for stop in &self.monitor.stopped {
            stop.store(true, Ordering::Relaxed);
        }
    }
}
//
/// Error returned when all threads have been killed due to OOM
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NoThreadLeft;

// FIXME: Add unit tests ? Could do that by having a dummy binary that consumes
//        X memory during Y time.
