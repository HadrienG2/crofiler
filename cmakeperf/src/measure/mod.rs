//! Measure full-build profile

mod main;
mod worker;

use self::{
    main::{MonitorServer, StopServer, WorkSender},
    worker::{MonitorClient, StopClient},
};
use crate::commands::{CompilationDatabase, DatabaseEntry};
use crossbeam_deque::{Injector, Stealer, Worker};
use crossbeam_utils::CachePadded;
use std::{
    num::NonZeroUsize,
    panic::UnwindSafe,
    path::Path,
    sync::{
        atomic::{AtomicBool, AtomicU32, AtomicUsize, Ordering},
        Arc, RwLock,
    },
    thread::JoinHandle,
    time::{Duration, Instant},
};
use sysinfo::{MemoryRefreshKind, ProcessRefreshKind, RefreshKind, System};

pub use self::main::{assume_oversubscription, MeasureError};

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
    /// `database` provides the list of compilation units to be profiled.
    ///
    /// Measuring elapsed time per compilation unit is optional, as is
    /// specifying the build concurrency. The default build concurrency is
    /// 1 process (sequential build) if elapsed time is requested, one process
    /// per CPU hyperthread otherwise.
    ///
    /// `step_done` is called whenever a compilation unit has been profiled,
    /// it is meant to be used for progress tracking.
    ///
    /// `build_done` is called once the full build has been profiled.
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
        let main_thread = {
            let kill = kill.clone();
            Some(std::thread::spawn(move || {
                build_done(main::run(
                    path,
                    jobs,
                    measure_time,
                    concurrency,
                    step_done,
                    kill,
                ))
            }))
        };
        Self { main_thread, kill }
    }
}
//
impl Drop for Measurement {
    fn drop(&mut self) {
        self.kill.store(true, Ordering::Release);
        if let Some(main_thread) = self.main_thread.take() {
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

    /// Notification channel for `WorkSender`
    ///
    /// Takes a different nonzero value every time work is sent via
    /// `WorkSender::push()` and goes to zero when the `WorkSender` is dropped.
    ///
    futex: AtomicU32,
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
            futex: AtomicU32::new(Self::FUTEX_INITIAL),
        }
    }

    /// Set up worker-local queues, used to build WorkReceivers
    ///
    /// It would be nice to directly return WorkReceivers from this method, but
    /// with the current borrow checker rules, that would result in an
    /// undesirable long-lived mutable borrow of the WorkQueue.
    ///
    pub fn local_queues(
        &mut self,
        concurrency: usize,
    ) -> impl Iterator<Item = Worker<DatabaseEntry>> {
        assert_eq!(self.stealers.len(), 0, "This should only be called once");
        assert!(concurrency > 0, "There should be at least one worker");
        let local_queues = std::iter::repeat_with(Worker::new_fifo)
            .take(concurrency)
            .collect::<Vec<_>>();
        self.stealers
            .extend(local_queues.iter().map(Worker::stealer));
        local_queues.into_iter()
    }

    /// Set up main thread interface
    pub fn sender(&self) -> WorkSender {
        WorkSender::new(self)
    }

    /// Special value of `futex` that indicates the WorkQueue is closed
    /// and will not receive more jobs.
    const FUTEX_FINISHED: u32 = 0;

    /// Initial value of `futex` that won't come back.
    const FUTEX_INITIAL: u32 = 1;
}

/// Shared state for system monitoring and OOM handling
///
/// This handles the following responsibilities :
/// - System monitoring by the main thread, readable by worker threads
///   * If system RAM gets too low, main thread handles it by killing a worker
/// - Tracking how long each worker has been at its task for OOM victim selection
/// - Letting the main thread ask a worker thread to stop
///
struct Monitor {
    /// Shared system monitor state
    system: RwLock<System>,

    /// Time spent by workers on their current job (arbitrary unit)
    elapsed: Vec<CachePadded<AtomicUsize>>,

    /// Request for workers to stop what they are doing
    stop: StopFlags,
}
//
impl Monitor {
    /// Set up the out-of-memory handler
    #[allow(clippy::assertions_on_constants)]
    pub fn new(concurrency: usize) -> Self {
        assert!(sysinfo::IS_SUPPORTED_SYSTEM, "this OS isn't supported");
        Self {
            system: RwLock::new(System::new_with_specifics(Self::refresh_kind())),
            elapsed: std::iter::repeat_with(|| CachePadded::new(AtomicUsize::new(0)))
                .take(concurrency)
                .collect(),
            stop: StopFlags::new(concurrency),
        }
    }

    /// Information that we need to refresh
    pub fn refresh_kind() -> RefreshKind {
        RefreshKind::new()
            .with_memory(MemoryRefreshKind::new().with_ram())
            .with_processes(ProcessRefreshKind::new().with_memory())
    }

    /// Get back access to the concurrency
    pub fn concurrency(&self) -> usize {
        self.elapsed.len()
    }

    /// Set up main thread interface
    pub fn server(&self) -> MonitorServer {
        MonitorServer::new(self)
    }

    /// Set up worker interfaces
    pub fn clients(&self) -> impl Iterator<Item = MonitorClient> {
        debug_assert_eq!(self.elapsed.len(), self.stop.clients().count());
        self.elapsed
            .iter()
            .zip(self.stop.clients())
            .map(|(elapsed, stop_client)| MonitorClient::new(self, elapsed, stop_client))
    }
}

/// Vector of atomic flags that can be raised to request threads to stop
///
/// A stop flag can only be raised by the main thread, and once it has been
/// raised, it will remain raised for the rest of its lifetime.
///
struct StopFlags(Vec<AtomicUsize>);
//
impl StopFlags {
    /// Create a set of stop flags
    pub fn new(len: usize) -> Self {
        let trailing_bits = len % BITS_PER_USIZE;
        Self(
            std::iter::repeat_with(|| AtomicUsize::new(usize::MAX))
                .take(len / BITS_PER_USIZE)
                .chain((trailing_bits != 0).then_some(AtomicUsize::new((1 << trailing_bits) - 1)))
                .collect(),
        )
    }

    /// Set up main thread interface
    pub fn server(&self) -> StopServer {
        StopServer::new(self)
    }

    /// Set up worker interfaces
    pub fn clients(&self) -> impl Iterator<Item = StopClient> + '_ {
        self.0.iter().enumerate().flat_map(|(idx, word)| {
            // Count worker flags within the active words
            let word_value = word.load(Ordering::Relaxed);
            let is_last = idx == self.0.len() - 1;
            let mut num_iters = if !is_last {
                debug_assert_eq!(
                    word_value,
                    usize::MAX,
                    "This should only be called during initialization"
                );
                BITS_PER_USIZE
            } else {
                debug_assert_eq!(
                    word_value.leading_zeros(),
                    word_value.count_zeros(),
                    "This should only be called during initialization"
                );
                word_value.trailing_ones() as usize
            };

            // Emit clients for each worker
            let mut mask = 1;
            std::iter::from_fn(move || {
                (num_iters != 0).then(|| {
                    let old_mask = mask;
                    if num_iters > 1 {
                        mask <<= 1
                    };
                    num_iters -= 1;
                    StopClient::new(word, old_mask)
                })
            })
        })
    }
}
//
/// Number of bits in an `usize` integer
const BITS_PER_USIZE: usize = std::mem::size_of::<usize>() * 8;

/// Polling clock, used for system monitoring tasks
pub struct PollClock {
    /// When the last poll occured
    last_poll: Instant,
}
//
impl PollClock {
    /// Start the clock, should be done right after starting monitored task(s)
    pub fn start() -> Self {
        Self {
            last_poll: Instant::now(),
        }
    }

    /// Advance poll clock, tell how long till the next poll should occur
    pub fn poll_delay(&mut self) -> Duration {
        let poll_start = Instant::now();
        let curr_polling_interval = Self::polling_interval();
        let mut next_poll = self.last_poll + curr_polling_interval;
        if next_poll < poll_start {
            let actual_polling_interval = poll_start - self.last_poll;
            let next_polling_interval_millis =
                (actual_polling_interval.as_secs_f64() * 1.1 * 1000.0).ceil() as usize;
            POLLING_INTERVAL_MILLIS.store(next_polling_interval_millis, Ordering::Relaxed);
            let next_polling_interval = Duration::from_millis(next_polling_interval_millis as u64);
            log::warn!("Cannot poll every {curr_polling_interval:?}, switching to {next_polling_interval:?}");
            next_poll = self.last_poll + next_polling_interval;
        }
        self.last_poll = next_poll;
        next_poll.saturating_duration_since(poll_start)
    }

    /// Manually adjust the polling interval
    ///
    /// Useful in scenarios like debug mode tests where the overhead of polling
    /// is expectedly higher than usual, but within some upper bound.
    ///
    pub fn set_polling_interval(duration: Duration) {
        POLLING_INTERVAL_MILLIS.store(duration.as_millis() as usize, Ordering::Relaxed)
    }

    /// Polling interval to be used
    fn polling_interval() -> Duration {
        Duration::from_millis(POLLING_INTERVAL_MILLIS.load(Ordering::Relaxed) as u64)
    }
}
//
/// PollClock polling interval in milliseconds
///
/// The default interval is empirically enough to measure the max-RSS of
/// compilation tasks with ~100MB precision.
///
/// It may increase if it is detected that the code can't keep up with
/// the expected cadence, in which case throttling is most appropriate.
///
static POLLING_INTERVAL_MILLIS: AtomicUsize = AtomicUsize::new(50);

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::sync::Mutex;

    use super::main::MonitorStatus;
    use super::worker::WorkReceiver;
    use super::*;
    use proptest::prelude::*;

    // Maximum stop flags length used in tests
    const MAX_LEN: usize = 256;

    // Strategy that picks a stop flags length and two distinct indices within
    fn len_and_distinct_idx_pair() -> impl Strategy<Value = (usize, usize, usize)> {
        (2..MAX_LEN)
            .prop_flat_map(|len| (Just(len), (0..len)))
            .prop_flat_map(|(len, idx1)| (Just(len), Just(idx1), 0..(len - 1)))
            .prop_map(|(len, idx1, pre_idx2)| (len, idx1, pre_idx2 + (pre_idx2 >= idx1) as usize))
    }

    proptest! {
        #[test]
        fn stop_flags_init(len in 0..MAX_LEN) {
            // Extract useful parameters
            let has_trailer = (len % BITS_PER_USIZE) != 0;
            let num_full_words = len / BITS_PER_USIZE;

            // Check initial state
            let flags = StopFlags::new(len);
            prop_assert_eq!(flags.0.len(), num_full_words + has_trailer as usize);
            //
            let clients = flags.clients().collect::<Vec<_>>();
            let server = flags.server();
            //
            prop_assert_eq!(server.num_active(), len);
            prop_assert_eq!(server.enumerate_active().count(), len);
            for (expected, actual) in server.enumerate_active().enumerate() {
                prop_assert_eq!(expected, actual);
            }
            //
            prop_assert_eq!(clients.len(), len);
            for client in clients {
                prop_assert!(!client.must_stop());
            }
        }

        #[test]
        fn stop_flags_raise(
            (len, mut raise_idx_1, mut raise_idx_2) in len_and_distinct_idx_pair()
        ) {
            // Set things up
            let flags = StopFlags::new(len);
            let clients = flags.clients().collect::<Vec<_>>();
            let server = flags.server();

            // Raise the first flag
            server.stop(raise_idx_1);
            prop_assert_eq!(server.num_active(), len - 1);
            prop_assert_eq!(server.enumerate_active().count(), len - 1);
            for (expected, actual) in
                ((0..raise_idx_1).chain(raise_idx_1 + 1..len)).zip(server.enumerate_active())
            {
                prop_assert_eq!(expected, actual);
            }
            for (idx, client) in clients.iter().enumerate() {
                prop_assert_eq!(client.must_stop(), idx == raise_idx_1);
            }

            // Raise the second flag
            server.stop(raise_idx_2);
            prop_assert_eq!(server.num_active(), len - 2);
            prop_assert_eq!(server.enumerate_active().count(), len - 2);
            if raise_idx_1 > raise_idx_2 {
                std::mem::swap(&mut raise_idx_1, &mut raise_idx_2);
            }
            for (expected, actual) in ((0..raise_idx_1)
                .chain(raise_idx_1 + 1..raise_idx_2)
                .chain(raise_idx_2 + 1..len))
            .zip(server.enumerate_active())
            {
                prop_assert_eq!(expected, actual);
            }
            for (idx, client) in clients.iter().enumerate() {
                prop_assert_eq!(client.must_stop(), idx == raise_idx_1 || idx == raise_idx_2);
            }

            // Raise all the remaining flags
            server.stop_all();
            prop_assert_eq!(server.num_active(), 0);
            prop_assert_eq!(server.enumerate_active().count(), 0);
            for client in clients {
                prop_assert!(client.must_stop());
            }
        }
    }

    #[test]
    fn monitor() {
        fn check_monitor(
            monitor: &Monitor,
            concurrency: usize,
            mut expected_elapsed: impl FnMut(usize) -> usize,
        ) {
            assert_eq!(monitor.concurrency(), concurrency);
            assert_eq!(monitor.elapsed.len(), concurrency);
            for (idx, elapsed) in monitor.elapsed.iter().enumerate() {
                assert_eq!(elapsed.load(Ordering::Relaxed), expected_elapsed(idx));
            }
            assert_eq!(monitor.stop.clients().count(), concurrency);
        }

        for concurrency in [1, 2] {
            // Test observable state at various construction stages
            let monitor = Monitor::new(concurrency);
            check_monitor(&monitor, concurrency, |_| 0);
            let mut server = monitor.server();
            check_monitor(&monitor, concurrency, |_| 0);
            let check_server_update_result = |server: &mut MonitorServer, result| {
                assert_eq!(server.update(), result);
            };
            check_server_update_result(&mut server, MonitorStatus::Running);
            let mut clients = monitor.clients().collect::<Vec<_>>();
            check_monitor(&monitor, concurrency, |_| 0);
            check_server_update_result(&mut server, MonitorStatus::Running);
            let check_system_not_poisoned = |clients: &Vec<MonitorClient>| {
                clients.iter().for_each(|c| std::mem::drop(c.system()));
            };
            check_system_not_poisoned(&clients);

            // Try retaining and switching jobs
            for client_idx in 0..concurrency {
                assert_eq!(clients[client_idx].keep_job(), Ok(()));
                check_monitor(&monitor, concurrency, |idx| (idx == client_idx) as usize);
                check_server_update_result(&mut server, MonitorStatus::Running);
                check_system_not_poisoned(&clients);

                assert_eq!(clients[client_idx].switch_job(), Ok(()));
                check_monitor(&monitor, concurrency, |_| 0);
                check_server_update_result(&mut server, MonitorStatus::Running);
                check_system_not_poisoned(&clients);
            }
        }
    }

    proptest! {
        // This test is pretty slow, can't afford too many runs
        #![proptest_config(ProptestConfig::with_cases(50))]
        #[test]
        fn work_queue_check(
            initial: HashSet<DatabaseEntry>,
            extra: HashSet<DatabaseEntry>,
            num_workers: u8,
        ) {
            let num_workers = num_workers as usize % 3 + 1;
            let remaining = Mutex::new(initial.clone());
            let initial_len = initial.len();
            let mut work_queue = WorkQueue::new(initial.into_iter().collect());

            // Check initial queue state
            prop_assert_eq!(work_queue.global.len(), initial_len);
            prop_assert!(work_queue.stealers.is_empty());
            prop_assert_eq!(
                work_queue.futex.load(Ordering::Relaxed),
                WorkQueue::FUTEX_INITIAL
            );

            // Now add some worker threads
            std::thread::scope(|scope| {
                // Set up workers
                for local_queue in work_queue.local_queues(num_workers) {
                    let work_queue = &work_queue;
                    let remaining = &remaining;
                    scope.spawn(move || {
                        let mut receiver = WorkReceiver::new(local_queue, work_queue);
                        receiver.wait();
                        for task in receiver {
                            prop_assert!(remaining.lock().unwrap().remove(&task));
                        }
                        Ok(())
                    });
                }
                prop_assert_eq!(work_queue.global.len(), initial_len);
                prop_assert_eq!(work_queue.stealers.len(), num_workers);
                prop_assert_eq!(
                    work_queue.futex.load(Ordering::Relaxed),
                    WorkQueue::FUTEX_INITIAL
                );

                // Set up main thread interface to the work queue
                let mut work_sender = work_queue.sender();
                prop_assert_eq!(work_queue.global.len(), initial_len);
                prop_assert_eq!(work_queue.stealers.len(), num_workers);
                prop_assert_eq!(
                    work_queue.futex.load(Ordering::Relaxed),
                    WorkQueue::FUTEX_INITIAL
                );

                // Start workers, wait a bit for them to have processed all tasks
                work_sender.start();
                loop {
                    std::thread::sleep(Duration::from_millis(1));
                    if remaining.lock().unwrap().is_empty() {
                        break;
                    }
                }

                // Inject more tasks into the queue, in a fashion that purposely
                // maximizes racy behavior at the cost of efficiency
                let mut futex_values =
                    std::iter::once(WorkQueue::FUTEX_INITIAL + 1).collect::<HashSet<_>>();
                for task in extra {
                    prop_assert!(remaining.lock().unwrap().insert(task.clone()));
                    work_sender.push(task);
                    prop_assert!(futex_values.insert(work_queue.futex.load(Ordering::Relaxed)));
                    std::thread::yield_now();
                }
                prop_assert!(!futex_values.contains(&WorkQueue::FUTEX_FINISHED));

                // Drop the main thread interface to tell workers that no more work
                // will be coming and they can exit after this batch.
                std::mem::drop(work_sender);
                prop_assert_eq!(
                    work_queue.futex.load(Ordering::Relaxed),
                    WorkQueue::FUTEX_FINISHED
                );
                Ok(())
            })?;

            // Check that workers processed all tasks before exiting
            prop_assert!(work_queue.global.is_empty());
            prop_assert!(work_queue.stealers.iter().all(Stealer::is_empty));
            prop_assert_eq!(
                work_queue.futex.load(Ordering::Relaxed),
                WorkQueue::FUTEX_FINISHED
            );
            prop_assert!(remaining.lock().unwrap().is_empty());
        }
    }

    #[test]
    #[ignore = "Sensitive to machine load, so not suitable for CI"]
    fn poll_clock() {
        // Basic harness
        let initial_interval = PollClock::polling_interval();
        let duration_close = |expected: Duration, actual: Duration| {
            (actual.as_secs_f64() - expected.as_secs_f64()).abs()
                < 0.1 * initial_interval.as_secs_f64()
        };
        let instant_close = |expected: Instant, actual: Instant| {
            duration_close(expected.elapsed(), actual.elapsed())
        };

        // Test initialization
        let mut clock = PollClock::start();
        assert!(instant_close(clock.last_poll, Instant::now()));
        assert_eq!(PollClock::polling_interval(), initial_interval);

        // Test normal operation
        for _ in 0..2 {
            assert!(duration_close(clock.poll_delay(), initial_interval));
            std::thread::sleep(initial_interval);
            assert!(instant_close(clock.last_poll, Instant::now()));
            assert_eq!(PollClock::polling_interval(), initial_interval);
        }

        // Test what happens when a deadline is missed
        std::thread::sleep(2 * initial_interval);
        let next_delay = clock.poll_delay();
        let new_interval = PollClock::polling_interval();
        assert!(new_interval > 2 * initial_interval);
        assert!(new_interval < 3 * initial_interval);
        assert!(next_delay < initial_interval);
    }

    // NOTE: The main full-build profiling functionality is tested by
    //       integration tests.
}
