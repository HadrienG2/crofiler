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
    time::Duration,
};
use sysinfo::{System, SystemExt};

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
    pub fn new(concurrency: usize) -> Self {
        Self {
            system: RwLock::new(System::new()),
            elapsed: std::iter::repeat_with(|| CachePadded::new(AtomicUsize::new(0)))
                .take(concurrency)
                .collect(),
            stop: StopFlags::new(concurrency),
        }
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
            .map(|(elapsed, stop_client)| MonitorClient::new(self, &elapsed, stop_client))
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

/// Polling interval in seconds
///
/// This is empirically enough to measure max-RSS with ~100MB precision.
///
const POLLING_INTERVAL: Duration = Duration::from_millis(1000 / 30);

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::sync::Mutex;

    use super::main::MonitorStatus;
    use super::worker::WorkReceiver;
    use super::*;
    use quickcheck::{QuickCheck, TestResult};
    use quickcheck_macros::quickcheck;

    #[quickcheck]
    fn stop_flags_init(len: u8) {
        // Extract useful parameters
        let len = len as usize;
        let has_trailer = (len % BITS_PER_USIZE) != 0;
        let num_full_words = len / BITS_PER_USIZE;

        // Check initial state
        let flags = StopFlags::new(len);
        assert_eq!(flags.0.len(), num_full_words + has_trailer as usize);
        //
        let clients = flags.clients().collect::<Vec<_>>();
        let server = flags.server();
        //
        assert_eq!(server.num_active(), len);
        assert_eq!(server.enumerate_active().count(), len);
        for (expected, actual) in server.enumerate_active().enumerate() {
            assert_eq!(expected, actual);
        }
        //
        assert_eq!(clients.len(), len);
        for client in clients {
            assert!(!client.must_stop());
        }
    }

    #[quickcheck]
    fn stop_flags_raise(len: u8, raise_idx_1: u8, raise_idx_2: u8) -> TestResult {
        // Wrap around raise_idx, ignore invalid requests
        if len == 0 {
            return TestResult::discard();
        }
        let raise_idx_1 = raise_idx_1 % len;
        let raise_idx_2 = raise_idx_2 % len;
        if raise_idx_1 == raise_idx_2 {
            return TestResult::discard();
        }

        // Set things up
        let len = len as usize;
        let mut raise_idx_1 = raise_idx_1 as usize;
        let mut raise_idx_2 = raise_idx_2 as usize;
        let flags = StopFlags::new(len);
        let clients = flags.clients().collect::<Vec<_>>();
        let server = flags.server();

        // Raise the first flag
        server.stop(raise_idx_1);
        assert_eq!(server.num_active(), len - 1);
        assert_eq!(server.enumerate_active().count(), len - 1);
        for (expected, actual) in
            ((0..raise_idx_1).chain(raise_idx_1 + 1..len)).zip(server.enumerate_active())
        {
            assert_eq!(expected, actual);
        }
        for (idx, client) in clients.iter().enumerate() {
            assert_eq!(client.must_stop(), idx == raise_idx_1);
        }

        // Raise the second flag
        server.stop(raise_idx_2);
        assert_eq!(server.num_active(), len - 2);
        assert_eq!(server.enumerate_active().count(), len - 2);
        if raise_idx_1 > raise_idx_2 {
            std::mem::swap(&mut raise_idx_1, &mut raise_idx_2);
        }
        for (expected, actual) in ((0..raise_idx_1)
            .chain(raise_idx_1 + 1..raise_idx_2)
            .chain(raise_idx_2 + 1..len))
        .zip(server.enumerate_active())
        {
            assert_eq!(expected, actual);
        }
        for (idx, client) in clients.iter().enumerate() {
            assert_eq!(client.must_stop(), idx == raise_idx_1 || idx == raise_idx_2);
        }

        // Raise all the remaining flags
        server.stop_all();
        assert_eq!(server.num_active(), 0);
        assert_eq!(server.enumerate_active().count(), 0);
        for client in clients {
            assert!(client.must_stop());
        }
        TestResult::passed()
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
        assert_eq!(work_queue.global.len(), initial_len);
        assert!(work_queue.stealers.is_empty());
        assert_eq!(
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
                    let receiver = WorkReceiver::new(local_queue, work_queue);
                    for task in receiver {
                        assert!(remaining.lock().unwrap().remove(&task));
                    }
                });
            }
            assert_eq!(work_queue.global.len(), initial_len);
            assert_eq!(work_queue.stealers.len(), num_workers);
            assert_eq!(
                work_queue.futex.load(Ordering::Relaxed),
                WorkQueue::FUTEX_INITIAL
            );

            // Set up main thread interface to the work queue
            let mut work_sender = work_queue.sender();
            assert_eq!(work_queue.global.len(), initial_len);
            assert_eq!(work_queue.stealers.len(), num_workers);
            assert_eq!(
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
                assert!(remaining.lock().unwrap().insert(task.clone()));
                work_sender.push(task);
                assert!(futex_values.insert(work_queue.futex.load(Ordering::Relaxed)));
                std::thread::yield_now();
            }
            assert!(!futex_values.contains(&WorkQueue::FUTEX_FINISHED));

            // Drop the main thread interface to tell workers that no more work
            // will be coming and they can exit after this batch.
            std::mem::drop(work_sender);
            assert_eq!(
                work_queue.futex.load(Ordering::Relaxed),
                WorkQueue::FUTEX_FINISHED
            );
        });

        // Check that workers processed all tasks before exiting
        assert!(work_queue.global.is_empty());
        assert!(work_queue.stealers.iter().all(Stealer::is_empty));
        assert_eq!(
            work_queue.futex.load(Ordering::Relaxed),
            WorkQueue::FUTEX_FINISHED
        );
        assert!(remaining.lock().unwrap().is_empty());
    }

    #[test]
    fn work_queue() {
        // This test is pretty slow, can only afford few configurations
        QuickCheck::new()
            .tests(5)
            .quickcheck(work_queue_check as fn(HashSet<DatabaseEntry>, HashSet<DatabaseEntry>, u8))
    }

    // NOTE: The main full-build profiling functionality is tested by
    //       integration tests.
}
