//! Processing thread of the TUI interface (owns the ClangTrace and takes care
//! of all the expensive rendering operations to allow good responsiveness).

use clang_time_trace::{ActivityTrace, ActivityTraceId, ClangTrace, ClangTraceLoadError, Duration};
use std::{
    path::Path,
    sync::{
        mpsc::{self, Receiver, Sender},
        Arc, Mutex, TryLockError,
    },
    thread::{self, JoinHandle},
    time,
};

/// Encapsulation of the processing thread
pub struct ProcessingThread<Metadata: Send + Sync + 'static> {
    /// JoinHandle of the processing thread
    handle: JoinHandle<()>,

    /// ClangTrace metadata (if already available and not yet fetched)
    metadata: Arc<Mutex<Option<Result<Metadata, ClangTraceLoadError>>>>,

    /// Channel to send instructions to the processing thread
    instruction_sender: Sender<Instruction>,

    /// Channel to receive lists of activities from the processing thread
    activities_receiver: Receiver<Box<[ActivityInfo]>>,
}
//
impl<Metadata: Send + Sync + 'static> ProcessingThread<Metadata> {
    /// Set up the processing thread, indicating how metadata should be produced
    pub fn new(
        path: &Path,
        generate_metadata: impl Send + FnOnce(&ClangTrace) -> Metadata + 'static,
    ) -> Self {
        // Set up processing thread state and communication channels
        let path = path.to_path_buf();
        let metadata = Arc::new(Mutex::new(None));
        let metadata2 = metadata.clone();
        let (instruction_sender, instruction_receiver) = mpsc::channel();
        let (activities_sender, activities_receiver) = mpsc::channel();

        // Spawn the processing thread
        let handle = thread::spawn(move || {
            // Load the ClangTrace and generate user-specified metadata
            let trace = {
                let mut metadata_lock = metadata2.lock().expect("Main thread has crashed");
                match ClangTrace::from_file(path) {
                    Ok(trace) => {
                        *metadata_lock = Some(Ok(generate_metadata(&trace)));
                        trace
                    }
                    Err(e) => {
                        *metadata_lock = Some(Err(e));
                        panic!("Failed to load ClangTrace")
                    }
                }
            };

            // Process instructions until the main thread hangs up
            worker(trace, instruction_receiver, activities_sender);
        });

        // Wait for the thread to start processing the ClangTrace (as indicated
        // by acquisition of the metadata lock or emission of metadata)
        while metadata
            .try_lock()
            .expect("Processing thread has crashed")
            .is_none()
        {
            thread::sleep(time::Duration::new(0, 10_000));
        }
        Self {
            handle,
            metadata,
            instruction_sender,
            activities_receiver,
        }
    }

    /// Extract the previously requested ClangTrace metadata, if available
    ///
    /// May return...
    /// - None if the ClangTrace is still being processed
    /// - Some(Ok(metadata)) if the processing thread is ready
    /// - Some(Err(error)) if the processing thread failed to load the ClangTrace
    ///
    /// Will panic if the processing thread has panicked or the ClangTrace
    /// metadata has already been extracted through a call to this function
    ///
    pub fn try_extract_metadata(&mut self) -> Option<Result<Metadata, ClangTraceLoadError>> {
        match self.metadata.try_lock() {
            Ok(mut guard) => match guard.take() {
                opt @ Some(_) => opt,
                None => panic!("Metadata has already been extracted"),
            },
            Err(TryLockError::WouldBlock) => None,
            Err(TryLockError::Poisoned(e)) => {
                panic!("Processing thread has crashed: {e}")
            }
        }
    }

    // TODO: Consider making the following requests asynchronous for increased
    //       UI responsiveness once basic functionality is achieved. This will
    //       require attaching a counter to replies which allows telling apart
    //       the reply to one query from that to another query.

    /// Get the list of root nodes
    pub fn root_activities(&self) -> Box<[ActivityInfo]> {
        self.query(Instruction::GetRootNodes);
        Self::fetch(&self.activities_receiver)
    }

    /// Get the list of a node's children
    pub fn direct_children(&self, id: ActivityTraceId) -> Box<[ActivityInfo]> {
        self.query(Instruction::GetChildren(id));
        Self::fetch(&self.activities_receiver)
    }

    /// The processing thread should keep listening to instructions as long as
    /// the main thread is active, otherwise it's strongly indicative of a crash
    fn query(&self, instruction: Instruction) {
        self.instruction_sender
            .send(instruction)
            .expect("Processing thread has likely crashed")
    }

    /// The processing thread should keep replying to instructions as long as
    /// the main thread is active, otherwise it's strongly indicative of a crash
    fn fetch<T: Send + 'static>(receiver: &Receiver<T>) -> T {
        receiver
            .recv()
            .expect("Processing thread has likely crashed")
    }
}

/// Basic activity data as emitted by the processing thread
///
/// This is meant to provide enough info to allow for pruning based on the
/// desired minimal duration criteria (e.g. self_duration > 0.5% total), before
/// querying for the name info which is more expensive to render.
///
pub struct ActivityInfo {
    /// Identifier that can be used to refer to the activity in later requests
    pub id: ActivityTraceId,

    /// Time spent processing this activity or one of its callees
    pub duration: Duration,

    /// Time spent specificially processing this activity
    pub self_duration: Duration,
}

/// Instructions that can be sent to the processing thread
enum Instruction {
    /// Get the list of root nodes (reply via activities channel)
    GetRootNodes,

    /// Get the list of a node's children (reply via activities channel)
    GetChildren(ActivityTraceId),
}

/// Processing thread worker
fn worker(
    trace: ClangTrace,
    instructions: Receiver<Instruction>,
    activities: Sender<Box<[ActivityInfo]>>,
) {
    // Process instructions until the main thread hangs up
    while let Ok(instruction) = instructions.recv() {
        match instruction {
            // Get the list of root nodes
            Instruction::GetRootNodes => reply(&activities, activity_list(trace.root_activities())),

            // Get the list of a node's children
            Instruction::GetChildren(id) => reply(
                &activities,
                activity_list(trace.activity(id).direct_children()),
            ),
        }
    }
}

/// When the main thread asks for something, it should wait for the reply before
/// quitting, otherwise it's strongly indicative of a crash.
fn reply<T: Send + 'static>(sender: &Sender<T>, data: T) {
    sender.send(data).expect("Main thread has likely crashed")
}

/// Build a list of activities
fn activity_list<'a>(iterator: impl Iterator<Item = ActivityTrace<'a>>) -> Box<[ActivityInfo]> {
    iterator
        .map(|activity_trace| ActivityInfo {
            id: activity_trace.id(),
            duration: activity_trace.duration(),
            self_duration: activity_trace.self_duration(),
        })
        .collect()
}

// FIXME: Add tests
