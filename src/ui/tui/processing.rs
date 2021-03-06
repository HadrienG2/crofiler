//! Processing thread of the TUI interface (owns the ClangTrace and takes care
//! of all the expensive rendering operations to allow good responsiveness).

use crate::ui::display::{
    activity::{display_activity_desc, ActivityDescError},
    metadata::metadata,
};
use clang_time_trace::{ActivityTrace, ActivityTraceId, ClangTrace, ClangTraceLoadError, Duration};
use std::{
    io::Write,
    path::Path,
    sync::{
        mpsc::{self, Receiver, Sender},
        Arc, Mutex, TryLockError,
    },
    thread::{self, JoinHandle},
    time,
};

/// Encapsulation of the processing thread
pub struct ProcessingThread {
    /// JoinHandle of the processing thread
    _handle: JoinHandle<()>,

    /// Result of loading the trace
    load_result: Arc<Mutex<Option<Result<(), ClangTraceLoadError>>>>,

    /// Channel to send instructions to the processing thread
    instruction_sender: Sender<Instruction>,

    /// Channel to receive individual mutable Strings from the processing thread
    string_receiver: Receiver<String>,

    /// Channel to receive lists of activities from the processing thread
    activities_receiver: Receiver<Box<[ActivityInfo]>>,

    /// Channel to receive lists of immutable strings from the processing thread
    strings_receiver: Receiver<Box<[Box<str>]>>,
}
//
impl ProcessingThread {
    /// Start the processing thread
    pub fn start(path: &Path) -> Self {
        // Set up processing thread state and communication channels
        let path = path.to_path_buf();
        let load_result = Arc::new(Mutex::new(None));
        let load_result_2 = load_result.clone();
        let (instruction_sender, instruction_receiver) = mpsc::channel();
        let (string_sender, string_receiver) = mpsc::channel();
        let (activities_sender, activities_receiver) = mpsc::channel();
        let (strings_sender, strings_receiver) = mpsc::channel();

        // Spawn the processing thread
        let handle = thread::spawn(move || {
            // Load the ClangTrace and generate user-specified metadata
            let trace = {
                let mut result_lock = load_result_2.lock().expect("Main thread has crashed");
                match ClangTrace::from_file(path) {
                    Ok(trace) => {
                        *result_lock = Some(Ok(()));
                        trace
                    }
                    Err(e) => {
                        *result_lock = Some(Err(e));
                        panic!("Failed to load ClangTrace")
                    }
                }
            };

            // Process instructions until the main thread hangs up
            worker(
                trace,
                instruction_receiver,
                string_sender,
                activities_sender,
                strings_sender,
            );
        });

        // Wait for the thread to start processing the ClangTrace (as indicated
        // by acquisition of the result lock or emission of a result)
        while match load_result.try_lock() {
            Ok(guard) => guard.is_none(),
            Err(TryLockError::WouldBlock) => false,
            Err(TryLockError::Poisoned(e)) => panic!("Processing thread has crashed: {e}"),
        } {
            thread::sleep(time::Duration::new(0, 10_000));
        }
        Self {
            _handle: handle,
            load_result,
            instruction_sender,
            string_receiver,
            activities_receiver,
            strings_receiver,
        }
    }

    /// Query the outcome of loading the ClangTrace
    ///
    /// May return...
    /// - None if the ClangTrace is still being loaded
    /// - Some(Ok(())) if the trace was successfully loaded
    /// - Some(Err(error)) if the ClangTrace could not be loaded
    ///
    /// Will panic if the processing thread has panicked or the result has
    /// already been extracted through a call to this function
    ///
    pub fn try_extract_load_result(&mut self) -> Option<Result<(), ClangTraceLoadError>> {
        match self.load_result.try_lock() {
            Ok(mut guard) => match guard.take() {
                opt @ Some(_) => opt,
                None => panic!("Load result has already been extracted"),
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

    /// Describe the trace for a certain terminal width
    pub fn describe_trace(&self, max_cols: u16) -> String {
        self.request(Instruction::DescribeTrace(max_cols));
        Self::fetch(&self.string_receiver)
    }

    /// Get the list of root nodes
    pub fn get_root_activities(&self) -> Box<[ActivityInfo]> {
        self.request(Instruction::GetRootActivities);
        Self::fetch(&self.activities_receiver)
    }

    /// Get the list of a node's children
    pub fn get_direct_children(&self, id: ActivityTraceId) -> Box<[ActivityInfo]> {
        self.request(Instruction::GetDirectChildren(id));
        Self::fetch(&self.activities_receiver)
    }

    /// Describe a set of activities
    pub fn describe_activities(
        &self,
        activities: Box<[ActivityTraceId]>,
        max_cols: u16,
    ) -> Box<[Box<str>]> {
        self.request(Instruction::DescribeActivities {
            activities,
            max_cols,
        });
        Self::fetch(&self.strings_receiver)
    }

    /// The processing thread should keep listening to instructions as long as
    /// the main thread is active, otherwise it's strongly indicative of a crash
    fn request(&self, instruction: Instruction) {
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
/// This query provides easily accessible info. The caller may decide to
/// eliminate activities based on the desired minimal duration criterion (e.g.
/// self_duration > 0.5% total) before requesting activity descriptions, which
/// are more expensive to render.
///
pub struct ActivityInfo {
    /// Identifier that can be used to refer to the activity in later requests
    pub id: ActivityTraceId,

    /// Time spent processing this activity or one of its callees
    pub duration: Duration,

    /// Time spent specificially processing this activity
    pub self_duration: Duration,

    /// Truth that this activity has children
    pub has_children: bool,
}

/// Instructions that can be sent to the processing thread
enum Instruction {
    /// Render trace-wide metadata for a certain terminal width
    DescribeTrace(u16),

    /// Get the list of root nodes (reply via activities channel)
    GetRootActivities,

    /// Get the list of a node's children (reply via activities channel)
    GetDirectChildren(ActivityTraceId),

    /// Display a set of activity descriptions
    DescribeActivities {
        activities: Box<[ActivityTraceId]>,
        max_cols: u16,
    },
}

/// Processing thread worker
fn worker(
    trace: ClangTrace,
    instructions: Receiver<Instruction>,
    string: Sender<String>,
    activities: Sender<Box<[ActivityInfo]>>,
    strings: Sender<Box<[Box<str>]>>,
) {
    // Process instructions until the main thread hangs up
    for instruction in instructions.iter() {
        match instruction {
            // Describe the trace
            Instruction::DescribeTrace(max_cols) => reply(&string, metadata(&trace, max_cols)),

            // Get the list of root nodes
            Instruction::GetRootActivities => {
                reply(&activities, activity_list(trace.root_activities()))
            }

            // Get the list of a node's children
            Instruction::GetDirectChildren(id) => reply(
                &activities,
                activity_list(trace.activity(id).direct_children()),
            ),

            // Describe a set of activities
            Instruction::DescribeActivities {
                activities,
                max_cols,
            } => reply(&strings, describe_activities(&trace, activities, max_cols)),
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
            has_children: activity_trace.direct_children().next() != None,
        })
        .collect()
}

/// Describe a list of activities
fn describe_activities(
    trace: &ClangTrace,
    activities: Box<[ActivityTraceId]>,
    max_cols: u16,
) -> Box<[Box<str>]> {
    activities
        .into_vec()
        .into_iter()
        .map(|id| {
            let activity_trace = trace.activity(id);
            let mut output = Vec::new();
            match display_activity_desc(
                &mut output,
                activity_trace.activity().id(),
                &activity_trace.activity().argument(&trace),
                max_cols,
            ) {
                Ok(()) => {}
                Err(ActivityDescError::NotEnoughCols(_)) => {
                    write!(output, "???").expect("IO to a buffer shouldn't fail");
                }
                Err(ActivityDescError::IoError(e)) => {
                    unreachable!("IO to a buffer shouldn't fail, but failed with error {e}");
                }
            }
            String::from_utf8(output)
                .expect("Activity descriptions should be UTF-8")
                .into()
        })
        .collect()
}

// FIXME: Add tests
