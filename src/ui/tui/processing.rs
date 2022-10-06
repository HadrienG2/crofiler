//! Processing thread of the TUI interface (owns the ClangTrace and takes care
//! of all the expensive rendering operations to allow good responsiveness).

use crate::ui::display::{
    activity::{display_activity_desc, ActivityDescError},
    metadata::metadata,
};
use clang_time_trace::{
    ActivityTrace, ActivityTraceId, ClangTrace, ClangTraceLoadError, Duration,
    ParsedActivityArgument,
};

use std::{
    collections::HashMap,
    io::Write,
    path::Path,
    sync::{
        mpsc::{self, Receiver, Sender},
        Arc,
    },
    thread::{self, JoinHandle},
};

/// Encapsulation of the processing thread
#[derive(Debug)]
pub struct ProcessingThread {
    /// JoinHandle of the processing thread
    _handle: JoinHandle<()>,

    /// Channel to send instructions to the processing thread
    instruction_sender: Sender<Instruction>,

    /// Channel to receive individual mutable Strings from the processing thread
    string_receiver: Receiver<String>,

    /// Channel to receive lists of activities from the processing thread
    activities_receiver: Receiver<Box<[ActivityInfo]>>,

    /// Channel to receive lists of immutable strings from the processing thread
    strings_receiver: Receiver<Box<[Arc<str>]>>,
}
//
impl ProcessingThread {
    /// Start the processing thread
    pub fn start() -> Self {
        // Set up processing thread state and communication channels
        let (instruction_sender, instruction_receiver) = mpsc::channel();
        let (string_sender, string_receiver) = mpsc::channel();
        let (activities_sender, activities_receiver) = mpsc::channel();
        let (strings_sender, strings_receiver) = mpsc::channel();

        // Spawn the processing thread
        let handle = thread::spawn(move || {
            // Process instructions until the main thread hangs up
            worker(
                instruction_receiver,
                string_sender,
                activities_sender,
                strings_sender,
            );
        });

        // Emit output interface
        Self {
            _handle: handle,
            instruction_sender,
            string_receiver,
            activities_receiver,
            strings_receiver,
        }
    }

    /// Start loading a clang time-trace file
    ///
    /// This operation does not block, enabling the display of an interactive
    /// loading screen by the UI thread. But the processing thread itself will
    /// be unresponsive for the duration of the loading process.
    ///
    /// Once the trace is loaded, the callback will be called with the result.
    ///
    pub fn start_load_trace(
        &mut self,
        path: impl AsRef<Path>,
        callback: impl FnOnce(ClangTraceLoadResult) + Send + 'static,
    ) {
        self.request(Instruction::LoadTrace(
            path.as_ref().into(),
            Box::new(callback),
        ));
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

    /// Get the list of root activities
    pub fn get_root_activities(&self) -> Box<[ActivityInfo]> {
        self.request(Instruction::GetRootActivities);
        Self::fetch(&self.activities_receiver)
    }

    /// Get the list of all activities
    pub fn get_all_activities(&self) -> Box<[ActivityInfo]> {
        self.request(Instruction::GetAllActivities);
        Self::fetch(&self.activities_receiver)
    }

    /// Get the list of a node's direct children
    pub fn get_direct_children(&self, id: ActivityTraceId) -> Box<[ActivityInfo]> {
        self.request(Instruction::GetDirectChildren(id));
        Self::fetch(&self.activities_receiver)
    }

    /// Get the list of all of a node's direct children
    pub fn get_all_children(&self, id: ActivityTraceId) -> Box<[ActivityInfo]> {
        self.request(Instruction::GetAllChildren(id));
        Self::fetch(&self.activities_receiver)
    }

    /// Describe a set of activities
    pub fn describe_activities(
        &self,
        activities: Box<[ActivityTraceId]>,
        max_cols: u16,
    ) -> Box<[Arc<str>]> {
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

/// Result of the ClangTrace loading process
pub type ClangTraceLoadResult = Result<(), ClangTraceLoadError>;

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
    /// Load a clang trace file
    LoadTrace(Box<Path>, Box<dyn FnOnce(ClangTraceLoadResult) + Send>),

    /// Render trace-wide metadata for a certain terminal width
    DescribeTrace(u16),

    /// Get the list of root nodes (reply via activities channel)
    GetRootActivities,

    /// Get the list of all activities (reply via activities channel)
    GetAllActivities,

    /// Get the list of a node's direct children (reply via activities channel)
    GetDirectChildren(ActivityTraceId),

    /// Get the list of all a node's children (reply via activities channel)
    GetAllChildren(ActivityTraceId),

    /// Display a set of activity descriptions
    DescribeActivities {
        activities: Box<[ActivityTraceId]>,
        max_cols: u16,
    },
}

/// Processing thread worker
fn worker(
    instructions: Receiver<Instruction>,
    string: Sender<String>,
    activities: Sender<Box<[ActivityInfo]>>,
    strings: Sender<Box<[Arc<str>]>>,
) {
    // Set up caches for activity parsing and rendering, which are costly
    let mut trace = None;
    fn expect(trace: &mut Option<ClangTrace>) -> &mut ClangTrace {
        trace
            .as_mut()
            .expect("Client requested trace analysis before loading a trace")
    }
    let mut parsed_arg_cache = HashMap::new();
    let mut description_cache = HashMap::new();
    let mut last_max_cols = 0;

    // Process instructions until the main thread hangs up
    for instruction in instructions.iter() {
        match instruction {
            // Load a trace
            Instruction::LoadTrace(path, callback) => {
                parsed_arg_cache.clear();
                description_cache.clear();
                trace = Some({
                    match ClangTrace::from_file(path) {
                        Ok(trace) => {
                            callback(Ok(()));
                            trace
                        }
                        Err(e) => {
                            callback(Err(e));
                            panic!("Failed to load ClangTrace")
                        }
                    }
                });
            }

            // Describe the trace
            Instruction::DescribeTrace(max_cols) => {
                let trace = expect(&mut trace);
                reply(&string, metadata(trace, max_cols))
            }

            // Get the list of root nodes
            Instruction::GetRootActivities => {
                let trace = expect(&mut trace);
                reply(&activities, activity_list(trace.root_activities()))
            }

            // Get the list of all activities
            Instruction::GetAllActivities => {
                let trace = expect(&mut trace);
                reply(&activities, activity_list(trace.all_activities()))
            }

            // Get the list of a node's direct children
            Instruction::GetDirectChildren(id) => {
                let trace = expect(&mut trace);
                reply(
                    &activities,
                    activity_list(trace.activity_trace(id).direct_children()),
                )
            }

            // Get the list of all a node's children
            Instruction::GetAllChildren(id) => {
                let trace = expect(&mut trace);
                reply(
                    &activities,
                    activity_list(trace.activity_trace(id).all_children()),
                )
            }

            // Describe a set of activities
            Instruction::DescribeActivities {
                activities,
                max_cols,
            } => {
                // A screen width change invalidates the description cache
                if max_cols != last_max_cols {
                    description_cache.clear();
                    last_max_cols = max_cols;
                }

                // Describe the requested activities
                let trace = expect(&mut trace);
                reply(
                    &strings,
                    describe_activities(
                        trace,
                        &mut parsed_arg_cache,
                        &mut description_cache,
                        activities,
                        max_cols,
                    ),
                )
            }
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
    trace: &mut ClangTrace,
    parsed_arg_cache: &mut HashMap<ActivityTraceId, ParsedActivityArgument>,
    description_cache: &mut HashMap<ActivityTraceId, Arc<str>>,
    activities: Box<[ActivityTraceId]>,
    max_cols: u16,
) -> Box<[Arc<str>]> {
    // Describe activities
    let result = activities
        .into_vec()
        .into_iter()
        .map(|id| {
            // Have we rendered that activity's description previously ?
            description_cache
                .entry(id)
                .or_insert_with(|| {
                    // Have we parsed that activity's argument previously ?
                    let parsed_arg = parsed_arg_cache
                        .entry(id)
                        .or_insert_with(|| crate::ui::force_parse_arg(trace, id));

                    // Render the activity description
                    let activity_trace = trace.activity_trace(id);
                    let mut output = Vec::new();
                    match display_activity_desc(
                        &mut output,
                        activity_trace.activity().id(),
                        &parsed_arg.resolve(trace),
                        max_cols,
                    ) {
                        Ok(()) => {}
                        Err(ActivityDescError::NotEnoughCols(_)) => {
                            write!(output, "…").expect("IO to a buffer shouldn't fail");
                        }
                        Err(ActivityDescError::IoError(e)) => {
                            unreachable!(
                                "IO to a buffer shouldn't fail, but failed with error {e}"
                            );
                        }
                    }
                    let output = std::str::from_utf8(&output[..])
                        .expect("Activity descriptions should be UTF-8");
                    Arc::<str>::from(output)
                })
                .clone()
        })
        .collect();

    // Conclude on new parser/interner usage after this transaction
    #[cfg(feature = "unstable_interner_stats")]
    trace.log_interner_usage();

    // Emit results
    result
}

// FIXME: Add tests
