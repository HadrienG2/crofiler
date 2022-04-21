use serde::Deserialize;
use serde_json as json;
use std::{collections::HashMap, fs::File, io::Read};

/// Chrome Trace Event Format, per documentation at
/// https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
#[derive(Debug, Deserialize)]
#[allow(non_snake_case)]
#[serde(untagged)]
enum TraceData {
    /// JSON Object Format
    Object(TraceDataObject),

    /// JSON Array Format, essentially an array of event objects, may not be in
    /// timestamp-sorted order, may lack trailing ]
    Array(Vec<TraceEvent>),
}

/// JSON Object Format, may have extra metadata fields
#[derive(Debug, Deserialize)]
#[allow(non_snake_case)]
struct TraceDataObject {
    /// Event objects, may not be in timestamp-sorted order
    traceEvents: Vec<TraceEvent>,

    /// Unit in which timestamps should be displayed.
    ///
    /// "ms" or "ns" ("ms" by default)
    displayTimeUnit: Option<String>,

    /// Linux ftrace data or Windows ETW trace data.
    ///
    /// If this starts with "# tracer:", this is Linux ftrace data,
    /// otherwise this is Windows ETW data.
    systemTraceEvents: Option<String>,

    /// String of BattOr power data
    powerTraceAsString: Option<String>,

    /// Dictionary of stack frames, their ids and their parents that allows
    /// compact representation of stack traces throughout the rest of the
    /// trace file.
    stackFrames: Option<HashMap<StackFrameID, StackFrame>>,

    /// Sampling profiler data from an OS level profiler
    samples: Option<Vec<Sample>>,

    /// Specifies which trace data comes from tracing controller. Its value
    /// should be the key for that specific trace data, e.g. "traceEvents".
    /// Mainly used for clock synchronization.
    controllerTraceDataKey: Option<String>,
}

/// Event description
#[derive(Debug, Deserialize)]
#[serde(tag = "ph")]
enum TraceEvent {
    // Duration events, can be nested, timestamps must be in increasing order
    // for a given thread.
    //
    /// Beginning of some work, must come before corresponding E event
    B(DurationEvent),
    //
    /// End of some work, must come after corresponding B event
    E(DurationEvent),

    /// Complete event = B+E with duration
    X {
        /// Most fields are shared with duration events
        #[serde(flatten)]
        duration_event: DurationEvent,

        /// Can track duration of complete events
        dur: Timestamp,

        /// ...optionally using a thread-local clock as well
        tdur: Option<Timestamp>,
        // Can also specify the stack trace at the end of the event, using the
        // same conventions as the sf/stack of DurationEvent
        esf: Option<StackFrameID>,
        //
        estack: Option<Vec<String>>,
    },
    // TODO: Add instant events, counter events, async events, flow events,
    //       sample events, object events, metadata events, memory dump events,
    //       mark events, clock sync events, context events
}

/// Duration events provide a way to mark a duration of work on a given thread
#[derive(Debug, Deserialize)]
struct DurationEvent {
    /// Process and thread ID that emitted this event
    #[serde(flatten)]
    pid_tid: PidTid,

    /// Tracing clock timestamp in microseconds
    ts: Timestamp,

    /// Name of the event (for display)
    name: Option<String>,

    /// Comma-separated list of categories (for filtering)
    cat: Option<String>,

    /// Thread clock timestamp in microseconds
    tts: Option<Timestamp>,

    /// No required arguments for duration events
    ///
    /// In the case of B/E events, arguments should be merged during display.
    args: Option<HashMap<String, json::Value>>,

    /// Can provide a stack trace using a global stack frame ID
    ///
    /// For complete events, this is the stack trace at the start of the event
    ///
    /// This is mutually exclusive with "stack", you should never see both set
    sf: Option<StackFrameID>,

    /// Can provide a stack trace inline, as a list of stack frames starting
    /// from the root of the call stack.
    ///
    /// For complete events, this is the stack trace at the start of the event
    ///
    /// This is mutually exclusive with "sf", you should never see both set
    stack: Option<Vec<String>>,
}

/// Process and Thread ID from which an event was emitted
#[derive(Debug, Deserialize)]
struct PidTid {
    /// Process ID for the process that output this event
    pid: i32,

    /// Thread ID for the thread that output this event
    tid: i32,
}

/// Clock timestamp with microsecond granularity
type Timestamp = f64;

/// Global stack frame ID (may be either an integer or a string)
#[derive(Debug, Deserialize, PartialEq, Eq, Hash)]
#[serde(untagged, deny_unknown_fields)]
enum StackFrameID {
    Int(i64),
    Str(String),
}

/// Stack frame object
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct StackFrame {
    // DSO ?
    category: String,

    // Symbol name ?
    name: String,

    /// Parent stack frame, if not at the root of the stack
    parent: Option<StackFrameID>,
}

/// Sampling profiler data from an OS level profiler
#[derive(Debug, Deserialize)]
struct Sample {
    /// CPU on which the sample was taken
    cpu: Option<i32>,

    /// Process and thread ID that emitted this event
    #[serde(flatten)]
    pid_tid: PidTid,

    /// Timestamp in fractional microseconds
    ts: Timestamp,

    /// Name of the event that was sampled
    name: String,

    /// Stack frame
    sf: StackFrameID,

    /// Weight for relative impact assessment
    weight: i64,
}

fn main() {
    const FILENAME: &str = "2020-05-25_CombinatorialKalmanFilterTests.cpp.json";
    let mut s = String::new();
    File::open(FILENAME)
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    // FIXME: Go back to TraceData once parser is debugged
    let value = json::from_str::<TraceDataObject>(&s).unwrap();
    println!("{:?}", value);
}
