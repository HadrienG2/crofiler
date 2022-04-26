use serde::Deserialize;
use serde_json as json;
use std::{collections::HashMap, fs::File, io::Read};

/// Chrome Trace Event Format, per documentation at
/// https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_snake_case)]
#[serde(untagged, deny_unknown_fields)]
enum TraceData {
    /// JSON Object Format
    Object(TraceDataObject),

    /// JSON Array Format, essentially an array of event objects, may not be in
    /// timestamp-sorted order, may lack trailing ]
    Array(Vec<TraceEvent>),
}

/// JSON Object Format
//
// May have extra metadata fields, so should not get the
// #[serde(deny_unknown_fields)] treatment
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
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
    ///
    /// We only accept strings as stack frame IDs here, as this is a JSON
    /// dictionary and JSON mandates that dict keys be strings.
    stackFrames: Option<HashMap<String, StackFrame>>,

    /// Sampling profiler data from an OS level profiler
    samples: Option<Vec<Sample>>,

    /// Specifies which trace data comes from tracing controller. Its value
    /// should be the key for that specific trace data, e.g. "traceEvents".
    /// Mainly used for clock synchronization.
    controllerTraceDataKey: Option<String>,
}

/// Event description
//
// Has a #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Deserialize, PartialEq)]
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
        //
        // Can also specify the stack trace at the end of the event, using the
        // same conventions as the sf/stack of DurationEvent
        esf: Option<StackFrameID>,
        //
        estack: Option<Vec<String>>,
    },

    /// Metadata event associates extra information with the events
    M(MetadataEvent),
    //
    // TODO: Add instant events, counter events, async events, flow events,
    //       sample events, object events, memory dump events,
    //       mark events, clock sync events, context events
}

/// Duration events provide a way to mark a duration of work on a given thread
//
// Used in #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
struct DurationEvent {
    /// Process ID for the process that output this event
    pid: Pid,

    /// Thread ID for the thread that output this event
    tid: Tid,

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
    /// In the case of B/E events, arguments should be merged during display
    /// with E event taking priority where a key conflict occurs.
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

/// Process ID (following libc)
type Pid = i32;

/// Thread ID (following libc)
type Tid = i32;

/// Clock timestamp with microsecond granularity
type Timestamp = f64;

/// Global stack frame ID (may be either an integer or a string)
#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Hash)]
#[serde(untagged, deny_unknown_fields)]
enum StackFrameID {
    Int(i64),
    Str(String),
}

/// Metadata events are used to associate extra information with the events in
/// the trace file. This information can be things like process names, or thread
/// names. Metadata events are denoted by the M phase type. The argument list
/// may be empty.
//
// Uses #[serde(flatten)] so no #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_camel_case_types)]
#[serde(tag = "name")]
enum MetadataEvent {
    /// Sets the display name for the provided pid
    ///
    /// Must contain a "name" arg mapping into a name string
    process_name {
        /// Common fields
        #[serde(flatten)]
        metadata_fields: MetadataFields,

        /// Process ID for the process that output this event
        pid: Pid,

        /// Thread ID for the thread that output this event
        tid: Option<Tid>,
    },

    /// Sets the extra process labels for the provided pid
    ///
    /// Must contain a "labels" arg mapping into a list of string labels.
    process_labels {
        /// Common fields
        #[serde(flatten)]
        metadata_fields: MetadataFields,

        /// Process ID for the process that output this event
        pid: Pid,

        /// Thread ID for the thread that output this event
        tid: Option<Tid>,
    },

    /// Sets the process sort order position
    ///
    /// Must contain a "sort_index" arg mapping into a number that
    /// represents the relative sorting position. Processes with identical
    /// keys are sorted by name, then by pid.
    process_sort_index {
        /// Common fields
        #[serde(flatten)]
        metadata_fields: MetadataFields,

        /// Process ID for the process that output this event
        pid: Pid,

        /// Thread ID for the thread that output this event
        tid: Option<Tid>,
    },

    /// Sets the display name for the provided tid
    ///
    /// Must contain a "name" arg mapping into a name string
    thread_name {
        /// Common fields
        #[serde(flatten)]
        metadata_fields: MetadataFields,

        /// Thread ID for the thread that output this event
        tid: Tid,

        /// Process ID for the process that output this event
        pid: Option<Pid>,
    },

    /// Sets the thread sort order position
    ///
    /// Must contain a "sort_index" arg mapping into a number that
    /// represents the relative sorting position. Threads with identical
    /// keys are sorted by name, then by tid.
    thread_sort_index {
        /// Common fields
        #[serde(flatten)]
        metadata_fields: MetadataFields,

        /// Thread ID for the thread that output this event
        tid: Tid,

        /// Process ID for the process that output this event
        pid: Option<Pid>,
    },
}

/// Common fields for all MetadataEvents
//
// Used in #[serde(flatten)] so no #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
struct MetadataFields {
    /// Event arguments
    ///
    /// All MetadataEvents have one required argument that you should check in
    /// their documentation.
    args: HashMap<String, json::Value>,

    /// Comma-separated list of categories (for filtering)
    cat: Option<String>,

    /// Tracing clock timestamp in microseconds
    ts: Option<Timestamp>,

    /// Thread clock timestamp in microseconds
    tts: Option<Timestamp>,
}

/// Stack frame object
#[derive(Clone, Debug, Deserialize, PartialEq)]
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
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
struct Sample {
    /// CPU on which the sample was taken
    cpu: Option<CpuId>,

    /// Thread ID that emitted this event
    tid: Tid,

    /// Timestamp in fractional microseconds
    ts: Timestamp,

    /// Name of the event that was sampled
    name: String,

    /// Stack frame
    sf: StackFrameID,

    /// Weight for relative impact assessment
    weight: SampleWeight,
}

/// CPU identifier
type CpuId = i32;

/// Sample weight
type SampleWeight = i64;

fn main() {
    const FILENAME: &str = "2020-05-25_CombinatorialKalmanFilterTests.cpp.json";
    let mut s = String::new();
    File::open(FILENAME)
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let value = json::from_str::<TraceData>(&s).unwrap();
    println!("{:?}", value);
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    /// Check that a TraceDataObject is correctly parsed
    fn check_trace_data_object(json_str: &str, expected: TraceDataObject) {
        // Start by parsing as TraceDateObject as this will result in better
        // error messages in case of failure, but check that TraceData parsing
        // works as well.
        let actual1 =
            json::from_str::<TraceDataObject>(json_str).expect("Deserialization should succeed");
        assert_eq!(expected, actual1);
        let actual2 =
            json::from_str::<TraceData>(json_str).expect("Deserialization should succeed");
        assert_eq!(TraceData::Object(expected), actual2);
    }

    /// Check that a TraceData array is correctly parsed, including in object format
    fn check_trace_data_array(json_str: &str, expected: &[TraceEvent]) {
        // Direct parsing
        let value = json::from_str::<TraceData>(json_str).expect("Deserialization should succeed");
        assert_eq!(value, TraceData::Array(expected.to_vec()));

        // Parsing of corresponding TraceDataObject
        let mut object_str = r#"{"traceEvents":"#.to_owned();
        object_str.push_str(json_str);
        object_str.push('}');
        let expected = TraceDataObject {
            traceEvents: expected.to_vec(),
            ..TraceDataObject::default()
        };
        check_trace_data_object(&object_str, expected);
    }

    /// Check that a single TraceEvent is correctly parsed, as well as the
    /// matching TraceData array and TraceDataObject
    fn check_trace_event(json_str: &str, expected: TraceEvent) {
        // Direct parsing
        let value = json::from_str::<TraceEvent>(json_str).expect("Deserialization should succeed");
        assert_eq!(value, expected);

        // Parsing of corresponding TraceData array
        let mut array_str = "[".to_owned();
        array_str.push_str(json_str);
        array_str.push(']');
        let expected = &[expected];
        check_trace_data_array(&array_str, expected);
    }

    #[test]
    fn json_array_format() {
        // Example from spec
        let example = r#"[ {"name": "Asub", "cat": "PERF", "ph": "B", "pid": 22630, "tid": 22630, "ts": 829},
  {"name": "Asub", "cat": "PERF", "ph": "E", "pid": 22630, "tid": 22630, "ts": 833} ]"#;
        let expected = &[
            TraceEvent::B(DurationEvent {
                pid: 22630,
                tid: 22630,
                ts: 829.0,
                name: Some("Asub".to_owned()),
                cat: Some("PERF".to_owned()),
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                pid: 22630,
                tid: 22630,
                ts: 833.0,
                name: Some("Asub".to_owned()),
                cat: Some("PERF".to_owned()),
                ..DurationEvent::default()
            }),
        ];
        check_trace_data_array(example, expected);
    }

    #[test]
    fn json_object_format() {
        // Example from spec with minimal changes to make it valid
        let example = r#"{
  "traceEvents": [
    {"name": "Asub", "cat": "PERF", "ph": "B", "pid": 22630, "tid": 22630, "ts": 829},
    {"name": "Asub", "cat": "PERF", "ph": "E", "pid": 22630, "tid": 22630, "ts": 833}
  ],
  "displayTimeUnit": "ns",
  "systemTraceEvents": "SystemTraceData",
  "otherData": {
    "version": "My Application v1.0"
  },
  "stackFrames": {
    "a": {
      "category": "libchrome.so",
      "name": "CrRendererMain",
      "parent": 1
    },
    "1": {
      "category": "libc.so",
      "name": "_crtmain"
    },
    "3": {
      "category": "libc.so",
      "name": "_start"
    }
  },
  "samples": [{
    "cpu": 0, "tid": 1, "ts": 1000.0,
    "name": "cycles:HG", "sf": 3, "weight": 1
  }]
}"#;
        let expected = TraceDataObject {
            traceEvents: vec![
                TraceEvent::B(DurationEvent {
                    pid: 22630,
                    tid: 22630,
                    ts: 829.0,
                    name: Some("Asub".to_owned()),
                    cat: Some("PERF".to_owned()),
                    ..DurationEvent::default()
                }),
                TraceEvent::E(DurationEvent {
                    pid: 22630,
                    tid: 22630,
                    ts: 833.0,
                    name: Some("Asub".to_owned()),
                    cat: Some("PERF".to_owned()),
                    ..DurationEvent::default()
                }),
            ],
            displayTimeUnit: Some("ns".to_owned()),
            systemTraceEvents: Some("SystemTraceData".to_owned()),
            stackFrames: Some(maplit::hashmap! {
                "a".to_owned() => StackFrame {
                    category: "libchrome.so".to_owned(),
                    name: "CrRendererMain".to_owned(),
                    parent: Some(StackFrameID::Int(1)),
                },
                "1".to_owned() => StackFrame {
                    category: "libc.so".to_owned(),
                    name: "_crtmain".to_owned(),
                    parent: None,
                },
                "3".to_owned() => StackFrame {
                    category: "libc.so".to_owned(),
                    name: "_start".to_owned(),
                    parent: None,
                }
            }),
            samples: Some(vec![Sample {
                cpu: Some(0),
                tid: 1,
                ts: 1000.0,
                name: "cycles:HG".to_owned(),
                sf: StackFrameID::Int(3),
                weight: 1,
            }]),
            ..TraceDataObject::default()
        };
        check_trace_data_object(example, expected);
    }

    #[test]
    fn duration_event() {
        // Example from spec with minimal changes to make it valid
        let example = r#"[
{"name": "myFunction", "cat": "foo", "ph": "B", "ts": 123, "pid": 2343, "tid": 2347,
 "args": {
   "first": 1
 }
},
{"ph": "E", "ts": 145, "pid": 2343, "tid": 2347,
 "args": {
   "first": 4,
   "second": 2
 }
}]"#;
        let expected = &[
            TraceEvent::B(DurationEvent {
                name: Some("myFunction".to_owned()),
                cat: Some("foo".to_owned()),
                ts: 123.0,
                pid: 2343,
                tid: 2347,
                args: Some(maplit::hashmap! {
                    "first".to_owned() => json::json!(1usize)
                }),
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                ts: 145.0,
                pid: 2343,
                tid: 2347,
                args: Some(maplit::hashmap! {
                    "first".to_owned() => json::json!(4usize),
                    "second".to_owned() => json::json!(2usize)
                }),
                ..DurationEvent::default()
            }),
        ];
        check_trace_data_array(example, expected);
    }

    #[test]
    fn nested_duration_event() {
        // Example from spec with minimal changes to make it valid
        let example = r#"[
{ "pid": 1, "ts": 1.0, "tid": 1, "ph": "B", "name": "A"},
{ "pid": 1, "ts": 1.1, "tid": 1, "ph": "B", "name": "Asub"},
{ "pid": 1, "ts": 3.9, "tid": 1, "ph": "E"},
{ "pid": 1, "ts": 4.0, "tid": 1, "ph": "E"}]"#;
        let expected = &[
            TraceEvent::B(DurationEvent {
                pid: 1,
                ts: 1.0,
                tid: 1,
                name: Some("A".to_owned()),
                ..DurationEvent::default()
            }),
            TraceEvent::B(DurationEvent {
                pid: 1,
                ts: 1.1,
                tid: 1,
                name: Some("Asub".to_owned()),
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                pid: 1,
                ts: 3.9,
                tid: 1,
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                pid: 1,
                ts: 4.0,
                tid: 1,
                ..DurationEvent::default()
            }),
        ];
        check_trace_data_array(example, expected);
    }

    #[test]
    fn thread_interleaving_duration_event() {
        // Example from spec with minimal changes to make it valid
        let example = r#"[
{ "pid": 1, "ts": 1.0, "tid": 1, "ph": "B", "name": "A"},
{ "pid": 1, "ts": 0.9, "tid": 2, "ph": "B", "name": "B"},
{ "pid": 1, "ts": 1.1, "tid": 1, "ph": "E"},
{ "pid": 1, "ts": 4.0, "tid": 2, "ph": "E"}]"#;
        let expected = &[
            TraceEvent::B(DurationEvent {
                pid: 1,
                ts: 1.0,
                tid: 1,
                name: Some("A".to_owned()),
                ..DurationEvent::default()
            }),
            TraceEvent::B(DurationEvent {
                pid: 1,
                ts: 0.9,
                tid: 2,
                name: Some("B".to_owned()),
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                pid: 1,
                ts: 1.1,
                tid: 1,
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                pid: 1,
                ts: 4.0,
                tid: 2,
                ..DurationEvent::default()
            }),
        ];
        check_trace_data_array(example, expected);
    }

    #[test]
    fn duration_with_global_stack() {
        // Example from spec with minimal changes to make it valid
        let example = r#"{
  "traceEvents": [
    { "pid": 1, "tid": 1, "ts": 0.1, "ph": "B", "name": "A", "sf": 7},
    { "pid": 1, "tid": 1, "ts": 0.2, "ph": "E", "name": "A", "sf": 9}
  ],
  "stackFrames": {
    "5": { "name": "main", "category": "my app" },
    "7": { "parent": "5", "name": "SomeFunction", "category": "my app" },
    "9": { "parent": "5", "name": "SomeFunction", "category": "my app" }
  }
}"#;
        let expected = TraceDataObject {
            traceEvents: vec![
                TraceEvent::B(DurationEvent {
                    pid: 1,
                    tid: 1,
                    ts: 0.1,
                    name: Some("A".to_owned()),
                    sf: Some(StackFrameID::Int(7)),
                    ..DurationEvent::default()
                }),
                TraceEvent::E(DurationEvent {
                    pid: 1,
                    tid: 1,
                    ts: 0.2,
                    name: Some("A".to_owned()),
                    sf: Some(StackFrameID::Int(9)),
                    ..DurationEvent::default()
                }),
            ],
            stackFrames: Some(maplit::hashmap! {
                "5".to_owned() => StackFrame {
                    name: "main".to_owned(),
                    category: "my app".to_owned(),
                    parent: None,
                },
                "7".to_owned() => StackFrame {
                    parent: Some(StackFrameID::Str("5".to_owned())),
                    category: "my app".to_owned(),
                    name: "SomeFunction".to_owned(),
                },
                "9".to_owned() => StackFrame {
                    parent: Some(StackFrameID::Str("5".to_owned())),
                    category: "my app".to_owned(),
                    name: "SomeFunction".to_owned(),
                }
            }),
            ..TraceDataObject::default()
        };
        check_trace_data_object(example, expected);
    }

    #[test]
    fn duration_with_inline_stack() {
        // Example from spec with minimal changes to make it valid
        let example = r#"{
            "pid": 1, "tid": 1, "ts": 1.0, "ph": "B", "name": "A", "stack": ["0x1", "0x2"]}"#;
        let expected = TraceEvent::B(DurationEvent {
            pid: 1,
            tid: 1,
            ts: 1.0,
            name: Some("A".to_owned()),
            stack: Some(vec!["0x1".to_owned(), "0x2".to_owned()]),
            ..DurationEvent::default()
        });
        check_trace_event(example, expected);
    }

    #[test]
    fn complete_event() {
        // Example from spec with minimal changes to make it valid
        let example = r#"{
 "name": "myFunction", "cat": "foo", "ph": "X", "ts": 123, "dur": 234, "pid": 2343, "tid": 2347,
 "args": {
   "first": 1
 }
}"#;
        let expected = TraceEvent::X {
            duration_event: DurationEvent {
                name: Some("myFunction".to_owned()),
                cat: Some("foo".to_owned()),
                ts: 123.0,
                pid: 2343,
                tid: 2347,
                args: Some(maplit::hashmap! {
                    "first".to_owned() => json::json!(1usize)
                }),
                ..DurationEvent::default()
            },
            dur: 234.0,
            tdur: None,
            esf: None,
            estack: None,
        };
        check_trace_event(example, expected);
    }

    #[test]
    fn metadata_event() {
        // Example from spec
        let example = r#"{"name": "thread_name", "ph": "M", "pid": 2343, "tid": 2347,
 "args": {
  "name" : "RendererThread"
 }
}"#;
        let expected = TraceEvent::M(MetadataEvent::thread_name {
            pid: Some(2343),
            tid: 2347,
            metadata_fields: MetadataFields {
                args: maplit::hashmap! {
                    "name".to_owned() => json::json!("RendererThread")
                },
                ..MetadataFields::default()
            },
        });
        check_trace_event(example, expected);
    }

    // TODO: Add more examples from the CTF spec
}
