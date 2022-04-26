//! Duration event handling

use crate::{Pid, StackFrameID, Tid, Timestamp};
use serde::Deserialize;
use serde_json as json;
use std::collections::HashMap;

/// Duration events provide a way to mark a duration of work on a given thread
//
// Used in #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct DurationEvent {
    /// Process ID for the process that output this event
    pub pid: Pid,

    /// Thread ID for the thread that output this event
    pub tid: Tid,

    /// Tracing clock timestamp in microseconds
    pub ts: Timestamp,

    /// Name of the event (for display)
    pub name: Option<String>,

    /// Comma-separated list of categories (for filtering)
    pub cat: Option<String>,

    /// Thread clock timestamp in microseconds
    pub tts: Option<Timestamp>,

    /// Extra arguments (none mandated for duration events)
    ///
    /// In the case of B/E events, arguments should be merged during display
    /// with E event taking priority where a key conflict occurs.
    pub args: Option<HashMap<String, json::Value>>,

    /// Can provide a stack trace using a global stack frame ID
    ///
    /// For complete events, this is the stack trace at the start of the event
    ///
    /// This is mutually exclusive with "stack", you should never see both set
    pub sf: Option<StackFrameID>,

    /// Can provide a stack trace inline, as a list of stack frames starting
    /// from the root of the call stack.
    ///
    /// For complete events, this is the stack trace at the start of the event
    ///
    /// This is mutually exclusive with "sf", you should never see both set
    pub stack: Option<Vec<String>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use crate::{StackFrame, TraceDataObject, TraceEvent};

    #[test]
    fn begin_end() {
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
    fn nested_event() {
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
    fn thread_interleaving() {
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
    fn global_stack() {
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
    fn inline_stack() {
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
}
