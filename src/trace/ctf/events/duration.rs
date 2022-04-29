//! Duration event handling

use crate::trace::ctf;
use ctf::{stack::StackTrace, EventCategories, Pid, Tid, Timestamp};
use serde::Deserialize;
use serde_json as json;
use std::collections::HashMap;

/// Duration events provide a way to mark a duration of work on a given thread
//
// Used in #[serde(flatten)] and uses it so no #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct DurationEvent {
    /// Process ID for the process that output this event
    pub pid: Pid,

    /// Thread ID for the thread that output this event
    pub tid: Tid,

    /// Tracing clock timestamp in microseconds
    pub ts: Timestamp,

    /// Name of the event (for display)
    pub name: Option<Box<str>>,

    /// Event categories (for filtering)
    pub cat: Option<EventCategories>,

    /// Thread clock timestamp in microseconds
    pub tts: Option<Timestamp>,

    /// Extra arguments (none mandated for duration events)
    ///
    /// In the case of B/E events, arguments should be merged during display
    /// with E event taking priority where a key conflict occurs.
    pub args: Option<HashMap<Box<str>, json::Value>>,

    /// Stack trace
    #[serde(flatten)]
    pub stack_trace: Option<StackTrace>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use ctf::{
        stack::{StackFrame, StackFrameId},
        tests::*,
        TraceDataObject, TraceEvent,
    };

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
                name: Some("myFunction".into()),
                cat: Some(EventCategories(vec!["foo".into()].into_boxed_slice())),
                ts: 123.0,
                pid: 2343,
                tid: 2347,
                args: Some(maplit::hashmap! {
                    "first".into() => json::json!(1usize)
                }),
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                ts: 145.0,
                pid: 2343,
                tid: 2347,
                args: Some(maplit::hashmap! {
                    "first".into() => json::json!(4usize),
                    "second".into() => json::json!(2usize)
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
                name: Some("A".into()),
                ..DurationEvent::default()
            }),
            TraceEvent::B(DurationEvent {
                pid: 1,
                ts: 1.1,
                tid: 1,
                name: Some("Asub".into()),
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
                name: Some("A".into()),
                ..DurationEvent::default()
            }),
            TraceEvent::B(DurationEvent {
                pid: 1,
                ts: 0.9,
                tid: 2,
                name: Some("B".into()),
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
                    name: Some("A".into()),
                    stack_trace: Some(StackTrace::sf(StackFrameId("7".into()))),
                    ..DurationEvent::default()
                }),
                TraceEvent::E(DurationEvent {
                    pid: 1,
                    tid: 1,
                    ts: 0.2,
                    name: Some("A".into()),
                    stack_trace: Some(StackTrace::sf(StackFrameId("9".into()))),
                    ..DurationEvent::default()
                }),
            ]
            .into_boxed_slice(),
            stackFrames: Some(maplit::hashmap! {
                "5".into() => StackFrame {
                    name: "main".into(),
                    category: "my app".into(),
                    parent: None,
                },
                "7".into() => StackFrame {
                    parent: Some(StackFrameId("5".into())),
                    category: "my app".into(),
                    name: "SomeFunction".into(),
                },
                "9".into() => StackFrame {
                    parent: Some(StackFrameId("5".into())),
                    category: "my app".into(),
                    name: "SomeFunction".into(),
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
            name: Some("A".into()),
            stack_trace: Some(StackTrace::stack(
                vec!["0x1".into(), "0x2".into()].into_boxed_slice(),
            )),
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
                name: Some("myFunction".into()),
                cat: Some(EventCategories(vec!["foo".into()].into_boxed_slice())),
                ts: 123.0,
                pid: 2343,
                tid: 2347,
                args: Some(maplit::hashmap! {
                    "first".into() => json::json!(1usize)
                }),
                ..DurationEvent::default()
            },
            dur: 234.0,
            tdur: None,
            end_stack_trace: None,
        };
        check_trace_event(example, expected);
    }
}
