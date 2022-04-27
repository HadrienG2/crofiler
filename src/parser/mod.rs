//! Parser for the Chrome Trace Event format
//!
//! Based on the documentation available at
//! https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
//!
//! Currently only implements what's necessary to parse the output of clang's
//! -ftime-trace, but could later be extended into a full CTF parser if the need
//! ever arises.

pub mod events;
pub mod stack;

use self::{
    events::{duration::DurationEvent, metadata::MetadataEvent},
    stack::{EndStackTrace, StackFrame, StackFrameId},
};
use serde::Deserialize;
use serde_json as json;
use std::collections::HashMap;

/// Chrome Trace Event Format, per documentation at
/// https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_snake_case)]
#[serde(untagged, deny_unknown_fields)]
pub enum TraceData {
    /// JSON Object Format
    Object(TraceDataObject),

    /// JSON Array Format, essentially an array of event objects, may not be in
    /// timestamp-sorted order, may lack trailing ]
    Array(Box<[TraceEvent]>),
}

/// JSON Object Format
//
// Has a #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[allow(non_snake_case)]
pub struct TraceDataObject {
    /// Event objects, may not be in timestamp-sorted order
    pub traceEvents: Box<[TraceEvent]>,

    /// Unit in which timestamps should be displayed
    #[serde(default)]
    pub displayTimeUnit: DisplayTimeUnit,

    /// Linux ftrace data or Windows ETW trace data.
    ///
    /// If this starts with "# tracer:", this is Linux ftrace data,
    /// otherwise this is Windows ETW data.
    //
    // TODO: Consider parsing this further in a future version
    pub systemTraceEvents: Option<String>,

    /// String of BattOr power data
    //
    // TODO: Consider parsing this further in a future version
    pub powerTraceAsString: Option<String>,

    /// Dictionary of stack frames, their ids and their parents that allows
    /// compact representation of stack traces throughout the rest of the
    /// trace file.
    //
    // NOTE: We only accept strings as stack frame IDs here, as this is a JSON
    //       dictionary and JSON actually mandates that dict keys be strings.
    pub stackFrames: Option<HashMap<String, StackFrame>>,

    /// Sampling profiler data from an OS level profiler
    pub samples: Option<Box<[Sample]>>,

    /// Specifies which trace data comes from tracing controller. Its value
    /// should be the key for that specific trace data, e.g. "traceEvents".
    /// Mainly used for clock synchronization.
    pub controllerTraceDataKey: Option<String>,

    /// Extra trace metadata not specified by the Trace Event Format spec
    #[serde(flatten)]
    pub extra: HashMap<String, json::Value>,
}

/// Event description
//
// Has a #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(tag = "ph")]
pub enum TraceEvent {
    // Duration events, can be nested, timestamps must be in increasing order
    // for a given thread.
    //
    /// Beginning of some work, must come before corresponding E event
    B(DurationEvent),
    //
    /// End of some work, must come after corresponding B event
    E(DurationEvent),

    /// Complete event = combines two consecutive B and E events
    X {
        /// Most fields are shared with duration events
        #[serde(flatten)]
        duration_event: DurationEvent,

        /// Can track duration of complete events
        dur: Duration,

        /// Like dur, but using the tts thread-local clock instead ot the global ts clock
        tdur: Option<Duration>,

        /// Stack trace at the end of the event
        #[serde(flatten)]
        end_stack_trace: Option<EndStackTrace>,
    },

    /// Metadata event associates extra information with the events
    M(MetadataEvent),
    //
    // TODO: Add instant events, counter events, async events, flow events,
    //       sample events, object events, memory dump events,
    //       mark events, clock sync events, context events
}

/// Clock timestamp with microsecond granularity
pub type Timestamp = f64;

/// Durations are just a difference of timestamps
pub type Duration = Timestamp;

/// Unit in which timestamps should be displayed
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_camel_case_types)]
pub enum DisplayTimeUnit {
    /// Milliseconds
    ms,

    /// Nanoseconds
    ns,
}
//
impl Default for DisplayTimeUnit {
    fn default() -> Self {
        Self::ms
    }
}

/// Sampling profiler data from an OS level profiler
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct Sample {
    /// CPU on which the sample was taken
    pub cpu: Option<CpuId>,

    /// Thread ID that emitted this event
    pub tid: Tid,

    /// Timestamp in fractional microseconds
    pub ts: Timestamp,

    /// Name of the event that was sampled
    pub name: String,

    /// Stack frame
    pub sf: StackFrameId,

    /// Weight for relative impact assessment
    pub weight: SampleWeight,
}

/// CPU identifier (following libc)
pub type CpuId = i32;

/// Thread ID (following libc)
pub type Tid = i32;

/// Sample weight
pub type SampleWeight = i64;

/// Process ID (following libc)
pub type Pid = i32;

/// Event categories
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(from = "String")]
pub struct EventCategories(Box<[String]>);
//
impl From<String> for EventCategories {
    fn from(s: String) -> Self {
        Self(
            s.split(',')
                .filter(|sub| sub.len() > 0)
                .map(|sub| sub.to_owned())
                .collect(),
        )
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    /// Check that a TraceDataObject is correctly parsed
    pub fn check_trace_data_object(json_str: &str, expected: TraceDataObject) {
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
    pub fn check_trace_data_array(json_str: &str, expected: &[TraceEvent]) {
        // Direct parsing
        let value = json::from_str::<TraceData>(json_str).expect("Deserialization should succeed");
        assert_eq!(
            value,
            TraceData::Array(expected.to_vec().into_boxed_slice())
        );

        // Parsing of corresponding TraceDataObject
        let mut object_str = r#"{"traceEvents":"#.to_owned();
        object_str.push_str(json_str);
        object_str.push('}');
        let expected = TraceDataObject {
            traceEvents: expected.to_vec().into_boxed_slice(),
            ..TraceDataObject::default()
        };
        check_trace_data_object(&object_str, expected);
    }

    /// Check that a single TraceEvent is correctly parsed, as well as the
    /// matching TraceData array and TraceDataObject
    pub fn check_trace_event(json_str: &str, expected: TraceEvent) {
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
                cat: Some(EventCategories(vec!["PERF".to_owned()].into_boxed_slice())),
                ..DurationEvent::default()
            }),
            TraceEvent::E(DurationEvent {
                pid: 22630,
                tid: 22630,
                ts: 833.0,
                name: Some("Asub".to_owned()),
                cat: Some(EventCategories(vec!["PERF".to_owned()].into_boxed_slice())),
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
                    cat: Some(EventCategories(vec!["PERF".to_owned()].into_boxed_slice())),
                    ..DurationEvent::default()
                }),
                TraceEvent::E(DurationEvent {
                    pid: 22630,
                    tid: 22630,
                    ts: 833.0,
                    name: Some("Asub".to_owned()),
                    cat: Some(EventCategories(vec!["PERF".to_owned()].into_boxed_slice())),
                    ..DurationEvent::default()
                }),
            ]
            .into_boxed_slice(),
            displayTimeUnit: DisplayTimeUnit::ns,
            systemTraceEvents: Some("SystemTraceData".to_owned()),
            stackFrames: Some(maplit::hashmap! {
                "a".to_owned() => StackFrame {
                    category: "libchrome.so".to_owned(),
                    name: "CrRendererMain".to_owned(),
                    parent: Some(StackFrameId("1".to_owned())),
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
            samples: Some(
                vec![Sample {
                    cpu: Some(0),
                    tid: 1,
                    ts: 1000.0,
                    name: "cycles:HG".to_owned(),
                    sf: StackFrameId("3".to_owned()),
                    weight: 1,
                }]
                .into_boxed_slice(),
            ),
            extra: maplit::hashmap! {
                "otherData".to_owned() => json::json!({
                    "version": "My Application v1.0"
                })
            },
            ..TraceDataObject::default()
        };
        check_trace_data_object(example, expected);
    }
}
