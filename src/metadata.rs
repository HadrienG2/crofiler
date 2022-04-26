//! Metadata event handling

use crate::{Pid, Tid, Timestamp};
use serde::Deserialize;
use serde_json as json;
use std::collections::HashMap;

/// Metadata events are used to associate extra information with the events in
/// the trace file. This information can be things like process names, or thread
/// names. Metadata events are denoted by the M phase type. The argument list
/// may be empty.
//
// Uses #[serde(flatten)] so no #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_camel_case_types)]
#[serde(tag = "name")]
pub enum MetadataEvent {
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
pub struct MetadataFields {
    /// Event arguments
    ///
    /// All MetadataEvents have one required argument that you should check in
    /// their documentation.
    pub args: HashMap<String, json::Value>,

    /// Comma-separated list of categories (for filtering)
    pub cat: Option<String>,

    /// Tracing clock timestamp in microseconds
    pub ts: Option<Timestamp>,

    /// Thread clock timestamp in microseconds
    pub tts: Option<Timestamp>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use crate::TraceEvent;

    #[test]
    fn event() {
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
}
