//! Metadata event handling

use crate::trace::ctf;
use ctf::{EventCategories, Pid, Tid, Timestamp};
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
    process_name {
        /// Process ID for the process that output this event
        pid: Pid,

        /// Arguments
        args: NameArgs,

        /// Thread ID for the thread that output this event
        tid: Option<Tid>,

        /// Common optional fields
        #[serde(flatten)]
        options: MetadataOptions,
    },

    /// Sets one extra process label for the provided pid
    process_labels {
        /// Process ID for the process that output this event
        pid: Pid,

        /// Arguments
        args: LabelsArgs,

        /// Thread ID for the thread that output this event
        tid: Option<Tid>,

        /// Common optional fields
        #[serde(flatten)]
        options: MetadataOptions,
    },

    /// Sets the process sort order position
    process_sort_index {
        /// Process ID for the process that output this event
        pid: Pid,

        /// Arguments
        args: SortIndexArgs,

        /// Thread ID for the thread that output this event
        tid: Option<Tid>,

        /// Common optional fields
        #[serde(flatten)]
        options: MetadataOptions,
    },

    /// Sets the display name for the provided tid
    thread_name {
        /// Thread ID for the thread that output this event
        tid: Tid,

        /// Arguments
        args: NameArgs,

        /// Process ID for the process that output this event
        pid: Option<Pid>,

        /// Common optional fields
        #[serde(flatten)]
        options: MetadataOptions,
    },

    /// Sets the thread sort order position
    thread_sort_index {
        /// Thread ID for the thread that output this event
        tid: Tid,

        /// Arguments
        args: SortIndexArgs,

        /// Process ID for the process that output this event
        pid: Option<Pid>,

        /// Common optional fields
        #[serde(flatten)]
        options: MetadataOptions,
    },
}

/// Common optional fields for all MetadataEvents
//
// Used in #[serde(flatten)] so no #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct MetadataOptions {
    /// Event categories (for filtering)
    pub cat: Option<EventCategories>,

    /// Tracing clock timestamp in microseconds
    pub ts: Option<Timestamp>,

    /// Thread clock timestamp in microseconds
    pub tts: Option<Timestamp>,
}

/// Arguments for MetadataEvents that name something (a process, a thread...)
//
// Has a #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct NameArgs {
    /// Name to be attributed to target entity
    pub name: String,

    /// Extra arguments not specified by the Trace Event Format spec
    #[serde(flatten)]
    pub extra: HashMap<String, json::Value>,
}

/// Arguments for MetadataEvent::process_labels
//
// Has a #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct LabelsArgs {
    /// Extra label to be attributed to the target process
    pub labels: String,

    /// Extra arguments not specified by the Trace Event Format spec
    #[serde(flatten)]
    pub extra: HashMap<String, json::Value>,
}

/// Arguments for MetadataEvents provide a sort order position
//
// Has a #[serde(flatten)] so should not get #[serde(deny_unknown_fields)]
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct SortIndexArgs {
    /// Relative sorting position
    ///
    /// Entities with identical sort order are sorted by name, then by tid.
    pub sort_order: i64,

    /// Extra arguments not specified by the Trace Event Format spec
    #[serde(flatten)]
    pub extra: HashMap<String, json::Value>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use ctf::{tests::*, TraceEvent};

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
            args: NameArgs {
                name: "RendererThread".to_owned(),
                ..NameArgs::default()
            },
            options: MetadataOptions::default(),
        });
        check_trace_event(example, expected);
    }
}
