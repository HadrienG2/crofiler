//! Stack trace handling

use serde::Deserialize;

/// Stack trace associated with a duration event
///
/// For complete events, this is the stack trace at the start of the event
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_camel_case_types)]
pub enum StackTrace {
    /// id for a stackFrame object in the TraceDataObject::stackFrames map
    sf(StackFrameId),

    /// Inline stack trace, as a list of symbols/addresses starting from the root
    stack(Box<[Box<str>]>),
}

/// Stack trace at the end of a complete event
//
// Basically a clone of StackTrace which only exists because of field naming
// differences... but the duplication isn't too bad, so I'll leave it be.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[allow(non_camel_case_types)]
pub enum EndStackTrace {
    /// id for a stackFrame object in the TraceDataObject::stackFrames map
    esf(StackFrameId),

    /// Inline stack trace, as a list of symbols/addresses starting from the root
    estack(Box<[Box<str>]>),
}

/// Global stack frame ID
///
/// The Chrome Trace Event format allows stack frame IDs to be either integers
/// or strings, but in the end that's a bit pointless since stackFrames keys
/// _must_ be strings to comply with the JSON spec. So we convert everything to
/// strings for convenience.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(from = "RawStackFrameId")]
pub struct StackFrameId(pub Box<str>);
//
impl From<RawStackFrameId> for StackFrameId {
    fn from(i: RawStackFrameId) -> Self {
        Self(match i {
            RawStackFrameId::Int(i) => i.to_string().into_boxed_str(),
            RawStackFrameId::Str(s) => s,
        })
    }
}
//
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(untagged, deny_unknown_fields)]
enum RawStackFrameId {
    Int(i64),
    Str(Box<str>),
}

/// Stack frame object
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct StackFrame {
    /// Usually a DSO or process name
    pub category: Box<str>,

    /// Symbol name or address
    pub name: Box<str>,

    /// Parent stack frame, if not at the root of the stack
    pub parent: Option<StackFrameId>,
}
