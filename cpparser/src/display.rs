//! Tools to help customize the display of C++ entities

use std::fmt::{self, Formatter};

/// Trait implemented by entities with a customizable display
pub trait CustomDisplay {
    /// Maximum recursion depth that is reached, across all recursive areas of
    /// the grammar, when rendering this entity
    fn recursion_depths(&self) -> RecursionDepths;

    /// Display the type, honoring a recursion depth constraint
    fn display(&self, f: &mut Formatter<'_>, depths: RecursionDepths) -> Result<(), fmt::Error>;
}
//
impl<T: CustomDisplay> CustomDisplay for Option<T> {
    fn recursion_depths(&self) -> RecursionDepths {
        self.as_ref()
            .map(|inner| inner.recursion_depths())
            .unwrap_or(RecursionDepths::NEVER)
    }

    fn display(&self, f: &mut Formatter<'_>, depths: RecursionDepths) -> Result<(), fmt::Error> {
        if let Some(inner) = self {
            inner.display(f, depths)
        } else {
            Ok(())
        }
    }
}

/// Recursion depth to be used while rendering an entity
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct RecursionDepths {
    /// Scopes (x::y::z::... paths)
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub scopes: usize,

    /// Template parameter sets
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub templates: usize,

    /// Function parameter sets (in declaration)
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub function_parameters: usize,

    /// Function argument sets (in calls)
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub function_calls: usize,

    /// Declarators (things that qualify a type like &, const, noexcept...)
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub declarators: usize,

    /// Expressions (in values)
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub expressions: usize,
}
//
impl RecursionDepths {
    /// Prevent any form of grammatically allowed recursion
    pub const NEVER: Self = Self {
        scopes: usize::MIN,
        templates: usize::MIN,
        function_parameters: usize::MIN,
        function_calls: usize::MIN,
        declarators: usize::MIN,
        expressions: usize::MIN,
    };

    /// Allow unbounded recursion
    pub const ALWAYS: Self = Self {
        scopes: usize::MAX,
        templates: usize::MAX,
        function_parameters: usize::MAX,
        function_calls: usize::MAX,
        declarators: usize::MAX,
        expressions: usize::MAX,
    };

    /// Combine two recursion depths, picking the highest limits
    pub fn max(self, other: Self) -> Self {
        Self {
            scopes: self.scopes.max(other.scopes),
            templates: self.templates.max(other.templates),
            function_parameters: self.function_parameters.max(other.function_parameters),
            function_calls: self.function_calls.max(other.function_calls),
            declarators: self.declarators.max(other.declarators),
            expressions: self.expressions.max(other.expressions),
        }
    }
}
//
impl Default for RecursionDepths {
    fn default() -> Self {
        Self::ALWAYS
    }
}
