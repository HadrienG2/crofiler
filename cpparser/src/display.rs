//! Tools to help customize the display of C++ entities

use std::{
    cell::RefCell,
    fmt::{self, Formatter},
};

/// Trait implemented by entities with a customizable display
pub trait CustomDisplay {
    /// Maximum recursion depth that is reached, across all recursive areas of
    /// the grammar, when rendering this entity
    fn recursion_depths(&self) -> RecursionDepths;

    /// Display the type, honoring a recursion depth constraint
    fn display(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error>;
}
//
impl<T: CustomDisplay> CustomDisplay for Option<T> {
    fn recursion_depths(&self) -> RecursionDepths {
        self.as_ref()
            .map(|inner| inner.recursion_depths())
            .unwrap_or(RecursionDepths::NEVER)
    }

    fn display(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if let Some(inner) = self {
            inner.display(f, state)
        } else {
            Ok(())
        }
    }
}

/// Maximal recursion depth to be used while rendering an entity
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct RecursionDepths {
    /// Total recursion depth no matter the entity type
    #[cfg_attr(feature = "clap", clap(long, default_value = "999999999"))]
    pub total: usize,

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
        total: usize::MIN,
        scopes: usize::MIN,
        templates: usize::MIN,
        function_parameters: usize::MIN,
        function_calls: usize::MIN,
        declarators: usize::MIN,
        expressions: usize::MIN,
    };

    /// Allow unbounded recursion
    pub const ALWAYS: Self = Self {
        total: usize::MAX,
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
            total: self.total.max(other.total),
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

/// State that is tracked while displaying things
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct DisplayState(RefCell<DisplayStateInner>);
//
#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct DisplayStateInner {
    /// Recursion limit from current point
    max_recursion: RecursionDepths,
}
//
impl DisplayState {
    /// Set up a display state with certain recursion limits
    pub fn new(max_recursion: RecursionDepths) -> Self {
        Self(RefCell::new(DisplayStateInner { max_recursion }))
    }

    /// Test if a certain recursion is possible under the current limit
    pub fn can_recurse<What: FnMut(&mut RecursionDepths) -> &mut usize>(
        &self,
        mut what: What,
    ) -> bool {
        let mut state = self.0.borrow_mut();
        state.max_recursion.total > 0 && *what(&mut state.max_recursion) > 0
    }

    /// Enter a new level of recursion, returns Err if recursion limit reached
    pub fn recurse<What: FnMut(&mut RecursionDepths) -> &mut usize>(
        &self,
        mut what: What,
    ) -> Result<RecursionGuard<What>, ()> {
        if !self.can_recurse(&mut what) {
            return Err(());
        }
        {
            let mut state = self.0.borrow_mut();
            state.max_recursion.total -= 1;
            *what(&mut state.max_recursion) -= 1;
        }
        Ok(RecursionGuard { state: self, what })
    }
}
//
/// Marker that recursion is ongoing, to be dropped automatically
pub struct RecursionGuard<'depths, What: FnMut(&mut RecursionDepths) -> &mut usize> {
    /// Recursion depths to be incremented later on
    state: &'depths DisplayState,

    /// Field of recursion_depths to be incremented at the end
    what: What,
}
//
impl<What: FnMut(&mut RecursionDepths) -> &mut usize> Drop for RecursionGuard<'_, What> {
    fn drop(&mut self) {
        let mut state = self.state.0.borrow_mut();
        state.max_recursion.total += 1;
        *(self.what)(&mut state.max_recursion) += 1;
    }
}
