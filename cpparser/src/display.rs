//! Tools to help customize the display of C++ entities

use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter},
};

/// Trait implemented by entities with a customizable display
pub trait CustomDisplay {
    /// Maximum recursion depth that is reached, across all recursive areas of
    /// the grammar, when rendering this entity
    fn recursion_depth(&self) -> usize;

    /// Display the type, honoring user-specified constraints
    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error>;

    /// Convenience layer over display_impl, returns a type that implements Display
    fn display<'a>(&'a self, state: &'a DisplayState) -> CustomDisplayView<Self> {
        CustomDisplayView { inner: self, state }
    }
}
//
/// View of a CustomDisplay type + display configuration that can be displayed
pub struct CustomDisplayView<'inner, Inner: CustomDisplay + ?Sized> {
    /// Entity to be displayed
    inner: &'inner Inner,

    /// Display configuration
    state: &'inner DisplayState,
}
//
impl<Inner: CustomDisplay> Display for CustomDisplayView<'_, Inner> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.inner.display_impl(f, self.state)
    }
}

// Optional types get an optional display
impl<T: CustomDisplay> CustomDisplay for Option<T> {
    fn recursion_depth(&self) -> usize {
        self.as_ref()
            .map(|inner| inner.recursion_depth())
            .unwrap_or(0)
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if let Some(inner) = self {
            inner.display_impl(f, state)
        } else {
            Ok(())
        }
    }
}

/// State that is tracked while displaying things
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct DisplayState(RefCell<DisplayStateInner>);
//
#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct DisplayStateInner {
    /// Recursion limit from current point
    max_recursion: usize,
}
//
impl DisplayState {
    /// Set up a display state with certain recursion limits
    pub fn new(max_recursion: usize) -> Self {
        Self(RefCell::new(DisplayStateInner { max_recursion }))
    }

    /// Test if a certain recursion is possible under the current limit
    pub fn can_recurse(&self) -> bool {
        let state = self.0.borrow();
        state.max_recursion > 0
    }

    /// Enter a new level of recursion or return Err if recursion limit reached
    pub fn recurse(&self) -> Result<RecursionGuard, ()> {
        if !self.can_recurse() {
            return Err(());
        }
        {
            let mut state = self.0.borrow_mut();
            state.max_recursion -= 1;
        }
        Ok(RecursionGuard { state: self })
    }
}
//
/// Marker that recursion is ongoing, to be dropped automatically
pub struct RecursionGuard<'state> {
    /// Display state to be updated after recursion finishes
    state: &'state DisplayState,
}
//
impl Drop for RecursionGuard<'_> {
    fn drop(&mut self) {
        let mut state = self.state.0.borrow_mut();
        state.max_recursion += 1;
    }
}
