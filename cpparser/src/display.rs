//! Tools to help customize the display of C++ entities

use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter, Write},
};
use unicode_width::UnicodeWidthStr;

/// Trait implemented by entities with a customizable display
pub trait CustomDisplay {
    /// Maximum recursion depth that is reached, across all recursive areas of
    /// the grammar, when rendering this entity
    fn recursion_depth(&self) -> usize;

    /// Display the type, honoring user-specified constraints
    ///
    /// It is guaranteed that the DisplayState at the end of execution will be
    /// configured as it was in the beginning of execution. Please reuse it when
    /// displaying entities in a row in order to benefit from features like
    /// allocation reuse and redundant text deduplication.
    ///
    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error>;

    /// Convenience layer over display_impl, returns a type that implements Display
    fn display<'a>(&'a self, state: &'a DisplayState) -> CustomDisplayView<Self> {
        CustomDisplayView { inner: self, state }
    }

    /// Display something, using up at most N terminal columns
    //
    // FIXME: Integrate char budget and buffers into DisplayState and
    //        display_impl in the final tree-based display implementation.
    //
    fn bounded_display(&self, max_cols: u16) -> String {
        assert!(max_cols >= 1, "Cannot display anything with 0 columns...");
        let mut prev_display = "…".to_string();
        let mut curr_display = String::new();
        for recursion_depth in 0..=self.recursion_depth() {
            write!(
                &mut curr_display,
                "{}",
                self.display(&DisplayState::new(recursion_depth))
            )
            .expect("Failed to display entity");
            if curr_display.width() > max_cols.into() {
                break;
            } else {
                std::mem::swap(&mut prev_display, &mut curr_display);
                curr_display.clear()
            }
        }
        prev_display
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
impl<Inner: CustomDisplay + ?Sized> Display for CustomDisplayView<'_, Inner> {
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
#[derive(Clone, Debug, Eq, PartialEq)]
struct DisplayStateInner {
    /// Recursion limit from current point
    max_recursion: usize,
}
//
impl Default for DisplayStateInner {
    fn default() -> Self {
        Self {
            max_recursion: usize::MAX,
        }
    }
}
//
impl DisplayState {
    /// Set up a display state with certain recursion limits
    pub fn new(max_recursion: usize) -> Self {
        Self(RefCell::new(DisplayStateInner { max_recursion }))
    }

    /// Test if a certain recursion is possible under the current limit
    pub fn can_recurse(&self) -> bool {
        self.max_recursion() > 0
    }

    /// Enter a new level of recursion or return Err if recursion limit reached
    pub fn recurse(&self) -> Result<RecursionGuard, RecursionLimitReached> {
        if !self.can_recurse() {
            return Err(RecursionLimitReached);
        }
        {
            let mut state = self.0.borrow_mut();
            state.max_recursion -= 1;
        }
        Ok(RecursionGuard { state: self })
    }

    /// Query current reachable recursion depth
    fn max_recursion(&self) -> usize {
        self.0.borrow().max_recursion
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
//
/// `DisplayState::recurse` failed because the recursion limit was reached
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RecursionLimitReached;

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// Check that a CustomDisplay implementation works as intended
    ///
    /// - Advertised recursion depth is correct
    /// - Display is correct at all recursion depths
    ///
    pub fn check_custom_display(view: impl CustomDisplay, displays: &[&str]) {
        assert!(displays.len() > 0, "There is always recursion depth 0...");

        assert_eq!(view.recursion_depth(), displays.len() - 1);

        let actual_display =
            |max_recursion| view.display(&DisplayState::new(max_recursion)).to_string();
        for (max_recursion, expected_display) in displays.iter().enumerate() {
            assert_eq!(*expected_display, actual_display(max_recursion));
        }

        let full_display = *displays
            .last()
            .expect("Per initial assert this cannot happen");
        assert_eq!(actual_display(view.recursion_depth() + 1), full_display);
        assert_eq!(actual_display(usize::MAX), full_display);
    }

    /// Simple mock CustomDisplay implementation for tests that need one
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct CustomDisplayMock(pub usize);
    //
    impl CustomDisplay for CustomDisplayMock {
        fn recursion_depth(&self) -> usize {
            self.0
        }

        fn display_impl(
            &self,
            f: &mut Formatter<'_>,
            state: &DisplayState,
        ) -> Result<(), fmt::Error> {
            if self.0 >= 1 {
                if let Ok(_guard) = state.recurse() {
                    write!(f, "(")?;
                    Self(self.0 - 1).display_impl(f, state)?;
                    write!(f, ")")?;
                } else {
                    write!(f, "…")?;
                }
            } else {
                write!(f, "@")?;
            }
            Ok(())
        }
    }

    // Check that CustomDisplayMock's basic functionality works as intended
    #[test]
    fn custom_display_mock() {
        check_custom_display(CustomDisplayMock(0), &["@"]);
        check_custom_display(CustomDisplayMock(1), &["…", "(@)"]);
        check_custom_display(CustomDisplayMock(2), &["…", "(…)", "((@))"]);
        check_custom_display(CustomDisplayMock(3), &["…", "(…)", "((…))", "(((@)))"]);
    }

    // Check that bounded_display works as intended
    #[test]
    fn bounded_display() {
        assert_eq!(CustomDisplayMock(0).bounded_display(1), "@");
        assert_eq!(CustomDisplayMock(0).bounded_display(2), "@");

        assert_eq!(CustomDisplayMock(1).bounded_display(1), "…");
        assert_eq!(CustomDisplayMock(1).bounded_display(2), "…");
        assert_eq!(CustomDisplayMock(1).bounded_display(3), "(@)");
        assert_eq!(CustomDisplayMock(1).bounded_display(4), "(@)");

        assert_eq!(CustomDisplayMock(2).bounded_display(1), "…");
        assert_eq!(CustomDisplayMock(2).bounded_display(2), "…");
        assert_eq!(CustomDisplayMock(2).bounded_display(3), "(…)");
        assert_eq!(CustomDisplayMock(2).bounded_display(4), "(…)");
        assert_eq!(CustomDisplayMock(2).bounded_display(5), "((@))");
        assert_eq!(CustomDisplayMock(2).bounded_display(6), "((@))");
    }

    // Test option display
    #[test]
    fn name() {
        let state = DisplayState::default();

        let none: Option<CustomDisplayMock> = None;
        assert_eq!(none.recursion_depth(), 0);
        assert_eq!(none.display(&state).to_string(), "");

        let mock = CustomDisplayMock(2);
        let some = Some(mock);
        assert_eq!(some.recursion_depth(), mock.recursion_depth());
        assert_eq!(
            some.display(&state).to_string(),
            mock.display(&state).to_string()
        );
    }

    // Check that DisplayState works as intended
    fn test_display_state_impl(state: &DisplayState) {
        let init_max_recursion = state.max_recursion();
        assert_eq!(state.can_recurse(), (init_max_recursion > 0));
        if state.can_recurse() {
            if let Ok(guard) = state.recurse() {
                assert_eq!(state.max_recursion(), init_max_recursion - 1);
                test_display_state_impl(state);
                std::mem::drop(guard);
            } else {
                unreachable!();
            }
        } else {
            assert!(state.recurse().is_err());
        }
        assert_eq!(state.max_recursion(), init_max_recursion);
    }
    //
    fn test_display_state(max_recursion: usize) {
        let state = DisplayState::new(max_recursion);
        assert_eq!(state.max_recursion(), max_recursion);
        test_display_state_impl(&state);
    }
    //
    #[test]
    fn display_state() {
        test_display_state(0);
        test_display_state(1);
        test_display_state(2);
        test_display_state(42);
    }
}
