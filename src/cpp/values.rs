//! Values and other things that follow the value grammar

use crate::cpp::{atoms, IResult};

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like(s: &str) -> IResult<ValueLike> {
    atoms::integer_literal(s)
}
//
/// A value, or something that looks close enough to it
pub type ValueLike = i128;

// FIXME: Add tests
