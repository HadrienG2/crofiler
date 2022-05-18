//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::{Literal, LiteralValue};
use crate::cpp::{
    operators::{self, Operator},
    IResult,
};
use nom::Parser;

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like(s: &str) -> IResult<ValueLike> {
    let literal = literals::literal.map(ValueLike::Literal);
    let unary_op = operators::unary_expr_prefix
        .and(value_like.map(Box::new))
        .map(|(op, expr)| ValueLike::UnaryOp(op, expr));
    // TODO: Should accept id-expressions here, but not necessary yet since they
    //       are accepted by TypeLike as well.
    literal.or(unary_op).parse(s)
}
//
/// A value, or something that looks close enough to it
#[derive(Clone, Debug, PartialEq)]
pub enum ValueLike<'source> {
    /// Literal
    Literal(Literal<'source>),

    /// Unary operator AST
    UnaryOp(Operator<'source>, Box<ValueLike<'source>>),
}
//
impl<T: Into<LiteralValue>> From<T> for ValueLike<'_> {
    fn from(value: T) -> Self {
        Self::Literal(Literal::from(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use operators::Symbol;
    use pretty_assertions::assert_eq;

    #[test]
    fn value_like() {
        assert_eq!(
            super::value_like("&123"),
            Ok((
                "",
                ValueLike::UnaryOp(Symbol::AndRef.into(), Box::new((123i8).into()))
            ))
        );
    }
}
