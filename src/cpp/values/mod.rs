//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::Literal;
use crate::cpp::{
    names::{self, IdExpression},
    operators::{self, Operator},
    IResult,
};
use nom::Parser;

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like(s: &str) -> IResult<ValueLike> {
    // TODO: Add function calls, indexing, binary operators, ternary operator
    value_like_no_head_recursion(s)
}
//
/// Like value_like but excluding patterns that start with a value_like
///
/// Used by value_like to prevent infinite head recursion.
fn value_like_no_head_recursion(s: &str) -> IResult<ValueLike> {
    use nom::{character::complete::char, sequence::delimited};
    let value_like = || value_like.map(Box::new);
    let literal = literals::literal.map(ValueLike::Literal);
    let parenthesized = delimited(char('('), value_like(), char(')')).map(ValueLike::Parenthesized);
    let unary_op = (operators::unary_expr_prefix.and(value_like()))
        .map(|(op, expr)| ValueLike::UnaryOp(op, expr));
    let id_expression = names::id_expression.map(ValueLike::IdExpression);
    literal
        .or(parenthesized)
        .or(unary_op)
        // TODO: Add new-expression here, see https://en.cppreference.com/w/cpp/language/new
        //
        // Must come late in the trial chain as it can match keywords, including
        // the name of some operators.
        .or(id_expression)
        .parse(s)
}
//
/// A value, or something that looks close enough to it
#[derive(Clone, Debug, PartialEq)]
pub enum ValueLike<'source> {
    /// Literal
    Literal(Literal<'source>),

    /// Value with parentheses around it
    Parenthesized(Box<ValueLike<'source>>),

    /// Unary operator applied to a value
    UnaryOp(Operator<'source>, Box<ValueLike<'source>>),

    /// Named value
    IdExpression(IdExpression<'source>),
}
//
impl<'source, T: Into<Literal<'source>>> From<T> for ValueLike<'source> {
    fn from(literal: T) -> Self {
        Self::Literal(literal.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use operators::Symbol;
    use pretty_assertions::assert_eq;

    #[test]
    fn value_like() {
        assert_eq!(super::value_like("'@'"), Ok(("", '@'.into())));
        assert_eq!(
            super::value_like("(42)"),
            Ok(("", ValueLike::Parenthesized(Box::new(42u8.into()))))
        );
        assert_eq!(
            super::value_like("&123"),
            Ok((
                "",
                ValueLike::UnaryOp(Symbol::AndRef.into(), Box::new((123u8).into()))
            ))
        );
        assert_eq!(
            super::value_like("MyValue"),
            Ok(("", ValueLike::IdExpression("MyValue".into())))
        );
    }
}
