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
    use nom::{character::complete::char, sequence::delimited};
    let value_like = || value_like.map(Box::new);
    let literal = literals::literal.map(ValueLike::Literal);
    let parentheses = delimited(char('('), value_like(), char(')')).map(ValueLike::Parentheses);
    let unary_op = (operators::unary_expr_prefix.and(value_like()))
        .map(|(op, expr)| ValueLike::UnaryOp(op, expr));
    let id_expression = names::id_expression.map(ValueLike::IdExpression);
    literal
        .or(parentheses)
        .or(unary_op)
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
    Parentheses(Box<ValueLike<'source>>),

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
            Ok(("", ValueLike::Parentheses(Box::new(42u8.into()))))
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
