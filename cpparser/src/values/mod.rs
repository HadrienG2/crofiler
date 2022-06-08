//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::Literal;
use crate::{
    functions,
    names::{
        atoms,
        scopes::{self, IdExpression},
        unqualified::{self, UnqualifiedId},
    },
    operators::{self, usage::NewExpression, Operator},
    IResult,
};
use nom::Parser;
use std::path::Path;

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like<const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(
    s: &str,
) -> IResult<ValueLike> {
    use nom::{character::complete::space0, multi::many0, sequence::preceded};
    value_header::<ALLOW_COMMA, ALLOW_GREATER>
        .and(
            many0(preceded(space0, after_value::<ALLOW_COMMA, ALLOW_GREATER>))
                .map(|v| v.into_boxed_slice()),
        )
        .map(|(header, trailer)| ValueLike { header, trailer })
        .parse(s)
}
//
/// A value, or something that looks close enough to it
// FIXME: This type appears in Box<T> and Box<[T]>, intern those once data is owned
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ValueLike<'source> {
    /// Initial value-like entity
    header: ValueHeader<'source>,

    /// Stream of additional entities (indexing operators, function calls,
    /// other operators...) that build this into a more complex value.
    trailer: Box<[AfterValue<'source>]>,
}
//
impl<'source, T: Into<ValueHeader<'source>>> From<T> for ValueLike<'source> {
    fn from(literal: T) -> Self {
        Self {
            header: literal.into(),
            trailer: Box::default(),
        }
    }
}

/// Like value_like but excluding patterns that start with a value_like
///
/// Used by value_like to prevent infinite recursion on the expression head.
///
// FIXME: Optimize, possibly via single-char dispatch as in UnqualifiedId
fn value_header<const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(
    s: &str,
) -> IResult<ValueHeader> {
    use nom::{
        character::complete::{char, space0},
        sequence::{delimited, separated_pair},
    };

    let literal = (|s| literals::literal(s, &atoms::identifier)).map(ValueHeader::Literal);

    let parenthesized = delimited(
        char('(').and(space0),
        value_like::<true, true>.map(Box::new),
        space0.and(char(')')),
    )
    .map(ValueHeader::Parenthesized);

    let unary_op = separated_pair(
        |s| operators::usage::unary_expr_prefix(s, &atoms::identifier, &Path::new),
        space0,
        value_like::<ALLOW_COMMA, ALLOW_GREATER>.map(Box::new),
    )
    .map(|(op, expr)| ValueHeader::UnaryOp(op, expr));

    let new_expression = (|s| operators::usage::new_expression(s, &atoms::identifier, &Path::new))
        .map(|e| ValueHeader::NewExpression(Box::new(e)));

    let id_expression = (|s| scopes::id_expression(s, &atoms::identifier, &Path::new))
        .map(ValueHeader::IdExpression);

    literal
        .or(unary_op)
        // Must come after unary_op to avoid mismatching the cast operator as a
        // parenthesized expression
        .or(parenthesized)
        .or(new_expression)
        // Must come late in the trial chain as it can match keywords, including
        // the name of some operators.
        .or(id_expression)
        .parse(s)
}
//
/// Values that are not expressions starting with a value
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueHeader<'source> {
    /// Literal
    Literal(Literal<&'source str>),

    /// Value with parentheses around it
    Parenthesized(Box<ValueLike<'source>>),

    /// Unary operator applied to a value
    UnaryOp(
        Operator<'source, &'source str, &'source Path>,
        Box<ValueLike<'source>>,
    ),

    /// New-expression
    NewExpression(Box<NewExpression<'source, &'source str, &'source Path>>),

    /// Named value
    IdExpression(IdExpression<'source, &'source str, &'source Path>),
}
//
impl<'source, T: Into<Literal<&'source str>>> From<T> for ValueHeader<'source> {
    fn from(literal: T) -> Self {
        Self::Literal(literal.into())
    }
}

/// Parse things that can come up after a value to form a more complex value
// FIXME: Optimize, possibly via single-char dispatch as in UnqualifiedId
fn after_value<'source, const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(
    s: &'source str,
) -> IResult<AfterValue> {
    use nom::{
        character::complete::{char, space0},
        sequence::{delimited, preceded, separated_pair},
    };

    let binary_op = separated_pair(
        operators::usage::binary_expr_middle::<
            ALLOW_COMMA,
            ALLOW_GREATER,
            &'source str,
            &'source Path,
        >,
        space0,
        value_like::<ALLOW_COMMA, ALLOW_GREATER>,
    )
    .map(|(op, value)| AfterValue::BinaryOp(op, value));

    let ternary_op = preceded(
        char('?').and(space0),
        separated_pair(
            value_like::<ALLOW_COMMA, ALLOW_GREATER>,
            space0.and(char(':')).and(space0),
            value_like::<ALLOW_COMMA, ALLOW_GREATER>,
        ),
    )
    .map(|(value1, value2)| AfterValue::TernaryOp(value1, value2));

    let array_index = delimited(
        char('[').and(space0),
        value_like::<false, true>,
        space0.and(char(']')),
    )
    .map(AfterValue::ArrayIndex);

    let function_call = functions::function_call.map(AfterValue::FunctionCall);

    let member_access = preceded(char('.').and(space0), |s| {
        unqualified::unqualified_id(s, &atoms::identifier, &Path::new)
    })
    .map(AfterValue::MemberAccess);

    let postfix_op = operators::usage::increment_decrement.map(AfterValue::PostfixOp);

    binary_op
        .or(ternary_op)
        .or(array_index)
        .or(function_call)
        .or(member_access)
        .or(postfix_op)
        .parse(s)
}
//
/// Things that can come up after a value to form a more complex value
// FIXME: This type appears in Box<[T]>, intern that once data is owned
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AfterValue<'source> {
    /// Array indexing
    ArrayIndex(ValueLike<'source>),

    /// Function call
    FunctionCall(Box<[ValueLike<'source>]>),

    /// Binary operator (OP x)
    BinaryOp(
        Operator<'source, &'source str, &'source Path>,
        ValueLike<'source>,
    ),

    /// Ternary operator (? x : y)
    TernaryOp(ValueLike<'source>, ValueLike<'source>),

    /// Member access (. stuff)
    MemberAccess(UnqualifiedId<'source, &'source str, &'source Path>),

    /// Postfix operator (++ and -- only in current C++)
    PostfixOp(Operator<'source, &'source str, &'source Path>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{tests::force_parse, types};
    use operators::Symbol;
    use pretty_assertions::assert_eq;

    #[test]
    fn value_header() {
        let value_header = super::value_header::<false, false>;

        // Literal
        assert_eq!(value_header("'@'"), Ok(("", '@'.into())));

        // Unary operators are supported...
        assert_eq!(
            value_header("&123"),
            Ok((
                "",
                ValueHeader::UnaryOp(Symbol::AndRef.into(), Box::new((123u8).into()))
            ))
        );

        // ...including c-style casts, not to be confused with parenthesized values
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        assert_eq!(
            value_header("(T)666"),
            Ok((
                "",
                ValueHeader::UnaryOp(
                    Operator::Conversion(Box::new(force_parse(parse_type_like, "T"))),
                    Box::new(666u16.into())
                )
            ))
        );

        // Parenthesized values are supported too
        assert_eq!(
            value_header("(42)"),
            Ok(("", ValueHeader::Parenthesized(Box::new(42u8.into()))))
        );

        // New expressions too
        let parse_new_expression =
            |s| operators::usage::new_expression(s, &atoms::identifier, &Path::new);
        assert_eq!(
            value_header("new TROOT"),
            Ok((
                "",
                ValueHeader::NewExpression(Box::new(force_parse(
                    parse_new_expression,
                    "new TROOT"
                ))),
            ))
        );

        // Named values as well
        assert_eq!(
            value_header("MyValue"),
            Ok(("", ValueHeader::IdExpression("MyValue".into())))
        );
    }

    #[test]
    fn after_value() {
        let after_value = super::after_value::<false, false>;
        assert_eq!(
            after_value("[666]"),
            Ok(("", AfterValue::ArrayIndex(666u16.into()),))
        );
        assert_eq!(
            after_value("('c', -5)"),
            Ok((
                "",
                AfterValue::FunctionCall(vec!['c'.into(), (-5i8).into()].into()),
            ))
        );
        assert_eq!(
            after_value("+42"),
            Ok((
                "",
                AfterValue::BinaryOp(Symbol::AddPlus.into(), (42u8).into())
            ))
        );
        assert_eq!(
            after_value("? 123 : 456"),
            Ok(("", AfterValue::TernaryOp(123u8.into(), 456u16.into())))
        );
        assert_eq!(
            after_value(".lol"),
            Ok(("", AfterValue::MemberAccess("lol".into())))
        );
        assert_eq!(
            after_value("++"),
            Ok((
                "",
                AfterValue::PostfixOp(Operator::Basic {
                    symbol: Symbol::AddPlus,
                    twice: true,
                    equal: false,
                })
            ))
        );
    }

    #[test]
    fn value_like() {
        let value_like = super::value_like::<false, false>;
        assert_eq!(
            value_like("array[666]"),
            Ok((
                "",
                ValueLike {
                    header: ValueHeader::IdExpression("array".into()),
                    trailer: vec![AfterValue::ArrayIndex(666u16.into())].into(),
                }
            ))
        );
        assert_eq!(
            value_like("func( 3,'x' )[666]"),
            Ok((
                "",
                ValueLike {
                    header: ValueHeader::IdExpression("func".into()),
                    trailer: vec![
                        AfterValue::FunctionCall(vec![(3u8).into(), 'x'.into()].into()),
                        AfterValue::ArrayIndex(666u16.into())
                    ]
                    .into(),
                }
            ))
        );
    }
}
