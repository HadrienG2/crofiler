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
    EntityParser, IResult,
};
use nom::Parser;
use std::fmt::Debug;

impl EntityParser {
    /// Parser recognizing values (and some types that are indistinguishable
    /// from values without extra source code context)
    ///
    /// The allow_comma and allow_greater parameters enable preventing parsing
    /// of operators based on the comma , and greater > sign in template and
    /// function parameter parsing scenarios, where the simpler interpretation
    /// of these symbols as parameter separators and template parameter set
    /// terminator should take priority.
    ///
    pub fn parse_value_like<'source>(
        &self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, ValueLike<'source, atoms::IdentifierKey, crate::PathKey>> {
        value_like(
            s,
            &|s| self.parse_identifier(s),
            &|path| self.path_to_key(path),
            allow_comma,
            allow_greater,
        )
    }
}

/// Parser recognizing values (and some types that are indistinguishable from
/// values without extra source code context)
///
/// See EntityParser::parse_value_like for extra docs
///
// TODO: Make private once users are migrated
pub fn value_like<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
    allow_comma: bool,
    allow_greater: bool,
) -> IResult<'source, ValueLike<'source, IdentifierKey, PathKey>> {
    use nom::{character::complete::space0, multi::many0, sequence::preceded};
    (|s| value_header(s, parse_identifier, path_to_key, allow_comma, allow_greater))
        .and(
            many0(preceded(space0, |s| {
                after_value(s, parse_identifier, path_to_key, allow_comma, allow_greater)
            }))
            .map(|v| v.into_boxed_slice()),
        )
        .map(|(header, trailer)| ValueLike { header, trailer })
        .parse(s)
}
//
/// A value, or something that looks close enough to it
// FIXME: This type appears in Box<T> and Box<[T]>, intern those once data is owned
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ValueLike<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Initial value-like entity
    header: ValueHeader<'source, IdentifierKey, PathKey>,

    /// Stream of additional entities (indexing operators, function calls,
    /// other operators...) that build this into a more complex value.
    trailer: Box<[AfterValue<'source, IdentifierKey, PathKey>]>,
}
//
impl<
        'source,
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
        T: Into<ValueHeader<'source, IdentifierKey, PathKey>>,
    > From<T> for ValueLike<'source, IdentifierKey, PathKey>
{
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
fn value_header<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
    allow_comma: bool,
    allow_greater: bool,
) -> IResult<'source, ValueHeader<'source, IdentifierKey, PathKey>> {
    use nom::{
        character::complete::{char, space0},
        sequence::{delimited, separated_pair},
    };

    let literal = (|s| literals::literal(s, parse_identifier)).map(ValueHeader::Literal);

    let parenthesized_value_like = |s| value_like(s, parse_identifier, path_to_key, true, true);
    let parenthesized = delimited(
        char('(').and(space0),
        parenthesized_value_like.map(Box::new),
        space0.and(char(')')),
    )
    .map(ValueHeader::Parenthesized);

    let curr_value_like =
        |s| value_like(s, parse_identifier, path_to_key, allow_comma, allow_greater);
    let unary_op = separated_pair(
        |s| operators::usage::unary_expr_prefix(s, parse_identifier, path_to_key),
        space0,
        curr_value_like.map(Box::new),
    )
    .map(|(op, expr)| ValueHeader::UnaryOp(op, expr));

    let new_expression = (|s| operators::usage::new_expression(s, parse_identifier, path_to_key))
        .map(|e| ValueHeader::NewExpression(Box::new(e)));

    let id_expression = (|s| scopes::id_expression(s, parse_identifier, path_to_key))
        .map(ValueHeader::IdExpression);

    literal
        .or(new_expression)
        // Must come after new_expression as it matches the new keyword
        .or(id_expression)
        .or(unary_op)
        // Must come after unary_op to match casts as intended
        .or(parenthesized)
        .parse(s)
}
//
/// Values that are not expressions starting with a value
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueHeader<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Literal
    Literal(Literal<IdentifierKey>),

    /// Value with parentheses around it
    Parenthesized(Box<ValueLike<'source, IdentifierKey, PathKey>>),

    /// Unary operator applied to a value
    UnaryOp(
        Operator<'source, IdentifierKey, PathKey>,
        Box<ValueLike<'source, IdentifierKey, PathKey>>,
    ),

    /// New-expression
    NewExpression(Box<NewExpression<'source, IdentifierKey, PathKey>>),

    /// Named value
    IdExpression(IdExpression<'source, IdentifierKey, PathKey>),
}
//
impl<
        'source,
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
        T: Into<Literal<IdentifierKey>>,
    > From<T> for ValueHeader<'source, IdentifierKey, PathKey>
{
    fn from(literal: T) -> Self {
        Self::Literal(literal.into())
    }
}

/// Parse things that can come up after a value to form a more complex value
fn after_value<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
    allow_comma: bool,
    allow_greater: bool,
) -> IResult<'source, AfterValue<'source, IdentifierKey, PathKey>> {
    use nom::{
        character::complete::{char, space0},
        sequence::{delimited, preceded, separated_pair},
    };

    let curr_value_like =
        |s| value_like(s, parse_identifier, path_to_key, allow_comma, allow_greater);

    let binary_op = separated_pair(
        |s| operators::usage::binary_expr_middle(s, allow_comma, allow_greater),
        space0,
        &curr_value_like,
    )
    .map(|(op, value)| AfterValue::BinaryOp(op, value));

    let mut ternary_op = preceded(
        char('?').and(space0),
        separated_pair(
            &curr_value_like,
            space0.and(char(':')).and(space0),
            &curr_value_like,
        ),
    )
    .map(|(value1, value2)| AfterValue::TernaryOp(value1, value2));

    let value_like_index = |s| value_like(s, parse_identifier, path_to_key, false, true);
    let mut array_index = delimited(
        char('[').and(space0),
        value_like_index,
        space0.and(char(']')),
    )
    .map(AfterValue::ArrayIndex);

    let mut function_call = (|s| functions::function_call(s, parse_identifier, path_to_key))
        .map(AfterValue::FunctionCall);

    let mut member_access = preceded(char('.').and(space0), |s| {
        unqualified::unqualified_id(s, parse_identifier, path_to_key)
    })
    .map(AfterValue::MemberAccess);

    let postfix_op = operators::usage::increment_decrement.map(AfterValue::PostfixOp);

    // Since this parser is quite hot (~1M calls on a test workload) and usually
    // fails, we reduce the cost of failure by dispatching to appropriate
    // sub-parsers after checking the first char of input.
    // Branches other than _ are ordered by decreasing occurence frequency.
    match s.as_bytes().first() {
        Some(b'(') => function_call.parse(s),
        Some(b'?') => ternary_op.parse(s),
        Some(b'.') => member_access.parse(s),
        Some(b'[') => array_index.parse(s),
        _ => binary_op.or(postfix_op).parse(s),
    }
}
//
/// Things that can come up after a value to form a more complex value
// FIXME: This type appears in Box<[T]>, intern that once data is owned
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AfterValue<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Array indexing
    ArrayIndex(ValueLike<'source, IdentifierKey, PathKey>),

    /// Function call
    FunctionCall(Box<[ValueLike<'source, IdentifierKey, PathKey>]>),

    /// Binary operator (OP x)
    BinaryOp(
        Operator<'source, IdentifierKey, PathKey>,
        ValueLike<'source, IdentifierKey, PathKey>,
    ),

    /// Ternary operator (? x : y)
    TernaryOp(
        ValueLike<'source, IdentifierKey, PathKey>,
        ValueLike<'source, IdentifierKey, PathKey>,
    ),

    /// Member access (. stuff)
    MemberAccess(UnqualifiedId<'source, IdentifierKey, PathKey>),

    /// Postfix operator (++ and -- only in current C++)
    PostfixOp(Operator<'source, IdentifierKey, PathKey>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{tests::force_parse, types};
    use operators::Symbol;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn value_header() {
        let parse_value_header =
            |s| super::value_header(s, &atoms::identifier, &Path::new, true, true);

        // Literal
        assert_eq!(parse_value_header("'@'"), Ok(("", '@'.into())));

        // Unary operators are supported...
        assert_eq!(
            parse_value_header("&123"),
            Ok((
                "",
                ValueHeader::UnaryOp(Symbol::AndRef.into(), Box::new((123u8).into()))
            ))
        );

        // ...including c-style casts, not to be confused with parenthesized values
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_value_header("(T)666"),
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
            parse_value_header("(42)"),
            Ok(("", ValueHeader::Parenthesized(Box::new(42u8.into()))))
        );

        // New expressions too
        let parse_new_expression =
            |s| operators::usage::new_expression(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_value_header("new TROOT"),
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
            parse_value_header("MyValue"),
            Ok(("", ValueHeader::IdExpression("MyValue".into())))
        );
    }

    #[test]
    fn after_value() {
        let parse_after_value =
            |s| super::after_value(s, &atoms::identifier, &Path::new, true, true);
        assert_eq!(
            parse_after_value("[666]"),
            Ok(("", AfterValue::ArrayIndex(666u16.into()),))
        );
        assert_eq!(
            parse_after_value("('c', -5)"),
            Ok((
                "",
                AfterValue::FunctionCall(vec!['c'.into(), (-5i8).into()].into()),
            ))
        );
        assert_eq!(
            parse_after_value("+42"),
            Ok((
                "",
                AfterValue::BinaryOp(Symbol::AddPlus.into(), (42u8).into())
            ))
        );
        assert_eq!(
            parse_after_value("? 123 : 456"),
            Ok(("", AfterValue::TernaryOp(123u8.into(), 456u16.into())))
        );
        assert_eq!(
            parse_after_value(".lol"),
            Ok(("", AfterValue::MemberAccess("lol".into())))
        );
        assert_eq!(
            parse_after_value("++"),
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
        let parse_value_like = |s| super::value_like(s, &atoms::identifier, &Path::new, true, true);
        assert_eq!(
            parse_value_like("array[666]"),
            Ok((
                "",
                ValueLike {
                    header: ValueHeader::IdExpression("array".into()),
                    trailer: vec![AfterValue::ArrayIndex(666u16.into())].into(),
                }
            ))
        );
        assert_eq!(
            parse_value_like("func( 3,'x' )[666]"),
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
