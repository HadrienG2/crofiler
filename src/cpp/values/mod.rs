//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::Literal;
use crate::cpp::{
    functions,
    names::{self, IdExpression, UnqualifiedId},
    operators::{self, Operator},
    types::{self, TypeLike},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like<const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(
    s: &str,
) -> IResult<ValueLike> {
    use nom::{character::complete::space0, multi::many0, sequence::preceded};
    value_without_trailer::<ALLOW_COMMA, ALLOW_GREATER>
        .and(
            many0(preceded(space0, after_value::<ALLOW_COMMA, ALLOW_GREATER>))
                .map(|v| v.into_boxed_slice()),
        )
        .map(|(header, trailer)| ValueLike { header, trailer })
        .parse(s)
}
//
/// A value, or something that looks close enough to it
#[derive(Clone, Debug, PartialEq)]
pub struct ValueLike<'source> {
    /// Initial value-like entity
    header: ValueWithoutTrailer<'source>,

    /// Stream of additional entities (indexing operators, function calls,
    /// other operators...) that build this into a more complex value.
    trailer: Box<[AfterValue<'source>]>,
}
//
impl<'source, T: Into<ValueWithoutTrailer<'source>>> From<T> for ValueLike<'source> {
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
fn value_without_trailer<const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(
    s: &str,
) -> IResult<ValueWithoutTrailer> {
    use nom::{
        character::complete::{char, space0},
        sequence::{delimited, separated_pair},
    };

    let literal = literals::literal.map(ValueWithoutTrailer::Literal);

    let parenthesized = delimited(
        char('(').and(space0),
        value_like::<true, true>.map(Box::new),
        space0.and(char(')')),
    )
    .map(ValueWithoutTrailer::Parenthesized);

    let unary_op = separated_pair(
        operators::unary_expr_prefix,
        space0,
        value_like::<ALLOW_COMMA, ALLOW_GREATER>.map(Box::new),
    )
    .map(|(op, expr)| ValueWithoutTrailer::UnaryOp(op, expr));

    let new_expression = new_expression.map(|e| ValueWithoutTrailer::NewExpression(Box::new(e)));

    let id_expression = names::id_expression.map(ValueWithoutTrailer::IdExpression);

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
#[derive(Clone, Debug, PartialEq)]
pub enum ValueWithoutTrailer<'source> {
    /// Literal
    Literal(Literal<'source>),

    /// Value with parentheses around it
    Parenthesized(Box<ValueLike<'source>>),

    /// Unary operator applied to a value
    UnaryOp(Operator<'source>, Box<ValueLike<'source>>),

    /// New-expression
    NewExpression(Box<NewExpression<'source>>),

    /// Named value
    IdExpression(IdExpression<'source>),
}
//
impl<'source, T: Into<Literal<'source>>> From<T> for ValueWithoutTrailer<'source> {
    fn from(literal: T) -> Self {
        Self::Literal(literal.into())
    }
}

/// Parse things that can come up after a value to form a more complex value
fn after_value<const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(s: &str) -> IResult<AfterValue> {
    use nom::{
        character::complete::{char, space0},
        sequence::{delimited, preceded, separated_pair},
    };

    let binary_op = separated_pair(
        operators::binary_expr_middle::<ALLOW_COMMA, ALLOW_GREATER>,
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

    let member_access =
        preceded(char('.').and(space0), names::unqualified_id).map(AfterValue::MemberAccess);

    let postfix_op = operators::increment_decrement.map(AfterValue::PostfixOp);

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
#[derive(Clone, Debug, PartialEq)]
pub enum AfterValue<'source> {
    /// Array indexing
    ArrayIndex(ValueLike<'source>),

    /// Function call
    FunctionCall(Box<[ValueLike<'source>]>),

    /// Binary operator (OP x)
    BinaryOp(Operator<'source>, ValueLike<'source>),

    /// Ternary operator (? x : y)
    TernaryOp(ValueLike<'source>, ValueLike<'source>),

    /// Member access (. stuff)
    MemberAccess(UnqualifiedId<'source>),

    /// Postfix operator (++ and -- only in current C++)
    PostfixOp(Operator<'source>),
}

/// Parse new expression
fn new_expression(s: &str) -> IResult<NewExpression> {
    use nom::{
        character::complete::space0,
        combinator::opt,
        sequence::{preceded, tuple},
    };
    use nom_supreme::tag::complete::tag;
    let rooted = opt(tag("::").and(space0)).map(|o| o.is_some());
    (rooted.and(preceded(
        tag("new").and(space0),
        tuple((
            opt(functions::function_call).terminated(space0),
            types::type_like.terminated(space0),
            opt(functions::function_call),
        )),
    )))
    .map(|(rooted, (placement, ty, constructor))| NewExpression {
        rooted,
        placement,
        ty,
        constructor,
    })
    .parse(s)
}
//
/// New expression, i.e. usage of the new operator
#[derive(Clone, Debug, Default, PartialEq)]
pub struct NewExpression<'source> {
    /// Whether this new expression is rooted (starts with ::), which means that
    /// class-specific replacements will be ignored
    rooted: bool,

    /// Placement parameters
    placement: Option<Box<[ValueLike<'source>]>>,

    /// Type of values being created
    ty: TypeLike<'source>,

    /// Parameters to the values' constructor (if any)
    constructor: Option<Box<[ValueLike<'source>]>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpp::tests::force_parse_type;
    use operators::Symbol;
    use pretty_assertions::assert_eq;

    #[test]
    fn new_expression() {
        // Basic form
        assert_eq!(
            super::new_expression("new int"),
            Ok((
                "",
                NewExpression {
                    ty: force_parse_type("int"),
                    ..Default::default()
                }
            ))
        );

        // Rooted form
        assert_eq!(
            super::new_expression("::new double"),
            Ok((
                "",
                NewExpression {
                    rooted: true,
                    ty: force_parse_type("double"),
                    ..Default::default()
                }
            ))
        );

        // Placement parameters
        assert_eq!(
            super::new_expression("new (42) MyClass"),
            Ok((
                "",
                NewExpression {
                    placement: Some(vec![42u8.into()].into()),
                    ty: force_parse_type("MyClass"),
                    ..Default::default()
                }
            ))
        );

        // Constructor parameters
        assert_eq!(
            super::new_expression("new MyClass('x')"),
            Ok((
                "",
                NewExpression {
                    ty: force_parse_type("MyClass"),
                    constructor: Some(vec!['x'.into()].into()),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn value_without_trailer() {
        let value_without_trailer = super::value_without_trailer::<false, false>;

        // Literal
        assert_eq!(value_without_trailer("'@'"), Ok(("", '@'.into())));

        // Unary operators are supported...
        assert_eq!(
            value_without_trailer("&123"),
            Ok((
                "",
                ValueWithoutTrailer::UnaryOp(Symbol::AndRef.into(), Box::new((123u8).into()))
            ))
        );

        // ...including c-style casts, not to be confused with parenthesized values
        assert_eq!(
            value_without_trailer("(T)666"),
            Ok((
                "",
                ValueWithoutTrailer::UnaryOp(
                    Operator::Conversion(Box::new(force_parse_type("T"))),
                    Box::new(666u16.into())
                )
            ))
        );

        // Parenthesized values are supported too
        assert_eq!(
            value_without_trailer("(42)"),
            Ok((
                "",
                ValueWithoutTrailer::Parenthesized(Box::new(42u8.into()))
            ))
        );

        // New expressions too
        assert_eq!(
            value_without_trailer("new TROOT"),
            Ok((
                "",
                ValueWithoutTrailer::NewExpression(Box::new(NewExpression {
                    ty: force_parse_type("TROOT"),
                    ..Default::default()
                }))
            ))
        );

        // Named values as well
        assert_eq!(
            value_without_trailer("MyValue"),
            Ok(("", ValueWithoutTrailer::IdExpression("MyValue".into())))
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
                    header: ValueWithoutTrailer::IdExpression("array".into()),
                    trailer: vec![AfterValue::ArrayIndex(666u16.into())].into(),
                }
            ))
        );
        assert_eq!(
            value_like("func( 3,'x' )[666]"),
            Ok((
                "",
                ValueLike {
                    header: ValueWithoutTrailer::IdExpression("func".into()),
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
