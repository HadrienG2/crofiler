//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::Literal;
use crate::{
    functions::FunctionCallKey,
    names::{scopes::IdExpression, unqualified::UnqualifiedId},
    operators::{usage::NewExpression, Operator},
    Entities, EntityParser, IResult,
};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;

/// Interned C++ value key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// values as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a value by passing this key to the
/// value_like() method of the Entities struct.
///
// TODO: Adjust key size based on observed entry count
pub type ValueKey = Spur;
//
/// Interned value trailer key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// ValueTrailers as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a value trailer by passing this key to the
/// value_trailer() method of the Entities struct.
///
// TODO: Adjust key size based on observed entry count
pub type ValueTrailerKey = SequenceKey<ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS>;
pub(crate) type ValueTrailerKeyImpl = Spur;
pub(crate) const VALUE_TRAILER_LEN_BITS: u32 = 8;
//
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
    ) -> IResult<'source, ValueKey> {
        use nom::{character::complete::space0, multi::fold_many0, sequence::preceded};

        let value_trailer = fold_many0(
            preceded(space0, |s| {
                self.parse_after_value(s, allow_comma, allow_greater)
            }),
            || self.value_trailers.entry(),
            |mut entry, item| {
                entry.push(item);
                entry
            },
        )
        .map(|entry| entry.intern());

        (|s| self.parse_value_header(s, allow_comma, allow_greater))
            .and(value_trailer)
            .map(|(header, trailer)| {
                self.values
                    .borrow_mut()
                    .intern(ValueLike { header, trailer })
            })
            .parse(s)
    }

    /// Retrieve a value previously parsed by parse_value_like
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    pub(crate) fn value_like(&self, key: ValueKey) -> ValueLike {
        self.values.borrow().get(key).clone()
    }

    /// Tell how many unique types have been parsed so far
    pub fn num_values(&self) -> usize {
        self.values.borrow().len()
    }

    /// Retrieve a value trailer previously parsed by parse_value_like
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    pub(crate) fn value_trailer(&self, key: ValueTrailerKey) -> Box<ValueTrailer> {
        self.value_trailers.borrow().get(key).into()
    }

    /// Total number of AfterValues across all interned ValueTrailers
    pub fn num_after_value(&self) -> usize {
        self.value_trailers.borrow().num_items()
    }

    /// Maximal number of template parameters
    pub fn max_value_trailer_len(&self) -> Option<usize> {
        self.value_trailers.borrow().max_sequence_len()
    }

    /// Like value_like but excluding patterns that start with a value_like
    ///
    /// Used by value_like to prevent infinite recursion on the expression head.
    ///
    /// See parse_value_like for an explanation on the boolean parameters
    ///
    fn parse_value_header<'source>(
        &self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, ValueHeader> {
        use nom::{
            character::complete::{char, space0},
            sequence::{delimited, separated_pair},
        };

        let literal = (|s| self.parse_literal(s)).map(ValueHeader::Literal);

        let parenthesized_value_like = |s| self.parse_value_like(s, true, true);
        let parenthesized = delimited(
            char('(').and(space0),
            parenthesized_value_like,
            space0.and(char(')')),
        )
        .map(ValueHeader::Parenthesized);

        let curr_value_like = |s| self.parse_value_like(s, allow_comma, allow_greater);
        let unary_op = separated_pair(|s| self.parse_unary_expr_prefix(s), space0, curr_value_like)
            .map(|(op, expr)| ValueHeader::UnaryOp(op, expr));

        let new_expression = (|s| self.parse_new_expression(s)).map(ValueHeader::NewExpression);

        let id_expression = (|s| self.parse_id_expression(s)).map(ValueHeader::IdExpression);

        literal
            .or(new_expression)
            // Must come after new_expression as it matches the new keyword
            .or(id_expression)
            .or(unary_op)
            // Must come after unary_op to match casts as intended
            .or(parenthesized)
            .parse(s)
    }

    /// Parse things that can come up after a value to form a more complex value
    ///
    /// See parse_value_like for an explanation on the boolean parameters
    ///
    fn parse_after_value<'source>(
        &self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, AfterValue> {
        use nom::{
            character::complete::{char, space0},
            sequence::{delimited, preceded, separated_pair},
        };

        let curr_value_like = |s| self.parse_value_like(s, allow_comma, allow_greater);

        let binary_op = separated_pair(
            |s| Self::parse_binary_expr_middle(s, allow_comma, allow_greater),
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

        let value_like_index = |s| self.parse_value_like(s, false, true);
        let mut array_index = delimited(
            char('[').and(space0),
            value_like_index,
            space0.and(char(']')),
        )
        .map(AfterValue::ArrayIndex);

        let mut function_call = (|s| self.parse_function_call(s)).map(AfterValue::FunctionCall);

        let mut member_access = preceded(char('.').and(space0), |s| self.parse_unqualified_id(s))
            .map(AfterValue::MemberAccess);

        let postfix_op = Self::parse_increment_decrement.map(AfterValue::PostfixOp);

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
}
//
impl Entities {
    /// Retrieve a value previously parsed by parse_value_like
    pub fn value_like(&self, key: ValueKey) -> &ValueLike {
        self.values.get(key)
    }

    /// Retrieve a value trailer previously parsed by parse_value_like
    pub fn value_trailer(&self, key: ValueTrailerKey) -> &ValueTrailer {
        self.value_trailers.get(key)
    }
}

/// A value, or something that looks close enough to it
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ValueLike {
    /// Initial value-like entity
    header: ValueHeader,

    /// Stream of additional entities (indexing operators, function calls,
    /// other operators...) that build this into a more complex value.
    trailer: ValueTrailerKey,
}

/// Values that are not expressions starting with a value
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ValueHeader {
    /// Literal
    Literal(Literal),

    /// Value with parentheses around it
    Parenthesized(ValueKey),

    /// Unary operator applied to a value
    UnaryOp(Operator, ValueKey),

    /// New-expression
    NewExpression(NewExpression),

    /// Named value
    IdExpression(IdExpression),
}
//
impl From<Literal> for ValueHeader {
    fn from(l: Literal) -> Self {
        Self::Literal(l)
    }
}
//
impl From<ValueKey> for ValueHeader {
    fn from(v: ValueKey) -> Self {
        Self::Parenthesized(v)
    }
}
//
impl From<NewExpression> for ValueHeader {
    fn from(n: NewExpression) -> Self {
        Self::NewExpression(n)
    }
}
//
impl From<IdExpression> for ValueHeader {
    fn from(i: IdExpression) -> Self {
        Self::IdExpression(i)
    }
}

/// Things that can come after a ValueHeader in the value grammar
pub type ValueTrailer = [AfterValue];

/// Things that can come up after a value to form a more complex value
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AfterValue {
    /// Array indexing
    ArrayIndex(ValueKey),

    /// Function call
    FunctionCall(FunctionCallKey),

    /// Binary operator (OP x)
    BinaryOp(Operator, ValueKey),

    /// Ternary operator (? x : y)
    TernaryOp(ValueKey, ValueKey),

    /// Member access (. stuff)
    MemberAccess(UnqualifiedId),

    /// Postfix operator (++ and -- only in current C++)
    PostfixOp(Operator),
}
//
impl From<FunctionCallKey> for AfterValue {
    fn from(f: FunctionCallKey) -> Self {
        AfterValue::FunctionCall(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::operators::Symbol;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn value_header() {
        let parser = EntityParser::new();
        let parse_value_header = |s| parser.parse_value_header(s, true, true);
        let literal = |s| unwrap_parse(parser.parse_literal(s));

        // Literal
        assert_eq!(parse_value_header("'@'"), Ok(("", literal("'@'").into())));

        // Unary operators are supported...
        assert_eq!(
            parse_value_header("&123"),
            Ok((
                "",
                ValueHeader::UnaryOp(Symbol::AndRef.into(), Box::new(literal("123").into()))
            ))
        );

        // ...including c-style casts, not to be confused with parenthesized values
        assert_eq!(
            parse_value_header("(T)666"),
            Ok((
                "",
                ValueHeader::UnaryOp(
                    unwrap_parse(parser.parse_type_like("T")).into(),
                    Box::new(literal("666").into())
                )
            ))
        );

        // Parenthesized values are supported too
        assert_eq!(
            parse_value_header("(42)"),
            Ok((
                "",
                ValueHeader::Parenthesized(Box::new(literal("42").into()))
            ))
        );

        // New expressions too
        assert_eq!(
            parse_value_header("new TROOT"),
            Ok((
                "",
                ValueHeader::NewExpression(Box::new(unwrap_parse(
                    parser.parse_new_expression("new TROOT")
                ))),
            ))
        );

        // Named values as well
        assert_eq!(
            parse_value_header("MyValue"),
            Ok((
                "",
                ValueHeader::IdExpression(unwrap_parse(parser.parse_id_expression("MyValue")))
            ))
        );
    }

    #[test]
    fn after_value() {
        let parser = EntityParser::new();
        let parse_after_value = |s| parser.parse_after_value(s, true, true);
        let literal = |s| unwrap_parse(parser.parse_literal(s));

        // Array indexing
        assert_eq!(
            parse_after_value("[666]"),
            Ok(("", AfterValue::ArrayIndex(literal("666").into()),))
        );

        // Function call
        assert_eq!(
            parse_after_value("('c', -5)"),
            Ok((
                "",
                AfterValue::FunctionCall(vec![literal("'c'").into(), literal("-5").into()].into()),
            ))
        );

        // Binary operator
        assert_eq!(
            parse_after_value("+42"),
            Ok((
                "",
                AfterValue::BinaryOp(Symbol::AddPlus.into(), literal("42").into())
            ))
        );

        // Ternary operator
        assert_eq!(
            parse_after_value("? 123 : 456"),
            Ok((
                "",
                AfterValue::TernaryOp(literal("123").into(), literal("456").into())
            ))
        );

        // Member access
        assert_eq!(
            parse_after_value(".lol"),
            Ok((
                "",
                AfterValue::MemberAccess(unwrap_parse(parser.parse_unqualified_id("lol")))
            ))
        );

        // Postfix operator
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
        let parser = EntityParser::new();
        let parse_value_like = |s| parser.parse_value_like(s, true, true);
        let id_expression = |s| unwrap_parse(parser.parse_id_expression(s));
        let literal = |s| unwrap_parse(parser.parse_literal(s));

        assert_eq!(
            parse_value_like("array[666]"),
            Ok((
                "",
                ValueLike {
                    header: ValueHeader::IdExpression(id_expression("array")),
                    trailer: vec![AfterValue::ArrayIndex(literal("666").into())].into(),
                }
            ))
        );
        assert_eq!(
            parse_value_like("func( 3,'x' )[666]"),
            Ok((
                "",
                ValueLike {
                    header: ValueHeader::IdExpression(id_expression("func")),
                    trailer: vec![
                        AfterValue::FunctionCall(
                            vec![literal("3").into(), literal("'x'").into()].into()
                        ),
                        AfterValue::ArrayIndex(literal("666").into())
                    ]
                    .into(),
                }
            ))
        );
    }
}
