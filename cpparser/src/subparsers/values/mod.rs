//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::{Literal, LiteralView};
use crate::{
    interning::slice::{SliceItemView, SliceView},
    subparsers::{
        functions::{FunctionArgumentsKey, FunctionArgumentsView},
        names::{
            scopes::{IdExpression, IdExpressionView},
            unqualified::{UnqualifiedId, UnqualifiedIdView},
        },
        operators::{
            self,
            usage::{NewExpression, NewExpressionView},
            Operator, OperatorView,
        },
    },
    Entities, EntityParser, IResult,
};
use asylum::{lasso::MiniSpur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

/// Interned C++ value key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// values as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a value by passing this key to the
/// value_like() method of the Entities struct.
///
pub type ValueKey = MiniSpur;
//
/// Interned value trailer key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// ValueTrailers as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a value trailer by passing this key to the
/// value_trailer() method of the Entities struct.
///
pub type ValueTrailerKey = SequenceKey<ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS>;
pub(crate) type ValueTrailerKeyImpl = MiniSpur;
pub(crate) const VALUE_TRAILER_LEN_BITS: u32 = 6;
//
impl EntityParser {
    /// Parser recognizing values (and some types that are indistinguishable
    /// from values without extra source code context)
    ///
    /// The allow_comma and allow_greater parameters enable preventing parsing
    /// of operators based on the comma , and greater `>` sign in template and
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
    #[cfg(test)]
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
    #[cfg(test)]
    pub(crate) fn value_trailer(&self, key: ValueTrailerKey) -> Box<[AfterValue]> {
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
        use nom_supreme::tag::complete::tag;

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

        let ellipsis = tag("...").value(ValueHeader::Ellipsis);

        literal
            .or(new_expression)
            // Must come after new_expression as it matches the new keyword
            .or(id_expression)
            .or(unary_op)
            // Must come after unary_op to match casts as intended
            .or(parenthesized)
            .or(ellipsis)
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
        use nom_supreme::tag::complete::tag;

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

        let member_access = preceded(char('.').and(space0), |s| self.parse_unqualified_id(s))
            .map(AfterValue::MemberAccess);

        let postfix_op = Self::parse_increment_decrement.map(AfterValue::PostfixOp);

        let ellipsis = tag("...").value(AfterValue::Ellipsis);

        // Since this parser is quite hot (~1M calls on a test workload) and usually
        // fails, we reduce the cost of failure by dispatching to appropriate
        // sub-parsers after checking the first char of input.
        // Branches other than _ are ordered by decreasing occurence frequency.
        match s.as_bytes().first() {
            Some(b'(') => function_call.parse(s),
            Some(b'?') => ternary_op.parse(s),
            Some(b'.') => member_access.or(ellipsis).parse(s),
            Some(b'[') => array_index.parse(s),
            _ => binary_op.or(postfix_op).parse(s),
        }
    }
}
//
impl Entities {
    /// Access a previously parsed value
    pub fn value_like(&self, v: ValueKey) -> ValueView {
        ValueView::new(v, self)
    }

    /// Access a previously parsed value header
    pub(crate) fn value_header(&self, vh: ValueHeader) -> ValueHeaderView {
        ValueHeaderView::new(vh, self)
    }

    /// Access a previously parsed value trailer
    pub(crate) fn value_trailer(&self, vt: ValueTrailerKey) -> ValueTrailerView {
        ValueTrailerView::new(vt, &self.value_trailers, self)
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

/// View of a value
pub struct ValueView<'entities> {
    /// Key used to retrieve the value
    key: ValueKey,

    /// Wrapped ValueLike
    inner: &'entities ValueLike,

    /// Underlying interned entity storage
    entities: &'entities Entities,
}
//
impl<'entities> ValueView<'entities> {
    /// Build a value view
    pub(crate) fn new(key: ValueKey, entities: &'entities Entities) -> Self {
        Self {
            key,
            inner: entities.values.get(key),
            entities,
        }
    }

    /// Initial value-like entity
    pub fn header(&self) -> ValueHeaderView {
        self.entities.value_header(self.inner.header)
    }

    /// Sequence of additional entities (indexing operators, function calls,
    /// other operators...) that build this into a more complex value.
    pub fn trailer(&self) -> ValueTrailerView {
        self.entities.value_trailer(self.inner.trailer)
    }
}
//
impl<'entities> PartialEq for ValueView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const Entities == other.entities as *const Entities)
            && (self.key == other.key)
    }
}
//
impl<'entities> Display for ValueView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.header(), self.trailer())
    }
}
//
impl<'entities> SliceItemView<'entities> for ValueView<'entities> {
    type Inner = ValueKey;

    fn new(inner: Self::Inner, entities: &'entities Entities) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "(";

    const DISPLAY_SEPARATOR: &'static str = ", ";

    const DISPLAY_TRAILER: &'static str = ")";
}

/// Values that are not expressions starting with a value
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum ValueHeader {
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

    /// Ellipsis sign ... (used in fold expressions)
    Ellipsis,
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

/// View of a value that is not an expression starting with a value
#[derive(PartialEq)]
pub enum ValueHeaderView<'entities> {
    /// Literal
    Literal(LiteralView<'entities>),

    /// Value with parentheses around it
    Parenthesized(ValueView<'entities>),

    /// Unary operator applied to a value
    UnaryOp(OperatorView<'entities>, ValueView<'entities>),

    /// New-expression
    NewExpression(NewExpressionView<'entities>),

    /// Named value
    IdExpression(IdExpressionView<'entities>),

    /// Ellipsis sign ... (used in fold expressions)
    Ellipsis,
}
//
impl<'entities> ValueHeaderView<'entities> {
    /// Build an operator view
    pub(crate) fn new(header: ValueHeader, entities: &'entities Entities) -> Self {
        match header {
            ValueHeader::Literal(l) => Self::Literal(entities.literal(l)),
            ValueHeader::Parenthesized(v) => Self::Parenthesized(entities.value_like(v)),
            ValueHeader::UnaryOp(o, v) => {
                Self::UnaryOp(entities.operator(o), entities.value_like(v))
            }
            ValueHeader::NewExpression(ne) => Self::NewExpression(entities.new_expression(ne)),
            ValueHeader::IdExpression(id) => Self::IdExpression(entities.id_expression(id)),
            ValueHeader::Ellipsis => Self::Ellipsis,
        }
    }
}
//
impl<'entities> Display for ValueHeaderView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Parenthesized(v) => write!(f, "({v})"),
            Self::UnaryOp(o, v) => {
                o.display(f, operators::DisplayContext::PrefixUsage)?;
                write!(f, "{v}")
            }
            Self::NewExpression(n) => write!(f, "{n}"),
            Self::IdExpression(i) => write!(f, "{i}"),
            Self::Ellipsis => write!(f, "..."),
        }
    }
}

/// View of a value's trailer
pub type ValueTrailerView<'entities> = SliceView<
    'entities,
    AfterValue,
    AfterValueView<'entities>,
    ValueTrailerKeyImpl,
    VALUE_TRAILER_LEN_BITS,
>;

/// Things that can come up after a value to form a more complex value
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AfterValue {
    /// Array indexing
    ArrayIndex(ValueKey),

    /// Function call
    FunctionCall(FunctionArgumentsKey),

    /// Binary operator (OP x)
    BinaryOp(Operator, ValueKey),

    /// Ternary operator (? x : y)
    TernaryOp(ValueKey, ValueKey),

    /// Member access (. stuff)
    MemberAccess(UnqualifiedId),

    /// Postfix operator (++ and -- only in current C++)
    PostfixOp(Operator),

    /// Ellipsis sign ... (used in template parameter pack expansion)
    Ellipsis,
}
//
impl From<FunctionArgumentsKey> for AfterValue {
    fn from(f: FunctionArgumentsKey) -> Self {
        AfterValue::FunctionCall(f)
    }
}

/// View of something that comes after a value to form a more complex value
#[derive(PartialEq)]
pub enum AfterValueView<'entities> {
    /// Array indexing
    ArrayIndex(ValueView<'entities>),

    /// Function call
    FunctionCall(FunctionArgumentsView<'entities>),

    /// Binary operator (OP x)
    BinaryOp(OperatorView<'entities>, ValueView<'entities>),

    /// Ternary operator (? x : y)
    TernaryOp(ValueView<'entities>, ValueView<'entities>),

    /// Member access (. stuff)
    MemberAccess(UnqualifiedIdView<'entities>),

    /// Postfix operator (++ and -- only in current C++)
    PostfixOp(OperatorView<'entities>),

    /// Ellipsis sign ... (used in template parameter pack expansion)
    Ellipsis,
}
//
impl<'entities> AfterValueView<'entities> {
    /// Build an operator view
    pub(crate) fn new(av: AfterValue, entities: &'entities Entities) -> Self {
        match av {
            AfterValue::ArrayIndex(v) => Self::ArrayIndex(entities.value_like(v)),
            AfterValue::FunctionCall(a) => Self::FunctionCall(entities.function_arguments(a)),
            AfterValue::BinaryOp(o, v) => {
                Self::BinaryOp(entities.operator(o), entities.value_like(v))
            }
            AfterValue::TernaryOp(v1, v2) => {
                Self::TernaryOp(entities.value_like(v1), entities.value_like(v2))
            }
            AfterValue::MemberAccess(m) => Self::MemberAccess(entities.unqualified_id(m)),
            AfterValue::PostfixOp(o) => Self::PostfixOp(entities.operator(o)),
            AfterValue::Ellipsis => Self::Ellipsis,
        }
    }
}
//
impl<'entities> Display for AfterValueView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::ArrayIndex(i) => write!(f, "[{i}]"),
            Self::FunctionCall(a) => write!(f, "{a}"),
            Self::BinaryOp(o, v) => {
                o.display(f, operators::DisplayContext::BinaryUsage)?;
                write!(f, "{v}")
            }
            Self::TernaryOp(v1, v2) => write!(f, " ? {v1} : {v2}"),
            Self::MemberAccess(m) => write!(f, ".{m}"),
            Self::PostfixOp(o) => o.display(f, operators::DisplayContext::PostfixUsage),
            Self::Ellipsis => write!(f, "..."),
        }
    }
}
//
impl<'entities> SliceItemView<'entities> for AfterValueView<'entities> {
    type Inner = AfterValue;

    fn new(inner: Self::Inner, entities: &'entities Entities) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "";

    const DISPLAY_SEPARATOR: &'static str = "";

    const DISPLAY_TRAILER: &'static str = "";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{subparsers::operators::Symbol, tests::unwrap_parse};
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn value_header() {
        let parser = EntityParser::new();
        let parse_value_header = |s| parser.parse_value_header(s, true, true);
        let literal = |s| unwrap_parse(parser.parse_literal(s));
        let literal_value = |s| {
            let key = unwrap_parse(parser.parse_value_like(s, true, true));
            assert_eq!(
                parser.value_like(key),
                ValueLike {
                    header: literal(s).into(),
                    trailer: parser.value_trailers.entry().intern()
                }
            );
            key
        };

        // Literal
        assert_eq!(parse_value_header("69"), Ok(("", literal("69").into())));
        assert_eq!(parse_value_header("'@'"), Ok(("", literal("'@'").into())));

        // Unary operators are supported...
        assert_eq!(
            parse_value_header("&123"),
            Ok((
                "",
                ValueHeader::UnaryOp(Symbol::AndRef.into(), literal_value("123"))
            ))
        );

        // ...including c-style casts, not to be confused with parenthesized values
        assert_eq!(
            parse_value_header("(T)666"),
            Ok((
                "",
                ValueHeader::UnaryOp(
                    unwrap_parse(parser.parse_type_like("T")).into(),
                    literal_value("666")
                )
            ))
        );

        // Parenthesized values are supported too
        assert_eq!(
            parse_value_header("(42)"),
            Ok(("", ValueHeader::Parenthesized(literal_value("42"))))
        );

        // New expressions too
        assert_eq!(
            parse_value_header("new TROOT"),
            Ok((
                "",
                ValueHeader::NewExpression(unwrap_parse(parser.parse_new_expression("new TROOT"))),
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

        // Ellipsis (as in fold expressions)
        assert_eq!(parse_value_header("..."), Ok(("", ValueHeader::Ellipsis)));
    }

    #[test]
    fn after_value() {
        let parser = EntityParser::new();
        let parse_after_value = |s| parser.parse_after_value(s, true, true);
        let literal = |s| unwrap_parse(parser.parse_literal(s));
        let literal_value = |s| {
            let key = unwrap_parse(parser.parse_value_like(s, true, true));
            assert_eq!(
                parser.value_like(key),
                ValueLike {
                    header: literal(s).into(),
                    trailer: parser.value_trailers.entry().intern()
                }
            );
            key
        };

        // Array indexing
        assert_eq!(
            parse_after_value("[666]"),
            Ok(("", AfterValue::ArrayIndex(literal_value("666"))))
        );

        // Function call
        assert_eq!(
            parse_after_value("('c', -5)"),
            Ok((
                "",
                AfterValue::FunctionCall(unwrap_parse(parser.parse_function_call("('c', -5)"))),
            ))
        );

        // Binary operator
        assert_eq!(
            parse_after_value("+42"),
            Ok((
                "",
                AfterValue::BinaryOp(Symbol::AddPlus.into(), literal_value("42"))
            ))
        );

        // Ternary operator
        assert_eq!(
            parse_after_value("? 123 : 456"),
            Ok((
                "",
                AfterValue::TernaryOp(literal_value("123"), literal_value("456"))
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

        // Trailing ellipsis
        assert_eq!(parse_after_value("..."), Ok(("", AfterValue::Ellipsis)));
    }

    #[test]
    fn value_like() {
        let parser = EntityParser::new();
        let parse_value_like = |s| parser.parse_value_like(s, true, true);
        let id_expression = |s| unwrap_parse(parser.parse_id_expression(s));
        let literal = |s| unwrap_parse(parser.parse_literal(s));
        let literal_value = |s| {
            let key = unwrap_parse(parser.parse_value_like(s, true, true));
            assert_eq!(
                parser.value_like(key),
                ValueLike {
                    header: literal(s).into(),
                    trailer: parser.value_trailers.entry().intern()
                }
            );
            key
        };

        assert_matches!(
            parse_value_like("array[666]"),
            Ok((
                "",
                value_key
            )) => {
                let value = parser.value_like(value_key);
                assert_eq!(value.header, ValueHeader::IdExpression(id_expression("array")));
                assert_eq!(parser.value_trailer(value.trailer), vec![AfterValue::ArrayIndex(literal_value("666"))].into());
            }
        );
        assert_matches!(
            parse_value_like("func( 3,'x' )[666]"),
            Ok((
                "",
                value_key
            )) => {
                let value = parser.value_like(value_key);
                assert_eq!(value.header, ValueHeader::IdExpression(id_expression("func")));
                assert_eq!(parser.value_trailer(value.trailer), vec![
                        unwrap_parse(parser.parse_function_call("( 3,'x' )")).into(),
                        AfterValue::ArrayIndex(literal_value("666"))
                    ]
                    .into());
            }
        );
    }
}
