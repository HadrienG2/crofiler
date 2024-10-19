//! Values and other things that follow the value grammar

pub mod literals;

use self::literals::{Literal, LiteralView};
use crate::{
    display::{CustomDisplay, DisplayState},
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
        types::{TypeKey, TypeView},
    },
    EntityParser, IResult,
};
use asylum::{lasso::MiniSpur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use reffers::ARef;
use std::fmt::{self, Display, Formatter};

/// Interned C++ value key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// values as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a value by passing this key to the
/// value_like() method of EntityParser.
///
pub type ValueKey = MiniSpur;
//
/// Interned value trailer key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// ValueTrailers as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a value trailer by passing this key to the
/// value_trailer() method of EntityParser.
///
pub type ValueTrailerKey = SequenceKey<ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS>;
type ValueTrailerKeyImpl = MiniSpur;
const VALUE_TRAILER_LEN_BITS: u32 = 6;
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
        &mut self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, ValueKey> {
        self.parse_value_like_imut(s, allow_comma, allow_greater)
    }

    /// Implementation of parse_value_like using internal mutability
    pub(crate) fn parse_value_like_imut<'source>(
        &self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, ValueKey> {
        use nom::{character::complete::multispace0, multi::fold_many0, sequence::preceded};

        let value_trailer = fold_many0(
            preceded(multispace0, |s| {
                self.parse_after_value_imut(s, allow_comma, allow_greater)
            }),
            || self.value_trailers.entry(),
            |mut entry, item| {
                entry.push(item);
                entry
            },
        )
        .map(|entry| entry.intern());

        (|s| self.parse_value_header_imut(s, allow_comma, allow_greater))
            .and(value_trailer)
            .map(|(header, trailer)| {
                self.values
                    .borrow_mut()
                    .intern(ValueLike { header, trailer })
            })
            .parse(s)
    }

    /// Access a previously parsed value
    pub fn value_like(&self, v: ValueKey) -> ValueView {
        ValueView::new(v, self)
    }

    /// Retrieve a value previously parsed by parse_value_like
    pub(crate) fn raw_value_like(&self, key: ValueKey) -> ARef<ValueLike> {
        ARef::new(self.values.borrow()).map(|values| values.get(key))
    }

    /// Tell how many unique types have been parsed so far
    pub fn num_values(&self) -> usize {
        self.values.borrow().len()
    }

    /// Like value_like but excluding patterns that start with a value_like
    ///
    /// Used by value_like to prevent infinite recursion on the expression head.
    ///
    /// See parse_value_like for an explanation on the boolean parameters
    ///
    fn parse_value_header_imut<'source>(
        &self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, ValueHeader> {
        use nom::{
            character::complete::{char, multispace0},
            sequence::{delimited, separated_pair},
        };
        use nom_supreme::tag::complete::tag;

        let literal = (|s| self.parse_literal_imut(s)).map(ValueHeader::Literal);

        let parenthesized_value_like = |s| self.parse_value_like_imut(s, true, true);
        let parenthesized = delimited(
            char('(').and(multispace0),
            parenthesized_value_like,
            multispace0.and(char(')')),
        )
        .map(ValueHeader::Parenthesized);

        let curr_value_like = |s| self.parse_value_like_imut(s, allow_comma, allow_greater);
        let unary_op = separated_pair(
            |s| self.parse_unary_expr_prefix_imut(s),
            multispace0,
            curr_value_like,
        )
        .map(|(op, expr)| ValueHeader::UnaryOp(op, expr));

        let new_expression =
            (|s| self.parse_new_expression_imut(s)).map(ValueHeader::NewExpression);

        let id_expression = (|s| self.parse_id_expression_imut(s)).map(ValueHeader::IdExpression);

        let ellipsis = tag("...").value(ValueHeader::Ellipsis);

        let sizeof = delimited(tag("sizeof("), |s| self.parse_type_like_imut(s), char(')'))
            .map(ValueHeader::SizeOf);
        let declval = delimited(tag("declval("), |s| self.parse_type_like_imut(s), char(')'))
            .map(ValueHeader::DeclVal);

        literal
            .or(new_expression)
            .or(sizeof)
            .or(declval)
            // Must come after sizeof and declaval as it matches the keywords
            .or(id_expression)
            .or(unary_op)
            // Must come after unary_op to match casts as intended
            .or(parenthesized)
            .or(ellipsis)
            .parse(s)
    }

    /// Access a previously parsed value header
    pub(crate) fn value_header(&self, vh: ValueHeader) -> ValueHeaderView {
        ValueHeaderView::new(vh, self)
    }

    /// Parse things that can come up after a value to form a more complex value
    ///
    /// See parse_value_like for an explanation on the boolean parameters
    ///
    fn parse_after_value_imut<'source>(
        &self,
        s: &'source str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<'source, AfterValue> {
        use nom::{
            character::complete::{char, multispace0},
            sequence::{delimited, preceded, separated_pair},
        };
        use nom_supreme::tag::complete::tag;

        let curr_value_like = |s| self.parse_value_like_imut(s, allow_comma, allow_greater);

        let binary_op = separated_pair(
            |s| Self::parse_binary_expr_middle(s, allow_comma, allow_greater),
            multispace0,
            &curr_value_like,
        )
        .map(|(op, value)| AfterValue::BinaryOp(op, value));

        let mut ternary_op = preceded(
            char('?').and(multispace0),
            separated_pair(
                &curr_value_like,
                multispace0.and(char(':')).and(multispace0),
                &curr_value_like,
            ),
        )
        .map(|(value1, value2)| AfterValue::TernaryOp(value1, value2));

        let value_like_index = |s| self.parse_value_like_imut(s, false, true);
        let mut array_index = delimited(
            char('[').and(multispace0),
            value_like_index,
            multispace0.and(char(']')),
        )
        .map(AfterValue::ArrayIndex);

        let mut function_call =
            (|s| self.parse_function_call_imut(s)).map(AfterValue::FunctionCall);

        let member_access = preceded(char('.').and(multispace0), |s| {
            self.parse_unqualified_id_imut(s)
        })
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

    // Access a previously parsed AfterValue
    #[cfg(test)]
    fn after_value(&self, av: AfterValue) -> AfterValueView {
        AfterValueView::new(av, self)
    }

    /// Access a previously parsed value trailer
    pub(crate) fn value_trailer(&self, vt: ValueTrailerKey) -> ValueTrailerView {
        ValueTrailerView::new(vt, self.value_trailers.borrow(), self)
    }

    /// Retrieve a value trailer previously parsed by parse_value_like
    #[cfg(test)]
    pub(crate) fn raw_value_trailer(&self, key: ValueTrailerKey) -> ARef<[AfterValue]> {
        self.value_trailers.get(key)
    }

    /// Total number of AfterValues across all interned ValueTrailers
    pub fn num_after_value(&self) -> usize {
        self.value_trailers.borrow().num_items()
    }

    /// Maximal number of template parameters
    pub fn max_value_trailer_len(&self) -> Option<usize> {
        self.value_trailers.borrow().max_sequence_len()
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
    inner: ARef<'entities, ValueLike>,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> ValueView<'entities> {
    /// Build a value view
    pub(crate) fn new(key: ValueKey, entities: &'entities EntityParser) -> Self {
        Self {
            key,
            inner: entities.raw_value_like(key),
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
impl PartialEq for ValueView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.key == other.key)
    }
}
//
impl Display for ValueView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for ValueView<'_> {
    fn recursion_depth(&self) -> usize {
        self.header()
            .recursion_depth()
            .max(self.trailer().recursion_depth())
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        self.header().display_impl(f, state)?;
        self.trailer().display_impl(f, state)
    }
}
//
impl<'entities> SliceItemView<'entities> for ValueView<'entities> {
    type Inner = ValueKey;

    fn new(inner: Self::Inner, entities: &'entities EntityParser) -> Self {
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

    /// sizeof() operator
    SizeOf(TypeKey),

    /// declval() operator
    DeclVal(TypeKey),
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

    /// sizeof() operator
    SizeOf(TypeView<'entities>),

    /// declval() operator
    DeclVal(TypeView<'entities>),
}
//
impl<'entities> ValueHeaderView<'entities> {
    /// Build an operator view
    pub(crate) fn new(header: ValueHeader, entities: &'entities EntityParser) -> Self {
        match header {
            ValueHeader::Literal(l) => Self::Literal(entities.literal(l)),
            ValueHeader::Parenthesized(v) => Self::Parenthesized(entities.value_like(v)),
            ValueHeader::UnaryOp(o, v) => {
                Self::UnaryOp(entities.operator(o), entities.value_like(v))
            }
            ValueHeader::NewExpression(ne) => Self::NewExpression(entities.new_expression(ne)),
            ValueHeader::IdExpression(id) => Self::IdExpression(entities.id_expression(id)),
            ValueHeader::Ellipsis => Self::Ellipsis,
            ValueHeader::SizeOf(t) => Self::SizeOf(entities.type_like(t)),
            ValueHeader::DeclVal(t) => Self::DeclVal(entities.type_like(t)),
        }
    }
}
//
impl Display for ValueHeaderView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for ValueHeaderView<'_> {
    fn recursion_depth(&self) -> usize {
        match self {
            Self::Literal(_) => 0,
            Self::Parenthesized(v) => v.recursion_depth(),
            Self::UnaryOp(o, v) => o.recursion_depth().max(v.recursion_depth()),
            Self::NewExpression(n) => n.recursion_depth(),
            Self::IdExpression(i) => i.recursion_depth(),
            Self::Ellipsis => 0,
            Self::SizeOf(t) => t.recursion_depth(),
            Self::DeclVal(t) => t.recursion_depth(),
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            // FIXME: Add a recursion bound for parentheses
            Self::Parenthesized(v) => {
                write!(f, "(")?;
                v.display_impl(f, state)?;
                write!(f, ")")
            }
            Self::UnaryOp(o, v) => {
                o.display(f, state, operators::DisplayContext::PrefixUsage)?;
                v.display_impl(f, state)
            }
            Self::NewExpression(n) => n.display_impl(f, state),
            Self::IdExpression(i) => i.display_impl(f, state),
            Self::Ellipsis => write!(f, "..."),
            Self::SizeOf(t) | Self::DeclVal(t) => {
                let keyword = if let Self::SizeOf(_) = self {
                    "sizeof"
                } else {
                    "declval"
                };
                write!(f, "{keyword}(")?;
                t.display_impl(f, state)?;
                write!(f, ")")
            }
        }
    }
}

/// View of a value's trailer
pub type ValueTrailerView<'entities> =
    SliceView<'entities, AfterValue, AfterValueView<'entities>, ValueTrailerKey>;

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
    pub(crate) fn new(av: AfterValue, entities: &'entities EntityParser) -> Self {
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
impl Display for AfterValueView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for AfterValueView<'_> {
    fn recursion_depth(&self) -> usize {
        match self {
            Self::ArrayIndex(i) => i.recursion_depth(),
            Self::FunctionCall(a) => a.recursion_depth(),
            Self::BinaryOp(o, v) => o.recursion_depth().max(v.recursion_depth()),
            Self::TernaryOp(v1, v2) => v1.recursion_depth().max(v2.recursion_depth()),
            Self::MemberAccess(m) => m.recursion_depth(),
            Self::PostfixOp(o) => o.recursion_depth(),
            Self::Ellipsis => 0,
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            // FIXME: Add a recursion bound on array indexing
            Self::ArrayIndex(i) => {
                write!(f, "[")?;
                i.display_impl(f, state)?;
                write!(f, "]")
            }
            Self::FunctionCall(a) => a.display_impl(f, state),
            Self::BinaryOp(o, v) => {
                o.display(f, state, operators::DisplayContext::BinaryUsage)?;
                v.display_impl(f, state)
            }
            Self::TernaryOp(v1, v2) => {
                write!(f, " ? ")?;
                v1.display_impl(f, state)?;
                write!(f, " : ")?;
                v2.display_impl(f, state)
            }
            Self::MemberAccess(m) => {
                write!(f, ".")?;
                m.display_impl(f, state)
            }
            Self::PostfixOp(o) => o.display(f, state, operators::DisplayContext::PostfixUsage),
            Self::Ellipsis => write!(f, "..."),
        }
    }
}
//
impl<'entities> SliceItemView<'entities> for AfterValueView<'entities> {
    type Inner = AfterValue;

    fn new(inner: Self::Inner, entities: &'entities EntityParser) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "";

    const DISPLAY_SEPARATOR: &'static str = "";

    const DISPLAY_TRAILER: &'static str = "";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        display::tests::check_custom_display, subparsers::operators::Symbol, tests::unwrap_parse,
    };
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn value_header() {
        let mut parser = EntityParser::new();
        let parse_value_header =
            |parser: &mut EntityParser, s| parser.parse_value_header_imut(s, true, true);
        let literal = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_literal(s));
        let literal_value = |parser: &mut EntityParser, s| {
            let key = unwrap_parse(parser.parse_value_like(s, true, true));
            let value = *parser.raw_value_like(key);
            assert_eq!(
                value,
                ValueLike {
                    header: literal(parser, s).into(),
                    trailer: parser.value_trailers.entry().intern()
                }
            );
            key
        };
        let check_value_header = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parse_value_header(parser, input), Ok(("", expected)));
            check_custom_display(parser.value_header(expected), displays);
        };

        // Literal
        let mut expected = literal(&mut parser, "69").into();
        check_value_header(&mut parser, "69", expected, &["69"]);
        //
        expected = literal(&mut parser, "'@'").into();
        check_value_header(&mut parser, "'@'", expected, &["'@'"]);

        // Unary operators are supported...
        expected = ValueHeader::UnaryOp(Symbol::AndRef.into(), literal_value(&mut parser, "123"));
        check_value_header(&mut parser, "&123", expected, &["&123"]);

        // ...including c-style casts, not to be confused with parenthesized values
        expected = ValueHeader::UnaryOp(
            unwrap_parse(parser.parse_type_like("T")).into(),
            literal_value(&mut parser, "666"),
        );
        check_value_header(&mut parser, "(T)666", expected, &["(T)666"]);

        // Parenthesized values are supported too
        expected = ValueHeader::Parenthesized(literal_value(&mut parser, "42"));
        check_value_header(&mut parser, "(42)", expected, &["(42)"]);

        // New expressions too
        expected =
            ValueHeader::NewExpression(unwrap_parse(parser.parse_new_expression("new TROOT")));
        check_value_header(&mut parser, "new TROOT", expected, &["new TROOT"]);

        // Named values as well
        expected = ValueHeader::IdExpression(unwrap_parse(parser.parse_id_expression("MyValue")));
        check_value_header(&mut parser, "MyValue", expected, &["MyValue"]);

        // Ellipsis (as in fold expressions)
        check_value_header(&mut parser, "...", ValueHeader::Ellipsis, &["..."]);

        // sizeof operator
        expected = ValueHeader::SizeOf(unwrap_parse(parser.parse_type_like("U")));
        check_value_header(&mut parser, "sizeof(U)", expected, &["sizeof(U)"]);

        // declval operator
        expected = ValueHeader::DeclVal(unwrap_parse(parser.parse_type_like("Lol")));
        check_value_header(&mut parser, "declval(Lol)", expected, &["declval(Lol)"]);
    }

    #[test]
    fn after_value() {
        let mut parser = EntityParser::new();
        let parse_after_value =
            |parser: &mut EntityParser, s| parser.parse_after_value_imut(s, true, true);
        let literal = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_literal(s));
        let literal_value = |parser: &mut EntityParser, s| {
            let key = unwrap_parse(parser.parse_value_like(s, true, true));
            let value = *parser.raw_value_like(key);
            assert_eq!(
                value,
                ValueLike {
                    header: literal(parser, s).into(),
                    trailer: parser.value_trailers.entry().intern()
                }
            );
            key
        };
        let check_after_value = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parse_after_value(parser, input), Ok(("", expected)));
            check_custom_display(parser.after_value(expected), displays);
        };

        // Array indexing
        let mut expected = AfterValue::ArrayIndex(literal_value(&mut parser, "666"));
        check_after_value(&mut parser, "[666]", expected, &["[666]"]);

        // Function call
        expected = AfterValue::FunctionCall(unwrap_parse(parser.parse_function_call("('c', -5)")));
        check_after_value(&mut parser, "('c', -5)", expected, &["(…)", "('c', -5)"]);

        // Binary operator
        expected = AfterValue::BinaryOp(Symbol::AddPlus.into(), literal_value(&mut parser, "42"));
        check_after_value(&mut parser, "+42", expected, &[" + 42"]);

        // Ternary operator
        expected = AfterValue::TernaryOp(
            literal_value(&mut parser, "123"),
            literal_value(&mut parser, "456"),
        );
        check_after_value(&mut parser, "? 123 : 456", expected, &[" ? 123 : 456"]);

        // Member access
        expected = AfterValue::MemberAccess(unwrap_parse(parser.parse_unqualified_id("lol")));
        check_after_value(&mut parser, ".lol", expected, &[".lol"]);

        // Postfix operator
        check_after_value(
            &mut parser,
            "++",
            AfterValue::PostfixOp(Operator::Basic {
                symbol: Symbol::AddPlus,
                twice: true,
                equal: false,
            }),
            &["++"],
        );

        // Trailing ellipsis
        check_after_value(&mut parser, "...", AfterValue::Ellipsis, &["..."]);
    }

    #[test]
    fn value_like() {
        let mut parser = EntityParser::new();
        let parse_value_like =
            |parser: &mut EntityParser, s| parser.parse_value_like(s, true, true);
        let id_expression =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_id_expression(s));
        let literal = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_literal(s));
        let literal_value = |parser: &mut EntityParser, s| {
            let key = unwrap_parse(parser.parse_value_like(s, true, true));
            let value = *parser.raw_value_like(key);
            assert_eq!(
                value,
                ValueLike {
                    header: literal(parser, s).into(),
                    trailer: parser.value_trailers.entry().intern()
                }
            );
            key
        };

        assert_matches!(
            parse_value_like(&mut parser, "array[666]"),
            Ok((
                "",
                value_key
            )) => {
                let value = *parser.raw_value_like(value_key);
                assert_eq!(value.header, ValueHeader::IdExpression(id_expression(&mut parser, "array")));
                let expected = [AfterValue::ArrayIndex(literal_value(&mut parser, "666"))];
                assert_eq!(&parser.raw_value_trailer(value.trailer)[..], &expected[..]);
                check_custom_display(parser.value_like(value_key), &["array…", "array[666]"]);
            }
        );
        assert_matches!(
            parse_value_like(&mut parser, "func( 3,'x' )[666]"),
            Ok((
                "",
                value_key
            )) => {
                let value = *parser.raw_value_like(value_key);
                assert_eq!(value.header, ValueHeader::IdExpression(id_expression(&mut parser, "func")));
                let expected = [unwrap_parse(parser.parse_function_call("( 3,'x' )")).into(),
                        AfterValue::ArrayIndex(literal_value(&mut parser, "666"))];
                assert_eq!(&parser.raw_value_trailer(value.trailer)[..], &expected[..]);
                check_custom_display(parser.value_like(value_key), &["func…", "func(…)[666]", "func(3, 'x')[666]"]);
            }
        );
    }
}
