//! All about declarators
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

use super::qualifiers::{ConstVolatile, Reference};
use crate::{
    interning::slice::{SliceItemView, SliceView},
    subparsers::{
        functions::{FunctionSignature, FunctionSignatureView},
        names::scopes::{NestedNameSpecifier, NestedNameSpecifierView},
        values::{ValueKey, ValueView},
    },
    Entities, EntityParser, IResult,
};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

/// Interned declarator key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// declarators as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a declarator by passing this key to the
/// declarator() method of the Entities struct.
///
pub type DeclaratorKey = SequenceKey<DeclaratorKeyImpl, DECLARATOR_LEN_BITS>;
pub(crate) type DeclaratorKeyImpl = Spur;
pub(crate) const DECLARATOR_LEN_BITS: u32 = 8;
//
impl EntityParser {
    /// Parser for declarators
    pub fn parse_declarator<'source>(&self, s: &'source str) -> IResult<'source, DeclaratorKey> {
        use nom::{character::complete::space0, multi::fold_many0};
        fold_many0(
            (|s| self.parse_decl_operator(s)).terminated(space0),
            || self.declarators.entry(),
            |mut acc, item| {
                acc.push(item);
                acc
            },
        )
        .map(|entry| entry.intern())
        .parse(s)
    }

    /// Retrieve a previously interned declarator
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    #[cfg(test)]
    pub(crate) fn declarator(&self, key: DeclaratorKey) -> Box<[DeclOperator]> {
        self.declarators.borrow().get(key).into()
    }

    /// Total number of DeclOperators across all interned declarators so far
    pub fn num_decl_operators(&self) -> usize {
        self.declarators.borrow().num_items()
    }

    /// Maximal number of template parameters
    pub fn max_declarator_len(&self) -> Option<usize> {
        self.declarators.borrow().max_sequence_len()
    }

    /// Parser for a declarator component
    #[inline]
    fn parse_decl_operator<'source>(&self, s: &'source str) -> IResult<'source, DeclOperator> {
        use nom::{
            character::complete::{char, space0},
            combinator::opt,
            sequence::{delimited, preceded, separated_pair},
        };
        use nom_supreme::tag::complete::tag;

        // CV qualifier
        let cv = Self::parse_cv
            .verify(|&cv| cv != ConstVolatile::default())
            .map(DeclOperator::ConstVolatile);

        // Reference declarator (/!\ Only works because we know >1 ref-sign is coming below)
        let mut reference = Self::parse_reference.map(DeclOperator::Reference);

        // Basic pointer declarator
        let mut basic_pointer =
            preceded(char('*').and(space0), Self::parse_cv).map(|cv| DeclOperator::Pointer {
                path: self.scope_sequences.entry().intern().into(),
                cv,
            });

        // The member pointer declarator is very exotic (2/1M parses) and harder to
        // parse so we don't unify it with basic_pointer.
        let nested_star = (|s| self.parse_nested_name_specifier(s)).terminated(char('*'));
        let mut member_pointer = separated_pair(nested_star, space0, Self::parse_cv)
            .map(|(path, cv)| DeclOperator::Pointer { path, cv });

        // Array declarator
        let array = delimited(
            char('[').and(space0),
            opt(|s| self.parse_value_like(s, false, true)),
            space0.and(char(']')),
        )
        .map(DeclOperator::Array);

        // Function declarator
        let function = (|s| self.parse_function_signature(s)).map(DeclOperator::Function);

        // Parenthesized declarator (to override operator priorities)
        let parenthesized = delimited(
            char('(').and(space0),
            (|s| self.parse_declarator(s)).verify(|d| d != &self.declarators.entry().intern()),
            space0.and(char(')')),
        )
        .map(DeclOperator::Parenthesized);

        // Vector size
        let vector_size = delimited(
            tag("__vector("),
            |s| self.parse_value_like(s, false, true),
            char(')'),
        )
        .map(DeclOperator::VectorSize);

        // Putting it all together...
        //
        // Since this parser is **very** hot (10M calls on a test workload), even
        // failed sub-parser trials taking tens of nanoseconds start contributing to
        // its performance, so we dispatch to a reduced set of sub-parsers by
        // eagerly checking the first character of input. Branches are ordered by
        // decreasing frequency of occurence.
        //
        match s.as_bytes().first() {
            Some(b'&') => reference.parse(s),
            Some(b'*') => basic_pointer.parse(s),
            Some(b'(') => function.or(parenthesized).or(member_pointer).parse(s),
            Some(b'[') => function.or(array).parse(s),
            Some(b'_') => vector_size.or(member_pointer).parse(s),
            Some(b'c') => cv.or(member_pointer).parse(s),
            Some(b'v') => cv.or(member_pointer).parse(s),
            _ => member_pointer.parse(s),
        }
    }
}
//
impl Entities {
    /// Access a previously parsed declarator
    pub fn declarator(&self, d: DeclaratorKey) -> DeclaratorView {
        DeclaratorView::new(d, &self.declarators, self)
    }
}

/// View of a declarator
pub type DeclaratorView<'entities> = SliceView<
    'entities,
    DeclOperator,
    DeclOperatorView<'entities>,
    DeclaratorKeyImpl,
    DECLARATOR_LEN_BITS,
>;

/// Operators that can appear within a declarator
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DeclOperator {
    /// CV qualifier
    ConstVolatile(ConstVolatile),

    /// Pointer declarator
    Pointer {
        /// Nested name specifier (for pointer-to-member)
        path: NestedNameSpecifier,

        /// Const and volatile qualifiers,
        cv: ConstVolatile,
    },

    /// Reference declarator
    Reference(Reference),

    /// Array declarator, with optional size
    Array(Option<ValueKey>),

    /// Function declarator
    Function(FunctionSignature),

    /// Parentheses, used to override operator priorities
    Parenthesized(DeclaratorKey),

    /// Vector size, as in `__vector(2)`
    VectorSize(ValueKey),
}
//
impl From<Reference> for DeclOperator {
    fn from(r: Reference) -> Self {
        Self::Reference(r)
    }
}
//
impl From<FunctionSignature> for DeclOperator {
    fn from(f: FunctionSignature) -> Self {
        Self::Function(f)
    }
}
//
impl From<DeclaratorKey> for DeclOperator {
    fn from(d: DeclaratorKey) -> Self {
        Self::Parenthesized(d)
    }
}

/// View of a type declarator component (operator)
#[derive(PartialEq)]
pub enum DeclOperatorView<'entities> {
    /// CV qualifier
    ConstVolatile(ConstVolatile),

    /// Pointer declarator
    Pointer {
        /// Nested name specifier (for pointer-to-member)
        path: NestedNameSpecifierView<'entities>,

        /// Const and volatile qualifiers,
        cv: ConstVolatile,
    },

    /// Reference declarator
    Reference(Reference),

    /// Array declarator, with optional size
    Array(Option<ValueView<'entities>>),

    /// Function declarator
    Function(FunctionSignatureView<'entities>),

    /// Parentheses, used to override operator priorities
    Parenthesized(DeclaratorView<'entities>),

    /// Vector size, as in `__vector(2)`
    VectorSize(ValueView<'entities>),
}
//
impl<'entities> DeclOperatorView<'entities> {
    /// Build an operator view
    pub(crate) fn new(op: DeclOperator, entities: &'entities Entities) -> Self {
        match op {
            DeclOperator::ConstVolatile(cv) => Self::ConstVolatile(cv),
            DeclOperator::Pointer { path, cv } => Self::Pointer {
                path: entities.nested_name_specifier(path),
                cv,
            },
            DeclOperator::Reference(r) => Self::Reference(r),
            DeclOperator::Array(v) => Self::Array(v.map(|v| entities.value_like(v))),
            DeclOperator::Function(f) => Self::Function(entities.function_signature(f)),
            DeclOperator::Parenthesized(d) => Self::Parenthesized(entities.declarator(d)),
            DeclOperator::VectorSize(v) => Self::VectorSize(entities.value_like(v)),
        }
    }
}
//
impl<'entities> Display for DeclOperatorView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::ConstVolatile(cv) => write!(f, "{cv}")?,
            Self::Pointer { path, cv } => {
                write!(f, "{path}*")?;
                if *cv != ConstVolatile::default() {
                    write!(f, " {cv}")?;
                }
            }
            Self::Reference(r) => write!(f, "{r}")?,
            Self::Array(a) => {
                write!(f, "[")?;
                if let Some(a) = a {
                    write!(f, "{a}")?;
                }
                write!(f, "]")?;
            }
            Self::Function(func) => write!(f, "{func}")?,
            Self::Parenthesized(d) => write!(f, "({d})")?,
            Self::VectorSize(s) => write!(f, "__vector({s})")?,
        }
        Ok(())
    }
}
//
impl<'entities> SliceItemView<'entities> for DeclOperatorView<'entities> {
    type Inner = DeclOperator;

    fn new(inner: Self::Inner, entities: &'entities Entities) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "";

    const DISPLAY_SEPARATOR: &'static str = " ";

    const DISPLAY_TRAILER: &'static str = "";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn decl_operator() {
        let parser = EntityParser::new();

        // CV qualifier
        assert_eq!(
            parser.parse_decl_operator("const volatile"),
            Ok((
                "",
                DeclOperator::ConstVolatile(ConstVolatile::CONST | ConstVolatile::VOLATILE),
            ))
        );

        // Basic pointer syntax
        let nested_name_specifier = |s| unwrap_parse(parser.parse_nested_name_specifier(s));
        assert_eq!(
            parser.parse_decl_operator("*"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: nested_name_specifier(""),
                    cv: ConstVolatile::default(),
                }
            ))
        );

        // Pointer with CV qualifier
        assert_eq!(
            parser.parse_decl_operator("* const"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: nested_name_specifier(""),
                    cv: ConstVolatile::CONST,
                }
            ))
        );

        // Pointer to member
        assert_eq!(
            parser.parse_decl_operator("A::B::*"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: nested_name_specifier("A::B::"),
                    cv: ConstVolatile::default(),
                }
            ))
        );

        // Reference
        assert_eq!(
            parser.parse_decl_operator("&"),
            Ok(("", Reference::LValue.into()))
        );

        // Array of unknown length
        assert_eq!(
            parser.parse_decl_operator("[]"),
            Ok(("", DeclOperator::Array(None)))
        );

        // Array of known length
        assert_eq!(
            parser.parse_decl_operator("[42]"),
            Ok((
                "",
                DeclOperator::Array(Some(unwrap_parse(
                    parser.parse_value_like("42", false, true)
                )))
            ))
        );

        // Function signature
        assert_eq!(
            parser.parse_decl_operator("()"),
            Ok((
                "",
                unwrap_parse(parser.parse_function_signature("()")).into()
            ))
        );

        // Parenthesized declarator
        assert_eq!(
            parser.parse_decl_operator("(&&)"),
            Ok(("", unwrap_parse(parser.parse_declarator("&&")).into()))
        );

        // Vector size
        assert_eq!(
            parser.parse_decl_operator("__vector(2)"),
            Ok((
                "",
                DeclOperator::VectorSize(unwrap_parse(parser.parse_value_like("2", false, true)))
            ))
        );
    }

    #[test]
    fn declarator() {
        let parser = EntityParser::new();

        let test_case = |declarator: &str, expected_operators: &[&str]| {
            assert_matches!(parser.parse_declarator(declarator), Ok(("", key)) => {
                let declarator = parser.declarator(key);
                assert_eq!(declarator.len(), expected_operators.len());
                for (expected, actual) in expected_operators.iter().zip(declarator.to_vec()) {
                    let expected = unwrap_parse(parser.parse_decl_operator(*expected));
                    assert_eq!(expected, actual);
                }
            })
        };

        // Empty declarator
        test_case("", &[]);

        // Single operator
        test_case("&", &["&"]);

        // Multiple operators
        test_case("&&*const()", &["&&", "*const", "()"]);
    }
}