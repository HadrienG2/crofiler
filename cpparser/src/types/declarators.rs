//! All about declarators
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

use super::qualifiers::{ConstVolatile, Reference};
use crate::{
    functions::FunctionSignature, names::scopes::NestedNameSpecifier, values::ValueKey, Entities,
    EntityParser, IResult,
};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

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
    pub(crate) fn declarator(&self, key: DeclaratorKey) -> Box<Declarator> {
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

        // Reference declarator (/!\ Only works because we know >1 ref-sign is coming below)
        let mut reference = Self::parse_reference.map(DeclOperator::Reference);

        // Basic pointer declarator
        let mut basic_pointer =
            preceded(char('*').and(space0), Self::parse_cv).map(|cv| DeclOperator::Pointer {
                path: self.scopes.entry().intern().into(),
                cv,
            });

        // The member pointer declarator is very exotic (2/1M parses) and harder to
        // parse so we don't unify it with basic_pointer.
        let nested_star = (|s| self.parse_nested_name_specifier(s)).terminated(char('*'));
        let mut member_pointer = separated_pair(nested_star, space0, Self::parse_cv)
            .map(|(path, cv)| DeclOperator::Pointer { path, cv });

        // Array declarator
        let mut array = delimited(
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
            Some(b'[') => array.parse(s),
            _ => member_pointer.parse(s),
        }
    }
}
//
impl Entities {
    /// Retrieve a previously interned declarator
    pub fn declarator(&self, key: DeclaratorKey) -> &Declarator {
        self.declarators.get(key)
    }
}

/// Declarator
pub type Declarator = [DeclOperator];

/// Operators that can appear within a declarator
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DeclOperator {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn decl_operator() {
        let parser = EntityParser::new();

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
                    parser.parse_value_like("42", true, true)
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
