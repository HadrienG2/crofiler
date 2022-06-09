//! All about declarators
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

use super::qualifiers::{ConstVolatile, Reference};
use crate::{
    functions::FunctionSignature,
    names::{atoms, scopes::NestedNameSpecifier},
    values::ValueLike,
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser for declarators
    pub fn parse_declarator<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Declarator<atoms::IdentifierKey, crate::PathKey>> {
        use nom::{character::complete::space0, multi::many0};
        many0((|s| self.parse_decl_operator(s)).terminated(space0))
            .map(Vec::into_boxed_slice)
            .parse(s)
    }

    /// Parser for a declarator component
    fn parse_decl_operator<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, DeclOperator<atoms::IdentifierKey, crate::PathKey>> {
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
                path: Default::default(),
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
            (|s| self.parse_declarator(s)).verify(|d| d != &Declarator::default()),
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

/// Declarator
pub type Declarator<IdentifierKey, PathKey> = Box<[DeclOperator<IdentifierKey, PathKey>]>;

/// Operators that can appear within a declarator
// FIXME: This type appears in Box<[T]>, intern it once data is owned
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DeclOperator<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Pointer declarator
    Pointer {
        /// Nested name specifier (for pointer-to-member)
        path: NestedNameSpecifier<IdentifierKey, PathKey>,

        /// Const and volatile qualifiers,
        cv: ConstVolatile,
    },

    /// Reference declarator
    Reference(Reference),

    /// Array declarator, with optional size
    Array(Option<ValueLike<IdentifierKey, PathKey>>),

    /// Function declarator
    Function(FunctionSignature<IdentifierKey, PathKey>),

    /// Parentheses, used to override operator priorities
    Parenthesized(Declarator<IdentifierKey, PathKey>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn decl_operator() {
        let parser = EntityParser::new();

        // Basic pointer syntax
        assert_eq!(
            parser.parse_decl_operator("*"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: NestedNameSpecifier::default(),
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
                    path: NestedNameSpecifier::default(),
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
                    path: unwrap_parse(parser.parse_nested_name_specifier("A::B::")),
                    cv: ConstVolatile::default(),
                }
            ))
        );

        // Reference
        assert_eq!(
            parser.parse_decl_operator("&"),
            Ok(("", DeclOperator::Reference(Reference::LValue)))
        );

        // Array of unknown length
        assert_eq!(
            parser.parse_decl_operator("[]"),
            Ok(("", DeclOperator::Array(None)))
        );

        // Array of known length
        assert_eq!(
            parser.parse_decl_operator("[42]"),
            Ok(("", DeclOperator::Array(Some(42u8.into()))))
        );

        // Function signature
        assert_eq!(
            parser.parse_decl_operator("()"),
            Ok(("", DeclOperator::Function(FunctionSignature::default())))
        );

        // Parenthesized declarator
        assert_eq!(
            parser.parse_decl_operator("(&&)"),
            Ok((
                "",
                DeclOperator::Parenthesized(
                    vec![DeclOperator::Reference(Reference::RValue)].into()
                )
            ))
        );
    }

    #[test]
    fn declarator() {
        let parser = EntityParser::new();

        // Empty declarator
        assert_eq!(parser.parse_declarator(""), Ok(("", Declarator::default())));

        // Single operator
        assert_eq!(
            parser.parse_declarator("&"),
            Ok(("", vec![DeclOperator::Reference(Reference::LValue)].into()))
        );

        // Multiple operators
        assert_eq!(
            parser.parse_declarator("&&*const()"),
            Ok((
                "",
                vec![
                    DeclOperator::Reference(Reference::RValue),
                    DeclOperator::Pointer {
                        path: NestedNameSpecifier::default(),
                        cv: ConstVolatile::CONST,
                    },
                    DeclOperator::Function(FunctionSignature::default())
                ]
                .into()
            ))
        );
    }
}
