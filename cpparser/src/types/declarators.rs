//! All about declarators
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

use super::qualifiers::{ConstVolatile, Reference};
use crate::{
    functions::{self, FunctionSignature},
    names::{
        atoms,
        scopes::{self, NestedNameSpecifier},
    },
    values::{self, ValueLike},
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
    ) -> IResult<'source, Declarator<'source, atoms::IdentifierKey, crate::PathKey>> {
        declarator(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }

    /// Parser for a declarator component
    pub fn parse_decl_operator<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, DeclOperator<'source, atoms::IdentifierKey, crate::PathKey>> {
        decl_operator(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parser for declarators
// TODO: Make private once users are migrated
pub fn declarator<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, Declarator<'source, IdentifierKey, PathKey>> {
    use nom::{character::complete::space0, multi::many0};
    many0((|s| decl_operator(s, parse_identifier, path_to_key)).terminated(space0))
        .map(Vec::into_boxed_slice)
        .parse(s)
}

/// Declarator
pub type Declarator<'source, IdentifierKey, PathKey> =
    Box<[DeclOperator<'source, IdentifierKey, PathKey>]>;

/// Parser for a declarator component
// TODO: Make private once users are migrated
fn decl_operator<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, DeclOperator<'source, IdentifierKey, PathKey>> {
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::{delimited, preceded, separated_pair},
    };

    // Reference declarator (/!\ Only works if we know >1 ref-sign is coming)
    let mut reference = EntityParser::parse_reference.map(DeclOperator::Reference);

    // Basic pointer declarator
    let mut basic_pointer =
        preceded(char('*').and(space0), EntityParser::parse_cv).map(|cv| DeclOperator::Pointer {
            path: Default::default(),
            cv,
        });

    // The member pointer declarator is very exotic (2/1M parses) and harder to
    // parse so we don't unify it with basic_pointer.
    let nested_star =
        (|s| scopes::nested_name_specifier(s, parse_identifier, path_to_key)).terminated(char('*'));
    let mut member_pointer = separated_pair(nested_star, space0, EntityParser::parse_cv)
        .map(|(path, cv)| DeclOperator::Pointer { path, cv });

    // Array declarator
    let mut array = delimited(
        char('[').and(space0),
        opt(|s| values::value_like(s, parse_identifier, path_to_key, false, true)),
        space0.and(char(']')),
    )
    .map(DeclOperator::Array);

    // Function declarator
    let function = (|s| functions::function_signature(s, parse_identifier, path_to_key))
        .map(DeclOperator::Function);

    // Parenthesized declarator (to override operator priorities)
    let parenthesized = delimited(
        char('(').and(space0),
        (|s| declarator(s, parse_identifier, path_to_key)).verify(|d| d != &Declarator::default()),
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

/// Operators that can appear within a declarator
// FIXME: This type appears in Box<[T]>, intern it once data is owned
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DeclOperator<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Pointer declarator
    Pointer {
        /// Nested name specifier (for pointer-to-member)
        path: NestedNameSpecifier<'source, IdentifierKey, PathKey>,

        /// Const and volatile qualifiers,
        cv: ConstVolatile,
    },

    /// Reference declarator
    Reference(Reference),

    /// Array declarator, with optional size
    Array(Option<ValueLike<'source, IdentifierKey, PathKey>>),

    /// Function declarator
    Function(FunctionSignature<'source, IdentifierKey, PathKey>),

    /// Parentheses, used to override operator priorities
    Parenthesized(Declarator<'source, IdentifierKey, PathKey>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::names::atoms;
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn decl_operator() {
        let parse_decl_operator = |s| super::decl_operator(s, &atoms::identifier, &Path::new);

        // Basic pointer syntax
        assert_eq!(
            parse_decl_operator("*"),
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
            parse_decl_operator("* const"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: NestedNameSpecifier::default(),
                    cv: ConstVolatile::CONST,
                }
            ))
        );

        // Pointer to member
        let parse_nested_name_specifier =
            |s| scopes::nested_name_specifier(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_decl_operator("A::B::*"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: force_parse(parse_nested_name_specifier, "A::B::"),
                    cv: ConstVolatile::default(),
                }
            ))
        );

        // Reference
        assert_eq!(
            parse_decl_operator("&"),
            Ok(("", DeclOperator::Reference(Reference::LValue)))
        );

        // Array of unknown length
        assert_eq!(
            parse_decl_operator("[]"),
            Ok(("", DeclOperator::Array(None)))
        );

        // Array of known length
        assert_eq!(
            parse_decl_operator("[42]"),
            Ok(("", DeclOperator::Array(Some(42u8.into()))))
        );

        // Function signature
        assert_eq!(
            parse_decl_operator("()"),
            Ok(("", DeclOperator::Function(FunctionSignature::default())))
        );

        // Parenthesized declarator
        assert_eq!(
            parse_decl_operator("(&&)"),
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
        let parse_declarator = |s| super::declarator(s, &atoms::identifier, &Path::new);

        // Empty declarator
        assert_eq!(parse_declarator(""), Ok(("", Declarator::default())));

        // Single operator
        assert_eq!(
            parse_declarator("&"),
            Ok(("", vec![DeclOperator::Reference(Reference::LValue)].into()))
        );

        // Multiple operators
        assert_eq!(
            parse_declarator("&&*const()"),
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
