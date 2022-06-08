//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{declarators::Declarator, specifiers::TypeSpecifier};
use crate::{functions, names::atoms, values::ValueLike, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser recognizing types (and some values that are indistinguishable from
    /// types without extra context).
    pub fn parse_type_like<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TypeLike<'source, atoms::IdentifierKey, crate::PathKey>> {
        type_like(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parser recognizing types (and some values that are indistinguishable from
/// types without extra context).
// TODO: Make private once users are migrated
pub fn type_like<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, TypeLike<'source, IdentifierKey, PathKey>> {
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::{delimited, tuple},
    };
    use nom_supreme::tag::complete::tag;

    // GNU-style type attributes come first
    let attributes = opt(delimited(
        tag("__attribute__("),
        |s| functions::function_call(s, parse_identifier, path_to_key),
        char(')'),
    ))
    .map(Option::unwrap_or_default);

    // Then come the type specifier and declarator
    tuple((
        attributes.terminated(space0),
        (|s| specifiers::type_specifier(s, parse_identifier, path_to_key)).terminated(space0),
        |s| declarators::declarator(s, parse_identifier, path_to_key),
    ))
    .map(|(attributes, type_specifier, declarator)| TypeLike {
        attributes,
        type_specifier,
        declarator,
    })
    .parse(s)
}

/// A type name, or something looking close enough to it
// FIXME: This type appears in Box<T> and Box<[T]>, intern those once data is owned
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeLike<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// GNU-style attributes __attribute__((...))
    attributes: Box<[ValueLike<'source, IdentifierKey, PathKey>]>,

    /// Type specifier
    type_specifier: TypeSpecifier<'source, IdentifierKey, PathKey>,

    /// Declarator
    declarator: Declarator<'source, IdentifierKey, PathKey>,
}
//
impl<
        'source,
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
        T: Into<TypeSpecifier<'source, IdentifierKey, PathKey>>,
    > From<T> for TypeLike<'source, IdentifierKey, PathKey>
{
    fn from(type_specifier: T) -> Self {
        Self {
            type_specifier: type_specifier.into(),
            ..Default::default()
        }
    }
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for TypeLike<'_, IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self {
            attributes: Default::default(),
            type_specifier: Default::default(),
            declarator: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::declarators::DeclOperator;
    use super::*;
    use crate::{functions::FunctionSignature, tests::force_parse, values};
    use pretty_assertions::assert_eq;
    use std::path::Path;

    fn parse_attribute_value(s: &str) -> IResult<ValueLike<&str, &Path>> {
        values::value_like(s, &atoms::identifier, &Path::new, false, false)
    }

    #[test]
    fn type_like() {
        let parse_type_like = |s| super::type_like(s, &atoms::identifier, &Path::new);
        let parse_type_specifier =
            |s| specifiers::type_specifier(s, &atoms::identifier, &Path::new);

        // Basic type specifier
        assert_eq!(
            parse_type_like("signed char"),
            Ok((
                "",
                TypeLike {
                    type_specifier: force_parse(parse_type_specifier, "signed char"),
                    ..Default::default()
                }
            ))
        );

        // GNU-style attributes before
        assert_eq!(
            parse_type_like("__attribute__((unused)) long long"),
            Ok((
                "",
                TypeLike {
                    attributes: vec![force_parse(parse_attribute_value, "unused")].into(),
                    type_specifier: force_parse(parse_type_specifier, "long long"),
                    ..Default::default()
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            parse_type_like("something()"),
            Ok((
                "",
                TypeLike {
                    type_specifier: force_parse(parse_type_specifier, "something"),
                    declarator: vec![DeclOperator::Function(FunctionSignature::default())].into(),
                    ..Default::default()
                }
            ))
        );

        // Fun template/expression ambiguity found during testing
        let parse_function_signature =
            |s| functions::function_signature(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_type_like("T<1>(U)"),
            Ok((
                "",
                TypeLike {
                    type_specifier: force_parse(parse_type_specifier, "T<1>"),
                    declarator: vec![DeclOperator::Function(force_parse(
                        parse_function_signature,
                        "(U)"
                    ))]
                    .into(),
                    ..Default::default()
                }
            ))
        );
    }
}
