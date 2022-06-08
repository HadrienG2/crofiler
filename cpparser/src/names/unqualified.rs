//! Unqualified id-expressions (those that do not feature the :: scope operator)

use crate::{
    anonymous::{self, AnonymousEntity, Lambda},
    names::atoms,
    operators::Operator,
    templates::{self, TemplateParameters},
    values::{self, ValueLike},
    EntityParser, IResult,
};
use nom::Parser;
use std::fmt::Debug;

impl EntityParser {
    /// Parser for unqualified id-expressions
    pub fn parse_unqualified_id<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, UnqualifiedId<'source, atoms::IdentifierKey, crate::PathKey>> {
        unqualified_id(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parser for unqualified id-expressions
// TODO: Make private once users are migrated
pub fn unqualified_id<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, UnqualifiedId<'source, IdentifierKey, PathKey>> {
    use crate::operators::overloads::operator_overload;
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::delimited,
    };
    use nom_supreme::tag::complete::tag;

    // An entity named by a user-specified identifier
    let named = |is_destructor| {
        ((parse_identifier).and(opt(templates::template_parameters))).map(
            move |(id, template_parameters)| UnqualifiedId::Named {
                is_destructor,
                id,
                template_parameters,
            },
        )
    };

    // An operator overload
    let operator = (|s| operator_overload(s, parse_identifier, path_to_key)).map(
        |(operator, template_parameters)| UnqualifiedId::Operator {
            operator,
            template_parameters,
        },
    );

    // A decltype expression
    let decltype = delimited(
        tag("decltype(").and(space0),
        values::value_like::<false, true>,
        space0.and(char(')')),
    )
    .map(Box::new)
    .map(UnqualifiedId::Decltype);

    // Anonymous entities to which clang gives a name
    let lambda = (|s| anonymous::lambda(s, path_to_key)).map(UnqualifiedId::Lambda);
    let anonymous = (|s| anonymous::anonymous(s, parse_identifier)).map(UnqualifiedId::Anonymous);

    // Operator and decltype must go before named because named matches keywords
    //
    // Since this parser is **very** hot (500M calls on a test workload), even
    // failed sub-parser trials taking ~10ns contribute to its performance, so
    // we dispatch to appropriate sub-parsers by eagerly checking the first
    // character of input. This also allows us to tell if a named entity is a
    // destructor or not. Branches other than _ are ordered by decreasing freq.
    //
    match s.as_bytes().first() {
        Some(b'(') => lambda.or(anonymous).parse(s),
        Some(b'd') => decltype.or(named(false)).parse(s),
        Some(b'o') => operator.or(named(false)).parse(s),
        Some(b'~') => named(true).parse(&s[1..]),
        _ => named(false).parse(s),
    }
}
//
/// Unqualified id-expression
///
/// This is the next level of complexity in C++ entity naming after raw
/// identifiers, it allows for things like templating and operator overloading
/// but not for scoping.
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnqualifiedId<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// An entity named by a user-specified identifier
    Named {
        /// Truth that this is a destructor (names starts with ~)
        is_destructor: bool,

        /// Base identifier
        id: IdentifierKey,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters<'source>>,
    },

    /// An operator overload
    Operator {
        /// Which operator was overloaded
        operator: Operator<'source, IdentifierKey, PathKey>,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters<'source>>,
    },

    /// A decltype(<value>) expression
    Decltype(Box<ValueLike<'source>>),

    /// A lambda function, with source location information
    Lambda(Lambda<PathKey>),

    /// Another kind of anonymous entity from clang
    Anonymous(AnonymousEntity<IdentifierKey>),
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for UnqualifiedId<'_, IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self::Anonymous(AnonymousEntity::default())
    }
}
//
impl<'source, PathKey: Clone + Debug + PartialEq + Eq> From<&'source str>
    for UnqualifiedId<'source, &'source str, PathKey>
{
    fn from(id: &'source str) -> Self {
        Self::Named {
            is_destructor: false,
            id,
            template_parameters: None,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{tests::force_parse, types};
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn unqualified_id() {
        let parse_unqualified_id = |s| super::unqualified_id(s, &atoms::identifier, &Path::new);

        // Just an identifier
        assert_eq!(parse_unqualified_id("basic"), Ok(("", "basic".into())));

        // Destructor
        assert_eq!(
            parse_unqualified_id("~stuff"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: true,
                    id: "stuff",
                    template_parameters: None,
                }
            ))
        );

        // Template with no parameters
        assert_eq!(
            parse_unqualified_id("no_parameters<>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "no_parameters",
                    template_parameters: Some(Some(vec![].into())),
                }
            ))
        );

        // Template with a few parameters
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_unqualified_id("A<B, C>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "A",
                    template_parameters: Some(Some(
                        vec![
                            force_parse(parse_type_like, "B").into(),
                            force_parse(parse_type_like, "C").into()
                        ]
                        .into()
                    ))
                }
            ))
        );

        // Operator overload
        assert_eq!(
            parse_unqualified_id("operator()"),
            Ok((
                "",
                UnqualifiedId::Operator {
                    operator: Operator::CallIndex { is_index: false },
                    template_parameters: Default::default(),
                }
            ))
        );

        // Decltype
        assert_eq!(
            parse_unqualified_id("decltype(42)"),
            Ok(("", UnqualifiedId::Decltype(Box::new(42u8.into()))))
        );

        // Lambda
        assert_eq!(
            parse_unqualified_id("(lambda at /path/to/stuff.h:9876:54)"),
            Ok((
                "",
                UnqualifiedId::Lambda(force_parse(
                    |s| anonymous::lambda(s, &Path::new),
                    "(lambda at /path/to/stuff.h:9876:54)"
                ))
            ))
        );

        // Anonymous entity
        assert_eq!(
            parse_unqualified_id("(anonymous class)"),
            Ok((
                "",
                UnqualifiedId::Anonymous(force_parse(
                    |s| anonymous::anonymous(s, &atoms::identifier),
                    "(anonymous class)"
                ))
            ))
        );
    }
}
