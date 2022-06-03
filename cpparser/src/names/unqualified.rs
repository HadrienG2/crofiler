//! Unqualified id-expressions (those that do not feature the :: scope operator)

use super::atoms;
use crate::{
    anonymous::{self, AnonymousEntity, Lambda},
    operators::Operator,
    templates::{self, TemplateParameters},
    values::{self, ValueLike},
    IResult,
};
use nom::Parser;
use std::path::Path;

/// Parser for unqualified id-expressions
pub fn unqualified_id(s: &str) -> IResult<UnqualifiedId> {
    use crate::operators::overloads::operator_overload;
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::delimited,
    };
    use nom_supreme::tag::complete::tag;

    // An entity named by a user-specified identifier
    let named = |is_destructor| {
        (atoms::identifier.and(opt(templates::template_parameters))).map(
            move |(id, template_parameters)| UnqualifiedId::Named {
                is_destructor,
                id,
                template_parameters,
            },
        )
    };

    // An operator overload
    let operator =
        operator_overload.map(|(operator, template_parameters)| UnqualifiedId::Operator {
            operator,
            template_parameters,
        });

    // A decltype expression
    let decltype = delimited(
        tag("decltype(").and(space0),
        values::value_like::<false, true>,
        space0.and(char(')')),
    )
    .map(Box::new)
    .map(UnqualifiedId::Decltype);

    // Anonymous entities to which clang gives a name
    let lambda = (|s| anonymous::lambda(s, Path::new)).map(UnqualifiedId::Lambda);
    let anonymous = anonymous::anonymous.map(UnqualifiedId::Anonymous);

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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnqualifiedId<'source> {
    /// An entity named by a user-specified identifier
    Named {
        /// Truth that this is a destructor (names starts with ~)
        is_destructor: bool,

        /// Base identifier
        id: &'source str,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters<'source>>,
    },

    /// An operator overload
    Operator {
        /// Which operator was overloaded
        operator: Operator<'source>,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters<'source>>,
    },

    /// A decltype(<value>) expression
    Decltype(Box<ValueLike<'source>>),

    /// A lambda function, with source location information
    Lambda(Lambda<&'source Path>),

    /// Another kind of anonymous entity from clang
    Anonymous(AnonymousEntity<'source>),
}
//
impl Default for UnqualifiedId<'_> {
    fn default() -> Self {
        Self::Anonymous(AnonymousEntity::default())
    }
}
//
impl<'source> From<&'source str> for UnqualifiedId<'source> {
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

    #[test]
    fn unqualified_id() {
        // Just an identifier
        assert_eq!(super::unqualified_id("basic"), Ok(("", "basic".into())));

        // Destructor
        assert_eq!(
            super::unqualified_id("~stuff"),
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
            super::unqualified_id("no_parameters<>"),
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
        assert_eq!(
            super::unqualified_id("A<B, C>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "A",
                    template_parameters: Some(Some(
                        vec![
                            force_parse(types::type_like, "B").into(),
                            force_parse(types::type_like, "C").into()
                        ]
                        .into()
                    ))
                }
            ))
        );

        // Operator overload
        assert_eq!(
            super::unqualified_id("operator()"),
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
            super::unqualified_id("decltype(42)"),
            Ok(("", UnqualifiedId::Decltype(Box::new(42u8.into()))))
        );

        // Lambda
        assert_eq!(
            super::unqualified_id("(lambda at /path/to/stuff.h:9876:54)"),
            Ok((
                "",
                UnqualifiedId::Lambda(force_parse(
                    |s| anonymous::lambda(s, Path::new),
                    "(lambda at /path/to/stuff.h:9876:54)"
                ))
            ))
        );

        // Anonymous entity
        assert_eq!(
            super::unqualified_id("(anonymous class)"),
            Ok((
                "",
                UnqualifiedId::Anonymous(force_parse(anonymous::anonymous, "(anonymous class)"))
            ))
        );
    }
}
