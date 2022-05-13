//! Id-expression related parsing
//!
//! In the C++ grammar, id-expressions are things which are more complex than
//! identifiers but can be used in place of identifiers, such as qualified
//! identifiers (path::to::something) or templated names (A<B>).

use crate::cpp::{
    anonymous::{self, AnonymousEntity, Lambda},
    atoms,
    functions::{self, FunctionSignature},
    templates::{self, TemplateParameter},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser for id-expressions
pub fn id_expression(s: &str) -> IResult<IdExpression> {
    use nom::multi::many0;
    let path = many0(scope).map(Vec::into_boxed_slice);
    (path.and(unqualified_id))
        .map(|(path, id)| IdExpression { path, id })
        .parse(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IdExpression<'source> {
    /// Hierarchical scope
    path: Box<[Scope<'source>]>,

    /// Unqualified id-expression
    id: UnqualifiedId<'source>,
}
//
impl<'source> From<UnqualifiedId<'source>> for IdExpression<'source> {
    fn from(id: UnqualifiedId<'source>) -> Self {
        Self {
            path: Default::default(),
            id,
        }
    }
}
//
impl<'source> From<&'source str> for IdExpression<'source> {
    fn from(id: &'source str) -> Self {
        Self {
            path: Default::default(),
            id: UnqualifiedId::from(id),
        }
    }
}

/// Parser for unqualified id-expressions
fn unqualified_id(s: &str) -> IResult<UnqualifiedId> {
    use nom::{character::complete::char, combinator::opt, sequence::tuple};
    // FIXME: Accept all unqualified id-expressions. Currently missing...
    //        - Operators, including conversion operators
    let is_destructor = opt(char('~')).map(|opt| opt.is_some());
    let named = tuple((
        is_destructor,
        atoms::identifier,
        opt(templates::template_parameters),
    ))
    .map(
        |(is_destructor, id, template_parameters)| UnqualifiedId::Named {
            is_destructor,
            id,
            template_parameters,
        },
    );
    let lambda = anonymous::lambda.map(UnqualifiedId::Lambda);
    let anonymous = anonymous::anonymous.map(UnqualifiedId::Anonymous);
    named.or(lambda).or(anonymous).parse(s)
}
//
#[derive(Clone, Debug, PartialEq)]
pub enum UnqualifiedId<'source> {
    /// An entity named by a standard identifier
    Named {
        /// Truth that this is a destructor (names starts with ~)
        is_destructor: bool,

        /// Base identifier
        id: &'source str,

        /// Optional template parameters
        template_parameters: Option<Box<[TemplateParameter<'source>]>>,
    },

    /// A lambda function, with source location information
    Lambda(Lambda<'source>),

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

/// Parser for scopes (things that contain other things)
fn scope(s: &str) -> IResult<Scope> {
    use nom::combinator::opt;
    use nom_supreme::tag::complete::tag;
    (unqualified_id
        .and(opt(functions::function_signature))
        .terminated(tag("::")))
    .map(|(id, function_signature)| Scope {
        id,
        function_signature,
    })
    .parse(s)
}
//
/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Scope<'source> {
    /// What identifies the scope
    id: UnqualifiedId<'source>,

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    function_signature: Option<FunctionSignature<'source>>,
}
//
impl<'source> From<&'source str> for Scope<'source> {
    fn from(id: &'source str) -> Self {
        Self {
            id: id.into(),
            function_signature: None,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::cpp::tests::force_parse_type;
    use pretty_assertions::assert_eq;

    #[test]
    fn unqualified_id() {
        // Just an identifier
        assert_eq!(
            super::unqualified_id("basic"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "basic",
                    template_parameters: None,
                }
            ))
        );

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
                    template_parameters: Some(vec![].into()),
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
                    template_parameters: Some(
                        vec![force_parse_type("B").into(), force_parse_type("C").into()].into()
                    )
                }
            ))
        );

        // Lambda
        assert_eq!(
            super::unqualified_id("(lambda at /path/to/stuff.h:9876:54)"),
            Ok((
                "",
                UnqualifiedId::Lambda(
                    anonymous::lambda("(lambda at /path/to/stuff.h:9876:54)")
                        .unwrap()
                        .1
                )
            ))
        );

        // Anonymous entity
        assert_eq!(
            super::unqualified_id("(anonymous class)"),
            Ok((
                "",
                UnqualifiedId::Anonymous(anonymous::anonymous("(anonymous class)").unwrap().1)
            ))
        );
    }

    #[test]
    fn scope() {
        // Without function signature
        assert_eq!(
            super::scope("std::"),
            Ok((
                "",
                Scope {
                    id: "std".into(),
                    function_signature: None,
                }
            ))
        );

        // With function signature
        assert_eq!(
            super::scope("my_function()::"),
            Ok((
                "",
                Scope {
                    id: "my_function".into(),
                    function_signature: Some(FunctionSignature::default()),
                }
            ))
        );
    }

    #[test]
    fn id_expression() {
        // Without any path
        assert_eq!(
            super::id_expression("something"),
            Ok((
                "",
                IdExpression {
                    path: vec![].into(),
                    id: "something".into(),
                }
            ))
        );

        // With a path
        assert_eq!(
            super::id_expression("boost::hana::to_t<unsigned long long>"),
            Ok((
                "",
                IdExpression {
                    path: vec!["boost".into(), "hana".into()].into(),
                    id: UnqualifiedId::Named {
                        is_destructor: false,
                        id: "to_t",
                        template_parameters: Some(
                            vec![force_parse_type("unsigned long long").into()].into()
                        )
                    }
                }
            ))
        );
    }
}
