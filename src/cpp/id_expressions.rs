//! Id-expression related parsing
//!
//! In the C++ grammar, id-expressions are things which are more complex than
//! identifiers but can be used in place of identifiers, such as qualified
//! identifiers (path::to::something) or templated names (A<B>).

use crate::cpp::{
    anonymous::{self, Lambda},
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
    use nom_supreme::tag::complete::tag;
    let path = many0(scope.terminated(tag("::"))).map(Vec::into_boxed_slice);
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

/// Parser recognizing an identifier which may or may not be coupled with
/// template arguments, i.e. id or id<...>
fn templatable_id(s: &str) -> IResult<TemplatableId> {
    use nom::combinator::opt;
    (atoms::identifier.and(opt(templates::template_parameters)))
        .map(|(id, parameters)| TemplatableId { id, parameters })
        .parse(s)
}
//
/// Identifier which may or may not have template arguments
#[derive(Clone, Default, Debug, PartialEq)]
pub struct TemplatableId<'source> {
    /// Identifier
    id: &'source str,

    /// Optional template parameters
    parameters: Option<Box<[TemplateParameter<'source>]>>,
}
//
impl<'source> From<&'source str> for TemplatableId<'source> {
    fn from(id: &'source str) -> Self {
        Self {
            id,
            parameters: Default::default(),
        }
    }
}

/// Parser for a templatable identifier or anonymous entity.
fn templatable_or_anonymous(s: &str) -> IResult<TemplatableOrAnonymous> {
    let templatable_id = templatable_id.map(TemplatableOrAnonymous::TemplatableId);
    let anonymous = anonymous::anonymous.map(TemplatableOrAnonymous::Anonymous);
    templatable_id.or(anonymous).parse(s)
}
//
/// Templatable identifier or anonymous entity
#[derive(Clone, Debug, PartialEq)]
pub enum TemplatableOrAnonymous<'source> {
    /// Identifier that may have template parameters
    TemplatableId(TemplatableId<'source>),

    /// Anonymous entity, if present the argument clarifies if this is an
    /// anonymous class, namespace...
    Anonymous(Option<&'source str>),
}
//
impl Default for TemplatableOrAnonymous<'_> {
    fn default() -> Self {
        Self::TemplatableId(TemplatableId::default())
    }
}
//
impl<'source> From<&'source str> for TemplatableOrAnonymous<'source> {
    fn from(id: &'source str) -> Self {
        Self::TemplatableId(TemplatableId::from(id))
    }
}

/// Parser for scopes
fn scope(s: &str) -> IResult<Scope> {
    use nom::combinator::opt;
    (templatable_or_anonymous.and(opt(functions::function_signature)))
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
    id: TemplatableOrAnonymous<'source>,

    /// When functions are scopes containing other entities (which can happen
    /// due to lambdas), they can appear with a signature in the path.
    function_signature: Option<FunctionSignature<'source>>,
}

/// Parser for unqualified id-expressions
fn unqualified_id(s: &str) -> IResult<UnqualifiedId> {
    // FIXME: Accept all unqualified id-expressions. In addition to identifiers,
    //        these include...
    //        - Destructors: ~identifier
    //        - Templates: identifier<param...>
    //        - Operators, including conversion operators
    let templatable_or_anonymous =
        templatable_or_anonymous.map(UnqualifiedId::TemplatableOrAnonymous);
    let lambda = anonymous::lambda.map(UnqualifiedId::Lambda);
    templatable_or_anonymous.or(lambda).parse(s)
}
//
#[derive(Clone, Debug, PartialEq)]
pub enum UnqualifiedId<'source> {
    /// Templatable identifiers and anonymous entities can appear in
    /// unqualified-id position
    TemplatableOrAnonymous(TemplatableOrAnonymous<'source>),

    /// A lambda can also appear in unqualified-id position
    Lambda(Lambda<'source>),
}
//
impl Default for UnqualifiedId<'_> {
    fn default() -> Self {
        Self::TemplatableOrAnonymous(TemplatableOrAnonymous::default())
    }
}
//
impl<'source> From<&'source str> for UnqualifiedId<'source> {
    fn from(id: &'source str) -> Self {
        Self::TemplatableOrAnonymous(TemplatableOrAnonymous::from(id))
    }
}

// FIXME: Modularize and add tests
#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::cpp::tests::force_parse_type;
    use pretty_assertions::assert_eq;

    #[test]
    fn templatable_id() {
        assert_eq!(
            super::templatable_id("no_parameters"),
            Ok((
                "",
                TemplatableId {
                    id: "no_parameters",
                    parameters: None,
                }
            ))
        );
        assert_eq!(
            super::templatable_id("empty_parameters<>"),
            Ok((
                "",
                TemplatableId {
                    id: "empty_parameters",
                    parameters: Some(vec![].into()),
                }
            ))
        );
        assert_eq!(
            super::templatable_id("A<B, C>"),
            Ok((
                "",
                TemplatableId {
                    id: "A",
                    parameters: Some(
                        vec![force_parse_type("B").into(), force_parse_type("C").into()].into()
                    )
                }
            ))
        );
    }
}
