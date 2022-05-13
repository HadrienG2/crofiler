//! C++ entity name parsing

mod anonymous;
mod atoms;
mod functions;
mod templates;
mod types;
mod values;

use nom::Parser;
use nom_supreme::ParserExt;

// TODO: Keep this up to date over time
pub use self::{
    anonymous::Lambda,
    functions::FunctionSignature,
    templates::{TemplatableId, TemplateParameter},
    types::{
        qualifiers::{ConstVolatile, Reference},
        TypeLike,
    },
};

// FIXME: Remove once done debugging
#[cfg(test)]
type Error<I> = nom::error::Error<I>;
#[cfg(not(test))]
type Error<I> = nom_supreme::error::ErrorTree<I>;
pub type IResult<'a, O> = nom::IResult<&'a str, O, Error<&'a str>>;

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<Option<TypeLike>> {
    let type_like = |s| types::type_like(s, atoms::end_of_string);
    let type_like = type_like.map(Some);
    let unknown = anonymous::unknown_entity.value(None);
    type_like.or(unknown).parse(s)
}

/// Parser for a templatable identifier or anonymous entity.
fn templatable_or_anonymous(s: &str) -> IResult<TemplatableOrAnonymous> {
    let templatable_id = templates::templatable_id.map(TemplatableOrAnonymous::TemplatableId);
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

    /// Functions can also act as scopes
    function_signature: Option<FunctionSignature<'source>>,
}

/// Parser for unqualified id-expressions
fn unqualified_id_expression(s: &str) -> IResult<UnqualifiedId> {
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

/// Parser for id-expressions
fn id_expression(s: &str) -> IResult<IdExpression> {
    use nom::multi::many0;
    use nom_supreme::tag::complete::tag;
    let path = many0(scope.terminated(tag("::"))).map(Vec::into_boxed_slice);
    (path.and(unqualified_id_expression))
        .map(|(path, id)| IdExpression { path, id })
        .parse(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IdExpression<'source> {
    /// Hierarchical scope (types or namespaces)
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

// FIXME: Modularize and add tests
#[cfg(test)]
pub mod tests {
    use super::*;

    pub fn force_parse_type(s: &str) -> TypeLike {
        types::type_like(s, atoms::end_of_string).unwrap().1
    }
}
