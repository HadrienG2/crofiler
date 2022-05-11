//! C++ entity name parsing

mod atoms;
mod functions;
mod templates;
mod types;

use nom::{IResult, Parser};
use nom_supreme::ParserExt;

pub use self::{
    atoms::{ConstVolatile, Lambda},
    templates::{TemplatableId, TemplateParameter},
    types::TypeLike,
};

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<&str, Option<CppEntity>> {
    let type_like = |s| types::type_like(s, atoms::end_of_string);
    let type_like = type_like.map(|t| Some(CppEntity::TypeLike(t)));
    let unknown = atoms::unknown_entity.value(None);
    let lambda = atoms::lambda.map(|l| Some(CppEntity::Lambda(l)));
    type_like.or(unknown).or(lambda).parse(s)
}
//
/// C++ entity description
#[derive(Clone, Debug, PartialEq)]
pub enum CppEntity<'source> {
    /// Something that parses like a type
    TypeLike(TypeLike<'source>),

    /// Clang-style lambda name
    Lambda(Lambda<'source>),
}

/// Parser for unqualified id-expressions
fn unqualified_id_expression(s: &str) -> IResult<&str, UnqualifiedId> {
    // FIXME: Accept all unqualified id-expressions. In addition to identifiers,
    //        these include...
    //        - Destructors: ~identifier
    //        - Templates: identifier<param...>
    //        - Operators, including conversion operators
    templates::templatable_id(s)
}
//
pub type UnqualifiedId<'source> = TemplatableId<'source>;

/// Parser for id-expressions
fn id_expression(s: &str) -> IResult<&str, IdExpression> {
    use nom::{bytes::complete::tag, multi::many0};
    let scope = templates::templatable_id.terminated(tag("::"));
    let path = many0(scope).map(Vec::into_boxed_slice);
    (path.and(unqualified_id_expression))
        .map(|(path, id)| IdExpression { path, id })
        .parse(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IdExpression<'source> {
    /// Hierarchical scope (types or namespaces)
    path: Box<[TemplatableId<'source>]>,

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
