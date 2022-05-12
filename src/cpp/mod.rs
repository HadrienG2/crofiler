//! C++ entity name parsing

mod atoms;
mod functions;
mod templates;
mod types;
mod values;

use nom::Parser;
use nom_supreme::ParserExt;

// TODO: Keep this up to date over time
pub use self::{
    atoms::Lambda,
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
pub fn entity(s: &str) -> IResult<Option<CppEntity>> {
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
fn unqualified_id_expression(s: &str) -> IResult<UnqualifiedId> {
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
fn id_expression(s: &str) -> IResult<IdExpression> {
    use nom::multi::many0;
    use nom_supreme::tag::complete::tag;
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
