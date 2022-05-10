//! C++ entity name parsing

mod atoms;
mod templates;
mod types;

use self::{atoms::Lambda, templates::TemplatableId, types::TypeLike};
use nom::IResult;

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<&str, Option<CppEntity>> {
    use nom::{branch::alt, combinator::map};
    let type_like = |s| types::type_like(s, atoms::end_of_string);
    let type_like = map(type_like, |t| Some(CppEntity::TypeLike(t)));
    let unknown = map(atoms::unknown_entity, |()| None);
    let lambda = map(atoms::lambda, |l| Some(CppEntity::Lambda(l)));
    alt((type_like, unknown, lambda))(s)
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
    use nom::{
        bytes::complete::tag,
        combinator::map,
        multi::many0,
        sequence::{pair, terminated},
    };
    let scope = terminated(templates::templatable_id, tag("::"));
    let path = map(many0(scope), Vec::into_boxed_slice);
    let path_and_id = pair(path, unqualified_id_expression);
    map(path_and_id, |(path, id)| IdExpression { path, id })(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, PartialEq)]
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
