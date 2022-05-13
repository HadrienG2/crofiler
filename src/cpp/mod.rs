//! C++ entity name parsing

mod anonymous;
mod atoms;
mod functions;
mod id_expressions;
mod templates;
mod types;
mod values;

use nom::Parser;
use nom_supreme::ParserExt;

// TODO: Keep this up to date over time
pub use self::{
    anonymous::{AnonymousEntity, Lambda},
    functions::FunctionSignature,
    id_expressions::{IdExpression, Scope, UnqualifiedId},
    templates::TemplateParameter,
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

// FIXME: Modularize and add tests
#[cfg(test)]
pub mod tests {
    use super::*;

    pub fn force_parse_type(s: &str) -> TypeLike {
        types::type_like(s, atoms::end_of_string).unwrap().1
    }
}
