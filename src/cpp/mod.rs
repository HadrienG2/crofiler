//! C++ entity name parsing

// FIXME: Remove pub once done debugging
pub mod anonymous;
pub mod atoms;
pub mod functions;
pub mod names;
pub mod operators;
pub mod templates;
pub mod types;
pub mod values;

use nom::Parser;
use nom_supreme::ParserExt;

// TODO: Keep this up to date over time
pub use self::{
    anonymous::{AnonymousEntity, Lambda},
    functions::FunctionSignature,
    names::{IdExpression, NestedNameSpecifier, Scope, UnqualifiedId},
    templates::TemplateParameter,
    types::{
        qualifiers::{ConstVolatile, Reference},
        TypeLike,
    },
    values::{
        literals::{Literal, LiteralValue},
        ValueLike,
    },
};

// FIXME: Remove once done debugging
//#[cfg(test)]
pub type Error<I> = nom::error::Error<I>;
/*#[cfg(not(test))]
type Error<I> = nom_supreme::error::ErrorTree<I>;*/
pub type IResult<'a, O> = nom::IResult<&'a str, O, Error<&'a str>>;

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<Option<TypeLike>> {
    let type_like = types::type_like.map(Some);
    let unknown = anonymous::unknown_entity.value(None);
    type_like.or(unknown).parse(s)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    pub fn force_parse_type(s: &str) -> TypeLike {
        types::type_like(s).unwrap().1
    }

    #[test]
    fn entity() {
        // Something that looks like a tyme name
        assert_eq!(
            super::entity("type_name"),
            Ok(("", Some(force_parse_type("type_name"))))
        );

        // The infamous unknown clang entity
        assert_eq!(super::entity("<unknown>"), Ok(("", None)));
    }
}
