//! C++ entity name parsing

mod atoms;
mod types;

use self::{atoms::Lambda, types::TypeLike};
use nom::IResult;

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<&str, Option<CppEntity>> {
    use nom::{branch::alt, combinator::map};
    let id_expression = map(id_expression, |i| Some(CppEntity::IdExpression(i)));
    let unknown = map(atoms::unknown_entity, |()| None);
    let lambda = map(atoms::lambda, |l| Some(CppEntity::Lambda(l)));
    alt((id_expression, unknown, lambda))(s)
}
//
/// C++ entity description
#[derive(Clone, Debug, PartialEq)]
pub enum CppEntity<'source> {
    IdExpression(IdExpression<'source>),
    Lambda(Lambda<'source>),
}

/// Parser recognizing a single template parameter/argument
fn template_parameter(s: &str) -> IResult<&str, TemplateParameter> {
    use nom::{
        branch::alt,
        character::complete::{char, space0},
        combinator::map,
        sequence::pair,
    };
    let integer_literal = map(atoms::integer_literal, TemplateParameter::Integer);
    fn delimiter(s: &str) -> IResult<&str, ()> {
        map(pair(space0, alt((char(','), char('>')))), std::mem::drop)(s)
    }
    let type_like = |s| types::type_like(s, delimiter);
    let type_like = map(type_like, TemplateParameter::TypeLike);
    alt((integer_literal, type_like))(s)
}
//
/// Template parameter
#[derive(Debug, PartialEq, Clone)]
pub enum TemplateParameter<'source> {
    /// Integer literal
    Integer(i128),

    /// Type or value looking close enough to
    TypeLike(TypeLike<'source>),
}

/// Parser recognizing a set of template parameters
fn template_parameters(s: &str) -> IResult<&str, Box<[TemplateParameter]>> {
    use nom::{
        character::complete::{char, space0},
        combinator::map,
        multi::separated_list1,
        sequence::{delimited, pair},
    };
    let arguments = separated_list1(pair(char(','), space0), template_parameter);
    let parameters = delimited(char('<'), arguments, pair(space0, char('>')));
    map(parameters, |p| p.into_boxed_slice())(s)
}

/// Parser recognizing an identifier which may or may not be coupled with
/// template arguments, i.e. id or id<...>
fn templatable_id(s: &str) -> IResult<&str, TemplatableId> {
    use nom::{
        combinator::{map, opt},
        sequence::pair,
    };
    let parameters_or_empty = map(opt(template_parameters), |opt| opt.unwrap_or_default());
    map(
        pair(atoms::identifier, parameters_or_empty),
        |(id, parameters)| TemplatableId { id, parameters },
    )(s)
}
//
/// Identifier which may or may not have template arguments
#[derive(Clone, Debug, PartialEq)]
pub struct TemplatableId<'source> {
    /// Identifier
    id: &'source str,

    /// Optional template parameters
    parameters: Box<[TemplateParameter<'source>]>,
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

/// Parser for unqualified id-expressions
fn unqualified_id_expression(s: &str) -> IResult<&str, UnqualifiedId> {
    // FIXME: Accept all unqualified id-expressions. In addition to identifiers,
    //        these include...
    //        - Destructors: ~identifier
    //        - Templates: identifier<param...>
    //        - Operators, including conversion operators
    templatable_id(s)
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
    let scope = terminated(templatable_id, tag("::"));
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
