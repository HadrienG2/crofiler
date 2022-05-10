//! C++ entity name parsing

mod atom;

use self::atom::{ConstVolatile, Lambda};
use nom::IResult;

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<&str, Option<CppEntity>> {
    use nom::{branch::alt, combinator::map};
    let id_expression = map(id_expression, |i| Some(CppEntity::IdExpression(i)));
    let unknown = map(atom::unknown_entity, |()| None);
    let lambda = map(atom::lambda, |l| Some(CppEntity::Lambda(l)));
    alt((id_expression, unknown, lambda))(s)
}
//
/// C++ entity description
#[derive(Clone, Debug, PartialEq)]
pub enum CppEntity<'source> {
    IdExpression(IdExpression<'source>),
    Lambda(Lambda<'source>),
}

/// Parser recognizing types (and some values), given an underlying identifier parser
///
/// This wraps either id_expression or legacy_primitive with extra logic
/// for CV qualifiers, pointers and references. Unfortunately, as a result of
/// the C++ grammar being the preposterous monster that it is, we cannot fully
/// decide at this layer of the parsing stack which of the id_expression or
/// legacy_primitive sub-parsers should be called.
///
/// Instead, we must reach the next delimiter character (e.g. ',' or '>' in
/// template parameter lists) before taking this decision.
fn type_like_impl(
    s: &str,
    inner_id: impl Fn(&str) -> IResult<&str, IdExpression>,
) -> IResult<&str, TypeLike> {
    use nom::{
        character::complete::{char, space0, space1},
        combinator::{map, opt},
        multi::{many0, many1_count},
        sequence::{pair, preceded, terminated, tuple},
    };
    let pointer_opt = preceded(pair(space0, char('*')), opt(preceded(space0, atom::cv)));
    let pointer = map(pointer_opt, |cv| cv.unwrap_or_default());
    let pointers = many0(pointer);
    let num_refs_opt = opt(preceded(space1, many1_count(char('&'))));
    let tuple = tuple((
        opt(terminated(atom::cv, space1)),
        inner_id,
        pointers,
        num_refs_opt,
    ));
    map(tuple, |(bottom_cv, bottom_id, pointers, num_refs_opt)| {
        TypeLike {
            bottom_cv: bottom_cv.unwrap_or_default(),
            bottom_id,
            pointers: pointers.into_boxed_slice(),
            num_references: num_refs_opt.unwrap_or_default() as u8,
        }
    })(s)
}

/// Parser recognizing types and some values, given a parser for the next delimiter
///
/// This resolves the type_like_impl ambiguity by checking out the next
/// delimiter, without consuming it.
fn type_like(
    s: &str,
    next_delimiter: impl FnMut(&str) -> IResult<&str, ()> + Copy,
) -> IResult<&str, TypeLike> {
    use nom::{
        branch::alt,
        combinator::{map, peek},
        sequence::terminated,
    };
    let id_expression = |s| type_like_impl(s, id_expression);
    fn legacy_id(s: &str) -> IResult<&str, IdExpression> {
        map(atom::legacy_primitive, IdExpression::from)(s)
    }
    let legacy_id = |s| type_like_impl(s, legacy_id);
    alt((
        terminated(id_expression, peek(next_delimiter)),
        terminated(legacy_id, peek(next_delimiter)),
    ))(s)
}

/// Output from type_like* parsers
#[derive(Debug, PartialEq, Clone)]
pub struct TypeLike<'source> {
    /// CV qualifiers applying to the leftmost type
    bottom_cv: ConstVolatile,

    /// Leftmost identifier (type or some values like boolean true/false)
    bottom_id: IdExpression<'source>,

    /// Layers of pointer indirection (* const * volatile...)
    pointers: Box<[ConstVolatile]>,

    /// Number of final references between 0 (no reference) and 2 (rvalue)
    num_references: u8,
}

/// Parser recognizing template arguments
fn template_argument(s: &str) -> IResult<&str, TemplateArgument> {
    use nom::{
        branch::alt,
        character::complete::{char, space0},
        combinator::map,
        sequence::pair,
    };
    let integer_literal = map(atom::integer_literal, TemplateArgument::Integer);
    fn delimiter(s: &str) -> IResult<&str, ()> {
        map(pair(space0, alt((char(','), char('>')))), std::mem::drop)(s)
    }
    let type_like = |s| type_like(s, delimiter);
    let type_like = map(type_like, TemplateArgument::TypeLike);
    alt((integer_literal, type_like))(s)
}
//
/// Template argument
#[derive(Debug, PartialEq, Clone)]
pub enum TemplateArgument<'source> {
    /// Integer literal
    Integer(i128),

    /// Type or value looking close enough to
    TypeLike(TypeLike<'source>),
}

/// Parser recognizing template parameters
fn template_parameters(s: &str) -> IResult<&str, Box<[TemplateArgument]>> {
    use nom::{
        character::complete::{char, space0},
        combinator::map,
        multi::separated_list1,
        sequence::{delimited, pair},
    };
    let arguments = separated_list1(pair(char(','), space0), template_argument);
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
        pair(atom::identifier, parameters_or_empty),
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
    parameters: Box<[TemplateArgument<'source>]>,
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

// FIXME: Add tests
