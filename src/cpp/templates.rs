//! Things that could be templates

use super::{
    atoms,
    types::{self, TypeLike},
};
use nom::IResult;

/// Parser recognizing an identifier which may or may not be coupled with
/// template arguments, i.e. id or id<...>
pub fn templatable_id(s: &str) -> IResult<&str, TemplatableId> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn template_parameter() {
        fn test_template_parameter_sep(text_wo_sep: &str, sep: &str, expected: TemplateParameter) {
            let mut text = text_wo_sep.to_owned();
            text.push_str(sep);
            assert_eq!(super::template_parameter(&text), Ok((sep, expected)));
        }
        fn test_template_parameter(text_wo_sep: &str, expected: TemplateParameter) {
            test_template_parameter_sep(text_wo_sep, ",", expected.clone());
            test_template_parameter_sep(text_wo_sep, ">", expected);
        }
        test_template_parameter(
            &(i64::MIN.to_string()),
            TemplateParameter::Integer(i64::MIN as _),
        );
        fn test_type_parameter(s: &str) {
            test_template_parameter(
                s,
                TemplateParameter::TypeLike(types::type_like(s, atoms::end_of_string).unwrap().1),
            );
        }
        test_type_parameter("signed char*");
        test_type_parameter("char_traits<lol>*");
    }
}
