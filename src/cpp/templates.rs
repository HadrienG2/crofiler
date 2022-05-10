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
#[derive(Clone, Default, Debug, PartialEq)]
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
//
impl From<i128> for TemplateParameter<'_> {
    fn from(i: i128) -> Self {
        Self::Integer(i)
    }
}
//
impl<'source> From<TypeLike<'source>> for TemplateParameter<'source> {
    fn from(t: TypeLike<'source>) -> Self {
        Self::TypeLike(t)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::force_parse_type;
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
            test_template_parameter(s, force_parse_type(s).into());
        }
        test_type_parameter("signed char*");
        test_type_parameter("charamel<lol>&");
    }

    #[test]
    fn template_parameters() {
        assert_eq!(
            super::template_parameters("<T>"),
            Ok(("", vec![force_parse_type("T").into()].into()))
        );
        assert_eq!(
            super::template_parameters("<char, stuff>"),
            Ok((
                "",
                vec![
                    force_parse_type("char").into(),
                    force_parse_type("stuff").into()
                ]
                .into()
            ))
        );
    }

    #[test]
    fn templatable_id() {
        assert_eq!(
            super::templatable_id("no_parameters"),
            Ok((
                "",
                TemplatableId {
                    id: "no_parameters",
                    parameters: vec![].into(),
                }
            ))
        );
        assert_eq!(
            super::templatable_id("A<B, C>"),
            Ok((
                "",
                TemplatableId {
                    id: "A",
                    parameters: vec![force_parse_type("B").into(), force_parse_type("C").into()]
                        .into()
                }
            ))
        );
    }
}
