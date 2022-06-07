//! Things that could be templates

use crate::{
    types::{self, TypeLike},
    values::{self, ValueLike},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing a set of template parameters
pub fn template_parameters(s: &str) -> IResult<TemplateParameters> {
    use nom::{
        character::complete::{char, space0},
        multi::separated_list0,
        sequence::delimited,
    };
    use nom_supreme::tag::complete::tag;
    let arguments = separated_list0(space0.and(char(',')).and(space0), template_parameter);
    (delimited(char('<').and(space0), arguments, space0.and(char('>')))
        .map(|v| Some(v.into_boxed_slice())))
    .or(tag("<, void>").value(None))
    .parse(s)
}
//
/// Set of template parameters
///
/// None means that a known invalid template parameter set printout from clang,
/// such as "<, void>", was encountered.
pub type TemplateParameters<'source> = Option<Box<[TemplateParameter<'source>]>>;

/// Parser recognizing a single template parameter/argument
///
/// Must look ahead to the next template parameter separator (, or >) in order
/// to resolve the type vs value ambiguity properly.
///
fn template_parameter(s: &str) -> IResult<TemplateParameter> {
    use nom::{
        character::complete::{char, space0},
        combinator::peek,
    };
    let type_like = types::type_like
        .map(TemplateParameter::TypeLike)
        .terminated(space0.and(peek(char(',').or(char('>')))));
    let value_like = values::value_like::<false, false>
        .map(TemplateParameter::ValueLike)
        .terminated(space0.and(peek(char(',').or(char('>')))));
    type_like.or(value_like).parse(s)
}
//
/// Template parameter
// FIXME: This type appears in Box<[T]>, intern that once data is owned
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TemplateParameter<'source> {
    /// Type or value looking close enough to a type
    TypeLike(TypeLike<'source>),

    /// Value
    ValueLike(ValueLike<'source>),
}
//
impl<'source> From<ValueLike<'source>> for TemplateParameter<'source> {
    fn from(v: ValueLike<'source>) -> Self {
        Self::ValueLike(v)
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
    use super::*;
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;

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
            TemplateParameter::ValueLike(i64::MIN.into()),
        );
        fn test_type_parameter(s: &str) {
            test_template_parameter(s, force_parse(types::type_like, s).into());
        }
        test_type_parameter("signed char*");
        test_type_parameter("charamel<lol>&");
    }

    #[test]
    fn template_parameters() {
        assert_eq!(
            super::template_parameters("<>"),
            Ok(("", Some(vec![].into())))
        );
        assert_eq!(
            super::template_parameters("<T>"),
            Ok((
                "",
                Some(vec![force_parse(types::type_like, "T").into()].into())
            ))
        );
        assert_eq!(
            super::template_parameters("<char, stuff>"),
            Ok((
                "",
                Some(
                    vec![
                        force_parse(types::type_like, "char").into(),
                        force_parse(types::type_like, "stuff").into()
                    ]
                    .into()
                )
            ))
        );
        assert_eq!(super::template_parameters("<, void>"), Ok(("", None)));
    }
}
