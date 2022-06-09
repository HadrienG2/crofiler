//! Things that could be templates

use crate::{names::atoms, types::TypeLike, values::ValueLike, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser for unqualified id-expressions
    pub fn parse_template_parameters<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameters<atoms::IdentifierKey, crate::PathKey>> {
        use nom::{
            character::complete::{char, space0},
            multi::separated_list0,
            sequence::delimited,
        };
        use nom_supreme::tag::complete::tag;
        let arguments = separated_list0(space0.and(char(',')).and(space0), |s| {
            self.parse_template_parameter(s)
        });
        (delimited(char('<').and(space0), arguments, space0.and(char('>')))
            .map(|v| Some(v.into_boxed_slice())))
        .or(tag("<, void>").value(None))
        .parse(s)
    }

    /// Parser recognizing a single template parameter/argument
    ///
    /// Must look ahead to the next template parameter separator (, or >) in order
    /// to resolve the type vs value ambiguity properly.
    ///
    fn parse_template_parameter<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameter<atoms::IdentifierKey, crate::PathKey>> {
        use nom::{
            character::complete::{char, space0},
            combinator::peek,
        };
        let type_like = (|s| self.parse_type_like(s))
            .map(TemplateParameter::TypeLike)
            .terminated(space0.and(peek(char(',').or(char('>')))));
        let value_like = (|s| self.parse_value_like(s, false, false))
            .map(TemplateParameter::ValueLike)
            .terminated(space0.and(peek(char(',').or(char('>')))));
        type_like.or(value_like).parse(s)
    }
}

/// Set of template parameters
///
/// None means that a known invalid template parameter set printout from clang,
/// such as "<, void>", was encountered.
pub type TemplateParameters<IdentifierKey, PathKey> =
    Option<Box<[TemplateParameter<IdentifierKey, PathKey>]>>;

/// Template parameter
// FIXME: This type appears in Box<[T]>, intern that once data is owned
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TemplateParameter<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Type or value looking close enough to a type
    TypeLike(TypeLike<IdentifierKey, PathKey>),

    /// Value
    ValueLike(ValueLike<IdentifierKey, PathKey>),
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > From<ValueLike<IdentifierKey, PathKey>> for TemplateParameter<IdentifierKey, PathKey>
{
    fn from(v: ValueLike<IdentifierKey, PathKey>) -> Self {
        Self::ValueLike(v)
    }
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > From<TypeLike<IdentifierKey, PathKey>> for TemplateParameter<IdentifierKey, PathKey>
{
    fn from(t: TypeLike<IdentifierKey, PathKey>) -> Self {
        Self::TypeLike(t)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    type TestTemplateParameter<'source> = TemplateParameter<atoms::IdentifierKey, crate::PathKey>;

    #[test]
    fn template_parameter() {
        let parser = EntityParser::new();
        let test_template_parameter_sep =
            |text_wo_sep: &str, sep: &str, expected: TestTemplateParameter| {
                let mut text = text_wo_sep.to_owned();
                text.push_str(sep);
                assert_eq!(parser.parse_template_parameter(&text), Ok((sep, expected)));
            };
        let test_template_parameter = |text_wo_sep: &str, expected: TestTemplateParameter| {
            test_template_parameter_sep(text_wo_sep, ",", expected.clone());
            test_template_parameter_sep(text_wo_sep, ">", expected);
        };
        test_template_parameter(
            &(i64::MIN.to_string()),
            TemplateParameter::ValueLike(i64::MIN.into()),
        );
        let test_type_parameter = |s: &str| {
            let type_like = unwrap_parse(parser.parse_type_like(s));
            test_template_parameter(s, type_like.into());
        };
        test_type_parameter("signed char*");
        test_type_parameter("charamel<lol>&");
    }

    #[test]
    fn template_parameters() {
        let parser = EntityParser::new();
        let type_like = |s| unwrap_parse(parser.parse_type_like(s));
        assert_eq!(
            parser.parse_template_parameters("<>"),
            Ok(("", Some(vec![].into())))
        );
        assert_eq!(
            parser.parse_template_parameters("<T>"),
            Ok(("", Some(vec![type_like("T").into()].into())))
        );
        assert_eq!(
            parser.parse_template_parameters("<char, stuff>"),
            Ok((
                "",
                Some(vec![type_like("char").into(), type_like("stuff").into()].into())
            ))
        );
        assert_eq!(parser.parse_template_parameters("<, void>"), Ok(("", None)));
    }
}
