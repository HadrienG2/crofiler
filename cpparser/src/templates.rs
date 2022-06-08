//! Things that could be templates

use crate::{
    names::atoms,
    types::{self, TypeLike},
    values::{self, ValueLike},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser for unqualified id-expressions
    pub fn parse_template_parameters<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameters<atoms::IdentifierKey, crate::PathKey>> {
        template_parameters(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parser recognizing a set of template parameters
// TODO: Make private once clients have been migrated
pub fn template_parameters<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, TemplateParameters<IdentifierKey, PathKey>> {
    use nom::{
        character::complete::{char, space0},
        multi::separated_list0,
        sequence::delimited,
    };
    use nom_supreme::tag::complete::tag;
    let arguments = separated_list0(space0.and(char(',')).and(space0), |s| {
        template_parameter(s, parse_identifier, path_to_key)
    });
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
pub type TemplateParameters<IdentifierKey, PathKey> =
    Option<Box<[TemplateParameter<IdentifierKey, PathKey>]>>;

/// Parser recognizing a single template parameter/argument
///
/// Must look ahead to the next template parameter separator (, or >) in order
/// to resolve the type vs value ambiguity properly.
///
fn template_parameter<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, TemplateParameter<IdentifierKey, PathKey>> {
    use nom::{
        character::complete::{char, space0},
        combinator::peek,
    };
    let type_like = (|s| types::type_like(s, parse_identifier, path_to_key))
        .map(TemplateParameter::TypeLike)
        .terminated(space0.and(peek(char(',').or(char('>')))));
    let value_like = (|s| values::value_like(s, parse_identifier, path_to_key, false, false))
        .map(TemplateParameter::ValueLike)
        .terminated(space0.and(peek(char(',').or(char('>')))));
    type_like.or(value_like).parse(s)
}
//
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
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    type TestTemplateParameter<'source> = TemplateParameter<&'source str, &'source Path>;

    #[test]
    fn template_parameter() {
        fn test_template_parameter_sep(
            text_wo_sep: &str,
            sep: &str,
            expected: TestTemplateParameter,
        ) {
            let mut text = text_wo_sep.to_owned();
            text.push_str(sep);
            assert_eq!(
                super::template_parameter(&text, &atoms::identifier, &Path::new),
                Ok((sep, expected))
            );
        }
        fn test_template_parameter(text_wo_sep: &str, expected: TestTemplateParameter) {
            test_template_parameter_sep(text_wo_sep, ",", expected.clone());
            test_template_parameter_sep(text_wo_sep, ">", expected);
        }
        test_template_parameter(
            &(i64::MIN.to_string()),
            TemplateParameter::ValueLike(i64::MIN.into()),
        );
        fn test_type_parameter(s: &str) {
            let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
            test_template_parameter(s, force_parse(parse_type_like, s).into());
        }
        test_type_parameter("signed char*");
        test_type_parameter("charamel<lol>&");
    }

    #[test]
    fn template_parameters() {
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        let parse_template_parameters =
            |s| super::template_parameters(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_template_parameters("<>"),
            Ok(("", Some(vec![].into())))
        );
        assert_eq!(
            parse_template_parameters("<T>"),
            Ok((
                "",
                Some(vec![force_parse(parse_type_like, "T").into()].into())
            ))
        );
        assert_eq!(
            parse_template_parameters("<char, stuff>"),
            Ok((
                "",
                Some(
                    vec![
                        force_parse(parse_type_like, "char").into(),
                        force_parse(parse_type_like, "stuff").into()
                    ]
                    .into()
                )
            ))
        );
        assert_eq!(parse_template_parameters("<, void>"), Ok(("", None)));
    }
}
