//! Things that could be templates

use crate::{types::TypeKey, values::ValueKey, Entities, EntityParser, IResult};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

/// Interned template parameter set key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// template parameter sets as long as both keys were produced by the same
/// EntityParser.
///
/// After parsing, you can retrieve a template parameter set by passing this key
/// to the template_parameters() method of the Entities struct.
///
// TODO: Adjust key size based on observed entry count
pub type TemplateParametersKey =
    SequenceKey<TemplateParametersKeyImpl, TEMPLATE_PARAMETERS_LEN_BITS>;
pub(crate) type TemplateParametersKeyImpl = Spur;
pub(crate) const TEMPLATE_PARAMETERS_LEN_BITS: u32 = 8;
//
impl EntityParser {
    /// Parser for unqualified id-expressions
    pub fn parse_template_parameters<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameters> {
        use nom::{
            character::complete::{char, space0},
            sequence::preceded,
        };
        use nom_supreme::{multi::parse_separated_terminated, tag::complete::tag};

        let arguments_header = char('<').and(space0);

        let non_empty_arguments = parse_separated_terminated(
            |s| self.parse_template_parameter(s),
            space0.and(char(',')).and(space0),
            space0.and(char('>')),
            || self.template_parameter_sets.entry(),
            |mut entry, item| {
                entry.push(item);
                entry
            },
        )
        .map(|entry| entry.intern());

        let empty_arguments = char('>').map(|_| self.template_parameter_sets.entry().intern());

        let invalid_arguments = tag(", void>");

        preceded(
            arguments_header,
            ((non_empty_arguments.or(empty_arguments)).map(Some)).or(invalid_arguments.value(None)),
        )
        .parse(s)
    }

    /// Retrieve a previously interned template parameter set
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    pub(crate) fn template_parameters(
        &self,
        key: TemplateParametersKey,
    ) -> Box<[TemplateParameter]> {
        self.template_parameter_sets.borrow().get(key).into()
    }

    /// Total number of template parameters across all interned template parameter sets so far
    pub fn num_template_parameters(&self) -> usize {
        self.template_parameter_sets.borrow().num_items()
    }

    /// Maximal number of template parameters
    pub fn max_template_parameter_set_len(&self) -> Option<usize> {
        self.template_parameter_sets.borrow().max_sequence_len()
    }

    /// Parser recognizing a single template parameter/argument
    ///
    /// Must look ahead to the next template parameter separator (, or >) in order
    /// to resolve the type vs value ambiguity properly.
    ///
    fn parse_template_parameter<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameter> {
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
//
impl Entities {
    /// Retrieve a previously interned template parameter set
    pub fn template_parameters(&self, key: TemplateParametersKey) -> &[TemplateParameter] {
        self.template_parameter_sets.get(key)
    }
}

/// Set of template parameters
///
/// None means that a known invalid template parameter set printout from clang,
/// such as "<, void>", was encountered.
///
pub type TemplateParameters = Option<TemplateParametersKey>;

/// Template parameter
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TemplateParameter {
    /// Type or value looking close enough to a type
    TypeLike(TypeKey),

    /// Value
    ValueLike(ValueKey),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn template_parameter() {
        let parser = EntityParser::new();
        let test_template_parameter_sep =
            |text_wo_sep: &str, sep: &str, expected: TemplateParameter| {
                let mut text = text_wo_sep.to_owned();
                text.push_str(sep);
                assert_eq!(parser.parse_template_parameter(&text), Ok((sep, expected)));
            };
        let test_template_parameter = |text_wo_sep: &str, expected: TemplateParameter| {
            test_template_parameter_sep(text_wo_sep, ",", expected.clone());
            test_template_parameter_sep(text_wo_sep, ">", expected);
        };

        let value_like = |s| unwrap_parse(parser.parse_value_like(s, true, true));
        let value_str = &(i64::MIN.to_string());
        test_template_parameter(value_str, value_like(value_str).into());

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
