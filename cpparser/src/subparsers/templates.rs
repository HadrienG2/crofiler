//! Things that could be templates

use crate::{
    display::{CustomDisplay, DisplayState},
    interning::slice::{SliceItemView, SliceView},
    subparsers::{
        types::{TypeKey, TypeView},
        values::{ValueKey, ValueView},
    },
    EntityParser, IResult,
};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

#[cfg(test)]
use reffers::ARef;

/// Interned template parameter set key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// template parameter sets as long as both keys were produced by the same
/// EntityParser.
///
/// After parsing, you can retrieve a template parameter set by passing this key
/// to the template_parameters() method of EntityParser.
///
pub type TemplateParameterListKey =
    SequenceKey<TemplateParameterListKeyImpl, TEMPLATE_PARAMETER_LIST_LEN_BITS>;
type TemplateParameterListKeyImpl = Spur;
const TEMPLATE_PARAMETER_LIST_LEN_BITS: u32 = 10;
//
impl EntityParser {
    /// Parser for unqualified id-expressions
    pub fn parse_template_parameters<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameters> {
        self.parse_template_parameters_imut(s)
    }

    /// Implementation of parse_template_parameters using internal mutability
    pub(crate) fn parse_template_parameters_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameters> {
        use nom::{
            character::complete::{char, multispace0},
            sequence::preceded,
        };
        use nom_supreme::{multi::parse_separated_terminated, tag::complete::tag};

        let arguments_header = char('<').and(multispace0);

        let non_empty_arguments = parse_separated_terminated(
            |s| self.parse_template_parameter_imut(s),
            multispace0.and(char(',')).and(multispace0),
            multispace0.and(char('>')),
            || self.template_parameter_lists.entry(),
            |mut entry, item| {
                entry.push(item);
                entry
            },
        )
        .map(|entry| entry.intern());

        let empty_arguments = char('>').map(|_| self.template_parameter_lists.entry().intern());

        let invalid_arguments = tag(", void>");

        preceded(
            arguments_header,
            ((non_empty_arguments.or(empty_arguments)).map(Some)).or(invalid_arguments.value(None)),
        )
        .parse(s)
    }

    /// Retrieve a previously interned template parameter set
    pub fn template_parameters(&self, tp: TemplateParameters) -> TemplateParametersView {
        TemplateParametersView::new(tp, self)
    }

    /// Retrieve a previously interned template parameter set
    #[cfg(test)]
    pub(crate) fn raw_template_parameters(
        &self,
        key: TemplateParameterListKey,
    ) -> ARef<[TemplateParameter]> {
        self.template_parameter_lists.get(key)
    }

    /// Total number of template parameters across all interned template parameter sets so far
    pub fn num_template_parameters(&self) -> usize {
        self.template_parameter_lists.borrow().num_items()
    }

    /// Maximal number of template parameters
    pub fn max_template_parameter_set_len(&self) -> Option<usize> {
        self.template_parameter_lists.borrow().max_sequence_len()
    }

    /// Retrieve a previously interned template parameter list (excludes `<, void>`)
    pub(crate) fn template_parameter_list(
        &self,
        key: TemplateParameterListKey,
    ) -> TemplateParameterListView {
        TemplateParameterListView::new(key, self.template_parameter_lists.borrow(), self)
    }

    /// Parser recognizing a single template parameter/argument
    ///
    /// Must look ahead to the next template parameter separator (, or `>`) in
    /// order to resolve the type vs value ambiguity properly.
    ///
    fn parse_template_parameter_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TemplateParameter> {
        use nom::{
            character::complete::{char, multispace0},
            combinator::peek,
        };
        let type_like = (|s| self.parse_type_like_imut(s))
            .map(TemplateParameter::TypeLike)
            .terminated(multispace0.and(peek(char(',').or(char('>')))));
        let value_like = (|s| self.parse_value_like_imut(s, false, false))
            .map(TemplateParameter::ValueLike)
            .terminated(multispace0.and(peek(char(',').or(char('>')))));
        type_like.or(value_like).parse(s)
    }

    /// Retrieve a previously interned template parameter
    #[cfg(test)]
    fn template_parameter(&self, tp: TemplateParameter) -> TemplateParameterView {
        TemplateParameterView::new(tp, self)
    }
}

/// Set of template parameters
///
/// None means that a known invalid template parameter set printout from clang,
/// such as `<, void>`, was encountered.
///
pub type TemplateParameters = Option<TemplateParameterListKey>;

/// View of a set of template parameters
///
/// None means that a known invalid template parameter set printout from clang,
/// such as `<, void>`, was encountered.
///
#[derive(PartialEq)]
pub struct TemplateParametersView<'entities>(pub Option<TemplateParameterListView<'entities>>);
//
impl<'entities> TemplateParametersView<'entities> {
    /// Build a new-expression view
    pub fn new(inner: TemplateParameters, entities: &'entities EntityParser) -> Self {
        Self(inner.map(|list| entities.template_parameter_list(list)))
    }
}
//
impl Display for TemplateParametersView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for TemplateParametersView<'_> {
    fn recursion_depth(&self) -> usize {
        if let Some(list) = &self.0 {
            list.recursion_depth()
        } else {
            1
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if let Some(list) = &self.0 {
            list.display_impl(f, state)
        } else if let Ok(_guard) = state.recurse() {
            write!(f, "<, void>")
        } else {
            write!(f, "<…>")
        }
    }
}

/// View of a set of template parameters, excluding clang's `<, void>` edge case
pub type TemplateParameterListView<'entities> = SliceView<
    'entities,
    TemplateParameter,
    TemplateParameterView<'entities>,
    TemplateParameterListKey,
>;

/// Template parameter
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TemplateParameter {
    /// Type or value looking close enough to a type
    TypeLike(TypeKey),

    /// Value
    ValueLike(ValueKey),
}

/// View of a template parameter
#[derive(PartialEq)]
pub enum TemplateParameterView<'entities> {
    /// Type or value looking close enough to a type
    TypeLike(TypeView<'entities>),

    /// Value
    ValueLike(ValueView<'entities>),
}
//
impl<'entities> TemplateParameterView<'entities> {
    /// Set up a simple type specifier view
    pub(crate) fn new(inner: TemplateParameter, entities: &'entities EntityParser) -> Self {
        match inner {
            TemplateParameter::TypeLike(t) => Self::TypeLike(entities.type_like(t)),
            TemplateParameter::ValueLike(v) => Self::ValueLike(entities.value_like(v)),
        }
    }
}
//
impl Display for TemplateParameterView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for TemplateParameterView<'_> {
    fn recursion_depth(&self) -> usize {
        match self {
            Self::TypeLike(t) => t.recursion_depth(),
            Self::ValueLike(v) => v.recursion_depth(),
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            Self::TypeLike(t) => t.display_impl(f, state),
            Self::ValueLike(v) => v.display_impl(f, state),
        }
    }
}
//
impl<'entities> SliceItemView<'entities> for TemplateParameterView<'entities> {
    type Inner = TemplateParameter;

    fn new(inner: Self::Inner, entities: &'entities EntityParser) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "<";

    const DISPLAY_SEPARATOR: &'static str = ", ";

    const DISPLAY_TRAILER: &'static str = ">";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn template_parameter() {
        let mut parser = EntityParser::new();
        let test_template_parameter_sep =
            |parser: &mut EntityParser,
             text_wo_sep: &str,
             sep: &str,
             expected: TemplateParameter,
             displays: &[&str]| {
                let mut text = text_wo_sep.to_owned();
                text.push_str(sep);
                assert_eq!(
                    parser.parse_template_parameter_imut(&text),
                    Ok((sep, expected))
                );
                check_custom_display(parser.template_parameter(expected), displays);
            };
        let test_template_parameter = |parser: &mut EntityParser,
                                       text_wo_sep: &str,
                                       expected: TemplateParameter,
                                       displays: &[&str]| {
            test_template_parameter_sep(parser, text_wo_sep, ",", expected, displays);
            test_template_parameter_sep(parser, text_wo_sep, ">", expected, displays);
        };

        let value_str = "-1000000000000";
        let value_like = TemplateParameter::ValueLike(unwrap_parse(
            parser.parse_value_like(value_str, true, true),
        ));
        test_template_parameter(&mut parser, value_str, value_like, &["-1000000000000"]);

        let test_type_parameter = |parser: &mut EntityParser, s: &str, displays: &[&str]| {
            let type_like = unwrap_parse(parser.parse_type_like(s));
            test_template_parameter(parser, s, TemplateParameter::TypeLike(type_like), displays);
        };
        test_type_parameter(
            &mut parser,
            "signed char*",
            &["signed char…", "signed char*"],
        );
        test_type_parameter(
            &mut parser,
            "charamel<lol>&",
            &["charamel<…>…", "charamel<lol>&"],
        );
    }

    #[test]
    fn template_parameters() {
        let mut parser = EntityParser::new();
        let mut test_case = |input: &str, expected_types: Option<&[&str]>, displays: &[&str]| {
            if let Some(expected_types) = expected_types {
                assert_matches!(parser.parse_template_parameters(input), Ok(("", Some(key))) => {
                    let parameters = parser.raw_template_parameters(key).to_owned();
                    assert_eq!(parameters.len(), expected_types.len());
                    for (expected, actual) in expected_types.iter().zip(parameters.to_vec()) {
                        let expected = TemplateParameter::TypeLike(unwrap_parse(parser.parse_type_like(expected)));
                        assert_eq!(expected, actual);
                    }
                    check_custom_display(parser.template_parameters(Some(key)), displays);
                })
            } else {
                assert_eq!(parser.parse_template_parameters(input), Ok(("", None)));
                check_custom_display(parser.template_parameters(None), displays)
            }
        };
        test_case("<>", Some(&[]), &["<>"]);
        test_case("<T>", Some(&["T"]), &["<…>", "<T>"]);
        test_case(
            "<char, stuff>",
            Some(&["char", "stuff"]),
            &["<…>", "<char, stuff>"],
        );
        test_case("<, void>", None, &["<…>", "<, void>"]);
    }
}
