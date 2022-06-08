//! Operator-related grammar that is only used when declaring overloads

use super::Operator;
use crate::{
    names::atoms,
    templates::{self, TemplateParameters},
    types::{self},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parse any supported operator overload
    ///
    /// The following template parameter set must be parsed in the same go in order
    /// to handle the syntaxically ambiguous nature of the < and > signs.
    ///
    pub fn parse_operator_overload<'source>(
        &self,
        s: &'source str,
    ) -> IResult<
        'source,
        (
            Operator<'source, atoms::IdentifierKey, crate::PathKey>,
            Option<TemplateParameters<'source>>,
        ),
    > {
        operator_overload(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parse any supported operator overload
///
/// See EntityParser::parse_operator_overload for semantics
///
// TODO: Make private once clients have been migrated
pub fn operator_overload<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<
    'source,
    (
        Operator<'source, IdentifierKey, PathKey>,
        Option<TemplateParameters<'source>>,
    ),
> {
    use nom::{character::complete::char, combinator::opt, sequence::preceded};

    // Try arithmetic operators of increasing length until hopefully finding one
    // that matches optimally.
    let arith_and_templates = arith_and_templates::<1, IdentifierKey, PathKey>
        .or(arith_and_templates::<2, IdentifierKey, PathKey>)
        .or(arith_and_templates::<3, IdentifierKey, PathKey>);

    // The other operator parses don't care about template parameters
    let template_oblivious = (call_or_index.or(|s| custom_literal(s, parse_identifier)))
        .or(preceded(
            char(' '),
            new.or(super::delete)
                .or(super::co_await)
                // Must come last as it matches keywords
                .or((|s| types::type_like(s, parse_identifier, path_to_key))
                    .map(|ty| Operator::Conversion(Box::new(ty)))),
        ))
        .and(opt(templates::template_parameters));

    // And for an operator overload, we need the operator keyword...
    preceded(
        EntityParser::keyword_parser("operator"),
        arith_and_templates.or(template_oblivious),
    )
    .parse(s)
}

/// Try to parse input as an arithmetic or comparison operator name, optionally
/// followed by a set of template parameters.
///
/// Reject the parse if there are operator-like symbols coming up next in the
/// stream, as it strongly suggests that the entirety of the operator name was
/// not parsed and the parse must be retried at a greater LEN.
///
fn arith_and_templates<
    const LEN: usize,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
>(
    s: &str,
) -> IResult<(Operator<IdentifierKey, PathKey>, Option<TemplateParameters>)> {
    use nom::{
        combinator::{map_opt, opt, peek},
        sequence::tuple,
    };
    map_opt(
        tuple((
            super::arithmetic_or_comparison::<LEN, IdentifierKey, PathKey>,
            opt(templates::template_parameters),
            peek(opt(super::symbol)),
        )),
        |(operator, parameters_opt, symbol)| {
            if symbol.is_none() {
                Some((operator, parameters_opt))
            } else {
                None
            }
        },
    )(s)
}

/// Parse bracket pair operators: calling and array indexing
fn call_or_index<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
>(
    s: &str,
) -> IResult<Operator<IdentifierKey, PathKey>> {
    use nom_supreme::tag::complete::tag;
    (tag("()").value(false).or(tag("[]").value(true)))
        .map(|is_index| Operator::CallIndex { is_index })
        .parse(s)
}

/// Parse custom literal
fn custom_literal<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
) -> IResult<'source, Operator<'source, IdentifierKey, PathKey>> {
    use nom::{character::complete::space0, sequence::preceded};
    use nom_supreme::tag::complete::tag;
    preceded(tag("\"\"").and(space0), parse_identifier)
        .map(Operator::CustomLiteral)
        .parse(s)
}

/// Parse allocation function overload declaration
fn new<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
>(
    s: &str,
) -> IResult<Operator<IdentifierKey, PathKey>> {
    use nom::{combinator::opt, sequence::preceded};
    use nom_supreme::tag::complete::tag;
    preceded(EntityParser::keyword_parser("new"), opt(tag("[]")))
        .map(|array| Operator::NewDelete {
            is_delete: false,
            array: array.is_some(),
        })
        .parse(s)
}

#[cfg(test)]
mod tests {
    use super::super::Symbol;
    use super::*;
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn call_or_index() {
        let parse_call_or_index = super::call_or_index::<&str, &Path>;
        assert_eq!(
            parse_call_or_index("()"),
            Ok(("", Operator::CallIndex { is_index: false }))
        );
        assert_eq!(
            parse_call_or_index("[]"),
            Ok(("", Operator::CallIndex { is_index: true }))
        );
    }

    fn parse_custom_literal(s: &str) -> IResult<Operator<&str, &Path>> {
        super::custom_literal(s, &atoms::identifier)
    }

    #[test]
    fn custom_literal() {
        assert_eq!(
            parse_custom_literal("\"\" _whatever"),
            Ok(("", Operator::CustomLiteral("_whatever")))
        );
    }

    #[test]
    fn new() {
        let parse_new = super::new::<&str, &Path>;
        assert_eq!(
            parse_new("new"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: false,
                    array: false
                }
            ))
        );
        assert_eq!(
            parse_new("new[]"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: false,
                    array: true
                }
            ))
        );
    }

    #[test]
    fn operator_overload() {
        let parse_operator_overload =
            |s| super::operator_overload(s, &atoms::identifier, &Path::new);

        // Symbol-based operators don't need spaces
        assert_eq!(
            parse_operator_overload("operator*="),
            Ok((
                "",
                (
                    Operator::Basic {
                        symbol: Symbol::MulDeref,
                        twice: false,
                        equal: true
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parse_operator_overload("operator[]"),
            Ok(("", (Operator::CallIndex { is_index: true }, None)))
        );
        assert_eq!(
            parse_operator_overload("operator\"\" _stuff"),
            Ok(("", (Operator::CustomLiteral("_stuff"), None)))
        );

        // Keyword-based operators need spaces
        assert_eq!(
            parse_operator_overload("operator new[]"),
            Ok((
                "",
                (
                    Operator::NewDelete {
                        is_delete: false,
                        array: true
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parse_operator_overload("operator co_await"),
            Ok(("", (Operator::CoAwait, None)))
        );

        // Type conversion operator works
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_operator_overload("operator unsigned long long"),
            Ok((
                "",
                (
                    Operator::Conversion(Box::new(force_parse(
                        parse_type_like,
                        "unsigned long long"
                    ))),
                    None
                )
            ))
        );

        // Ambiguities between template and operator syntax are handled well
        assert_eq!(
            parse_operator_overload("operator<<>"),
            Ok((
                "",
                (
                    Operator::Basic {
                        symbol: Symbol::Less,
                        twice: false,
                        equal: false,
                    },
                    Some(Some(Default::default()))
                )
            ))
        );
        assert_eq!(
            parse_operator_overload("operator<<void>"),
            Ok((
                "",
                (
                    Operator::Basic {
                        symbol: Symbol::Less,
                        twice: false,
                        equal: false,
                    },
                    Some(Some(
                        vec![force_parse(parse_type_like, "void").into()].into()
                    ))
                )
            ))
        );
    }
}
