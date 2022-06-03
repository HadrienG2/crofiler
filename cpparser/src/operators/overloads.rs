//! Operator-related grammar that is only used when declaring overloads

use super::Operator;
use crate::{
    names::atoms,
    templates::{self, TemplateParameters},
    types::{self},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parse any supported operator overload
///
/// The following template parameter set must be parsed in the same go in order
/// to handle the syntaxically ambiguous nature of < and >.
pub fn operator_overload(s: &str) -> IResult<(Operator, Option<TemplateParameters>)> {
    use nom::{character::complete::char, combinator::opt, sequence::preceded};

    // Try arithmetic operators of increasing length until hopefully finding one
    // that matches optimally.
    let arith_and_templates = arith_and_templates::<1>
        .or(arith_and_templates::<2>)
        .or(arith_and_templates::<3>);

    // The other operator parses don't care about template parameters
    let template_oblivious = (call_or_index.or(custom_literal))
        .or(preceded(
            char(' '),
            new.or(super::delete)
                .or(super::co_await)
                // Must come last as it matches keywords
                .or(types::type_like.map(|ty| Operator::Conversion(Box::new(ty)))),
        ))
        .and(opt(templates::template_parameters));

    // And for an operator overload, we need the operator keyword...
    preceded(
        atoms::keyword("operator"),
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
fn arith_and_templates<const LEN: usize>(
    s: &str,
) -> IResult<(Operator, Option<TemplateParameters>)> {
    use nom::{
        combinator::{map_opt, opt, peek},
        sequence::tuple,
    };
    map_opt(
        tuple((
            super::arithmetic_or_comparison::<LEN>,
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
fn call_or_index(s: &str) -> IResult<Operator> {
    use nom_supreme::tag::complete::tag;
    (tag("()").value(false).or(tag("[]").value(true)))
        .map(|is_index| Operator::CallIndex { is_index })
        .parse(s)
}

/// Parse custom literal
fn custom_literal(s: &str) -> IResult<Operator> {
    use nom::{character::complete::space0, sequence::preceded};
    use nom_supreme::tag::complete::tag;
    preceded(tag("\"\"").and(space0), atoms::identifier)
        .map(Operator::CustomLiteral)
        .parse(s)
}

/// Parse allocation function overload declaration
fn new(s: &str) -> IResult<Operator> {
    use nom::{combinator::opt, sequence::preceded};
    use nom_supreme::tag::complete::tag;
    preceded(atoms::keyword("new"), opt(tag("[]")))
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

    #[test]
    fn call_or_index() {
        assert_eq!(
            super::call_or_index("()"),
            Ok(("", Operator::CallIndex { is_index: false }))
        );
        assert_eq!(
            super::call_or_index("[]"),
            Ok(("", Operator::CallIndex { is_index: true }))
        );
    }

    #[test]
    fn custom_literal() {
        assert_eq!(
            super::custom_literal("\"\" _whatever"),
            Ok(("", Operator::CustomLiteral("_whatever")))
        );
    }

    #[test]
    fn new() {
        assert_eq!(
            super::new("new"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: false,
                    array: false
                }
            ))
        );
        assert_eq!(
            super::new("new[]"),
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
        // Symbol-based operators don't need spaces
        assert_eq!(
            super::operator_overload("operator*="),
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
            super::operator_overload("operator[]"),
            Ok(("", (Operator::CallIndex { is_index: true }, None)))
        );
        assert_eq!(
            super::operator_overload("operator\"\" _stuff"),
            Ok(("", (Operator::CustomLiteral("_stuff"), None)))
        );

        // Keyword-based operators need spaces
        assert_eq!(
            super::operator_overload("operator new[]"),
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
            super::operator_overload("operator co_await"),
            Ok(("", (Operator::CoAwait, None)))
        );

        // Type conversion operator works
        assert_eq!(
            super::operator_overload("operator unsigned long long"),
            Ok((
                "",
                (
                    Operator::Conversion(Box::new(force_parse(
                        types::type_like,
                        "unsigned long long"
                    ))),
                    None
                )
            ))
        );

        // Ambiguities between template and operator syntax are handled well
        assert_eq!(
            super::operator_overload("operator<<>"),
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
            super::operator_overload("operator<<void>"),
            Ok((
                "",
                (
                    Operator::Basic {
                        symbol: Symbol::Less,
                        twice: false,
                        equal: false,
                    },
                    Some(Some(
                        vec![force_parse(types::type_like, "void").into()].into()
                    ))
                )
            ))
        );
    }
}
