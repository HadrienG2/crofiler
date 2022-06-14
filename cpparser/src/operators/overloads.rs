//! Operator-related grammar that is only used when declaring overloads

use super::Operator;
use crate::{templates::TemplateParameters, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

impl EntityParser {
    /// Parse any supported operator overload
    ///
    /// The following template parameter set must be parsed in the same go in order
    /// to handle the syntaxically ambiguous nature of the `<` and `>` signs.
    ///
    pub fn parse_operator_overload<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, (Operator, Option<TemplateParameters>)> {
        use nom::{character::complete::char, combinator::opt, sequence::preceded};

        // Try arithmetic operators of increasing length until hopefully finding one
        // that matches optimally.
        let arith_and_templates = (|s| self.parse_arith_and_templates(s, 1))
            .or(|s| self.parse_arith_and_templates(s, 2))
            .or(|s| self.parse_arith_and_templates(s, 3));

        // The other operator parses don't care about template parameters
        let template_oblivious = (call_or_index.or(|s| self.parse_custom_literal(s)))
            .or(preceded(
                char(' '),
                new.or(super::delete)
                    .or(super::co_await)
                    // Must come last as it matches keywords
                    .or((|s| self.parse_type_like(s)).map(Operator::Conversion)),
            ))
            .and(opt(|s| self.parse_template_parameters(s)));

        // And for an operator overload, we need the operator keyword...
        preceded(
            Self::keyword_parser("operator"),
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
    #[inline(always)]
    fn parse_arith_and_templates<'source>(
        &self,
        s: &'source str,
        len: usize,
    ) -> IResult<'source, (Operator, Option<TemplateParameters>)> {
        use nom::{
            combinator::{map_opt, opt, peek},
            sequence::tuple,
        };
        let arithmetic_or_comparison = match len {
            1 => super::arithmetic_or_comparison::<1>,
            2 => super::arithmetic_or_comparison::<2>,
            3 => super::arithmetic_or_comparison::<3>,
            _ => panic!("Unexpected operator length {len}"),
        };
        map_opt(
            tuple((
                arithmetic_or_comparison,
                opt(|s| self.parse_template_parameters(s)),
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

    /// Parse custom literal
    fn parse_custom_literal<'source>(&self, s: &'source str) -> IResult<'source, Operator> {
        use nom::{character::complete::space0, sequence::preceded};
        use nom_supreme::tag::complete::tag;
        preceded(tag("\"\"").and(space0), |s| self.parse_identifier(s))
            .map(Operator::CustomLiteral)
            .parse(s)
    }
}

/// Parse bracket pair operators: calling and array indexing
fn call_or_index(s: &str) -> IResult<Operator> {
    use nom_supreme::tag::complete::tag;
    (tag("()").value(false).or(tag("[]").value(true)))
        .map(|is_index| Operator::CallIndex { is_index })
        .parse(s)
}

/// Parse allocation function overload declaration
fn new(s: &str) -> IResult<Operator> {
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
    use crate::tests::unwrap_parse;
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
        let parser = EntityParser::new();
        assert_eq!(
            parser.parse_custom_literal("\"\" _whatever"),
            Ok((
                "",
                Operator::CustomLiteral(unwrap_parse(parser.parse_identifier("_whatever")))
            ))
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
        let parser = EntityParser::new();

        // Symbol-based operators don't need spaces
        assert_eq!(
            parser.parse_operator_overload("operator*"),
            Ok(("", (Symbol::MulDeref.into(), None)))
        );
        assert_eq!(
            parser.parse_operator_overload("operator[]"),
            Ok(("", (Operator::CallIndex { is_index: true }, None)))
        );
        assert_eq!(
            parser.parse_operator_overload("operator\"\" _stuff"),
            Ok((
                "",
                (
                    Operator::CustomLiteral(unwrap_parse(parser.parse_identifier("_stuff"))),
                    None
                )
            ))
        );

        // Keyword-based operators need spaces
        assert_eq!(
            parser.parse_operator_overload("operator new[]"),
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
            parser.parse_operator_overload("operator co_await"),
            Ok(("", (Operator::CoAwait, None)))
        );

        // Type conversion operator works
        let type_like = |s| unwrap_parse(parser.parse_type_like(s));
        assert_eq!(
            parser.parse_operator_overload("operator unsigned long long"),
            Ok(("", (type_like("unsigned long long").into(), None)))
        );

        // Ambiguities between template and operator syntax are handled well
        let template_parameters = |s| unwrap_parse(parser.parse_template_parameters(s));
        assert_eq!(
            parser.parse_operator_overload("operator<<>"),
            Ok(("", (Symbol::Less.into(), Some(template_parameters("<>")))))
        );
        assert_eq!(
            parser.parse_operator_overload("operator<<void>"),
            Ok((
                "",
                (Symbol::Less.into(), Some(template_parameters("<void>")))
            ))
        );
    }
}
