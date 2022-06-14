//! Operator-related grammar that is only used when parsing expressions

use super::{Operator, Symbol};
use crate::{functions::FunctionArgumentsKey, types::TypeKey, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

impl EntityParser {
    /// Parse the increment/decrement operator
    pub fn parse_increment_decrement(s: &str) -> IResult<Operator> {
        use nom_supreme::tag::complete::tag;
        (tag("++").value(Operator::Basic {
            symbol: Symbol::AddPlus,
            twice: true,
            equal: false,
        }))
        .or(tag("--").value(Operator::Basic {
            symbol: Symbol::SubNeg,
            twice: true,
            equal: false,
        }))
        .parse(s)
    }

    /// Parse an unary operator that can be applied to an expression in prefix position
    pub fn parse_unary_expr_prefix<'source>(&self, s: &'source str) -> IResult<'source, Operator> {
        use nom::{
            character::complete::{char, space0, space1},
            sequence::delimited,
        };
        use Symbol::*;

        let unary_symbol = super::symbol
            .verify(|s| [AddPlus, SubNeg, MulDeref, AndRef, BitNot, Not].contains(s))
            .map(Operator::from);

        let cast = delimited(
            char('(').and(space0),
            |s| self.parse_type_like(s),
            space0.and(char(')')),
        )
        .map(Operator::Conversion);

        // Must parse inc/dec before unary_symbol to prevent under-parsing
        ((cast.or(Self::parse_increment_decrement).or(unary_symbol)).terminated(space0))
            .or((super::delete.or(super::co_await)).terminated(space1))
            .parse(s)
    }

    /// Parse a binary operator that can be put between two expressions
    ///
    /// We may sometimes not want to allow the comma , operator in order to avoid
    /// confusing comma-delimited parsers like function calls, and may sometimes not
    /// want to allow the greater > and shr >> operators in order to avoid confusing
    /// the template parameter parser.
    ///
    pub fn parse_binary_expr_middle(
        s: &str,
        allow_comma: bool,
        allow_greater: bool,
    ) -> IResult<Operator> {
        // Most 1-character operators can be used in binary position, except for
        // the negation operators Not and BitNot
        let arith1 = super::arithmetic_or_comparison::<1>.verify(|op| match op {
            Operator::Basic {
                symbol,
                twice: false,
                equal: false,
            } => {
                use Symbol::*;
                match symbol {
                    BitNot | Not => false,
                    AddPlus | SubNeg | MulDeref | Div | Mod | Xor | AndRef | Or | AssignEq
                    | Less => true,
                    Comma => allow_comma,
                    Greater => allow_greater,
                }
            }
            _ => unreachable!(),
        });

        // Most 2-character operators can be used in binary position, except for
        // increment and decrement, and shr in template contexts.
        let arith2 = super::arithmetic_or_comparison::<2>.verify(|op| match op {
            Operator::Basic {
                symbol,
                twice: true,
                equal: false,
            } => {
                use Symbol::*;
                match symbol {
                    AddPlus | SubNeg => false,
                    AndRef | Or | AssignEq | Less => true,
                    Greater => allow_greater,
                    Xor | Mod | Div | MulDeref | BitNot | Not | Comma => unreachable!(),
                }
            }
            // This may need to be revised as C++ evolves
            _ => true,
        });

        // All 3-character operators can be used in binary position
        let arith3 = super::arithmetic_or_comparison::<3>;

        // No other operator can be used in binary position
        arith3.or(arith2).or(arith1).parse(s)
    }

    /// Parse new expression, i.e. usage of the new operator
    pub fn parse_new_expression<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, NewExpression> {
        use nom::{
            character::complete::space0,
            combinator::opt,
            sequence::{separated_pair, tuple},
        };
        use nom_supreme::tag::complete::tag;
        let rooted = opt(tag("::")).map(|o| o.is_some());
        separated_pair(
            rooted,
            tag("new").and(space0),
            tuple((
                opt(|s| self.parse_function_call(s)).terminated(space0),
                (|s| self.parse_type_like(s)).terminated(space0),
                opt(|s| self.parse_function_call(s)),
            )),
        )
        .map(|(rooted, (placement, ty, constructor))| NewExpression {
            rooted,
            placement,
            ty,
            constructor,
        })
        .parse(s)
    }
}

/// New expression, i.e. usage of the new operator
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct NewExpression {
    /// Whether this new expression is rooted (starts with ::), which means that
    /// class-specific replacements will be ignored
    rooted: bool,

    /// Placement parameters
    placement: Option<FunctionArgumentsKey>,

    /// Type of values being created
    ty: TypeKey,

    /// Parameters to the values' constructor (if any)
    constructor: Option<FunctionArgumentsKey>,
}
//
impl From<TypeKey> for NewExpression {
    fn from(ty: TypeKey) -> Self {
        Self {
            rooted: false,
            placement: None,
            ty,
            constructor: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn increment_decrement() {
        assert_eq!(
            EntityParser::parse_increment_decrement("++"),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::AddPlus,
                    twice: true,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            EntityParser::parse_increment_decrement("--"),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::SubNeg,
                    twice: true,
                    equal: false,
                }
            ))
        );
    }

    #[test]
    fn unary_expr_prefix() {
        let parser = EntityParser::new();

        // Lone symbol
        assert_eq!(
            parser.parse_unary_expr_prefix("+"),
            Ok(("", Symbol::AddPlus.into()))
        );
        assert_eq!(
            parser.parse_unary_expr_prefix("- "),
            Ok(("", Symbol::SubNeg.into()))
        );
        assert_eq!(
            parser.parse_unary_expr_prefix("*"),
            Ok(("", Symbol::MulDeref.into()))
        );
        assert_eq!(
            parser.parse_unary_expr_prefix("& "),
            Ok(("", Symbol::AndRef.into()))
        );
        assert_eq!(
            parser.parse_unary_expr_prefix("~"),
            Ok(("", Symbol::BitNot.into()))
        );
        assert_eq!(
            parser.parse_unary_expr_prefix("!"),
            Ok(("", Symbol::Not.into()))
        );

        // Increment and decrement
        assert_eq!(
            parser.parse_unary_expr_prefix("++"),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::AddPlus,
                    twice: true,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            parser.parse_unary_expr_prefix("--"),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::SubNeg,
                    twice: true,
                    equal: false,
                }
            ))
        );

        // Casts
        assert_eq!(
            parser.parse_unary_expr_prefix("(float)"),
            Ok(("", unwrap_parse(parser.parse_type_like("float")).into()))
        );

        // co_await
        assert_eq!(
            parser.parse_unary_expr_prefix("co_await  "),
            Ok(("", Operator::CoAwait))
        );

        // delete
        assert_eq!(
            parser.parse_unary_expr_prefix("delete[] "),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: true,
                    array: true
                }
            ))
        );
    }

    #[test]
    fn binary_expr_middle() {
        // Lone symbol, other than not
        assert_eq!(
            EntityParser::parse_binary_expr_middle("=", true, true),
            Ok(("", Symbol::AssignEq.into()))
        );

        // Two-character, other than increment/decrement
        assert_eq!(
            EntityParser::parse_binary_expr_middle("+=", true, true),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::AddPlus,
                    twice: false,
                    equal: true,
                }
            ))
        );

        // Three-character
        assert_eq!(
            EntityParser::parse_binary_expr_middle("<=>", true, true),
            Ok(("", Operator::Spaceship))
        );

        // Only accept comma if instructed to do so
        assert!(EntityParser::parse_binary_expr_middle(",", false, true).is_err());
        assert_eq!(
            EntityParser::parse_binary_expr_middle(",", true, true),
            Ok(("", Symbol::Comma.into()))
        );

        // Only accept greater sign if instructed to do so
        assert!(EntityParser::parse_binary_expr_middle(">", true, false).is_err());
        assert!(EntityParser::parse_binary_expr_middle(">>", true, false).is_err());
        assert_eq!(
            EntityParser::parse_binary_expr_middle(">", true, true),
            Ok(("", Symbol::Greater.into()))
        );
        assert_eq!(
            EntityParser::parse_binary_expr_middle(">>", true, true),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::Greater,
                    twice: true,
                    equal: false,
                }
            ))
        );
    }

    #[test]
    fn new_expression() {
        let parser = EntityParser::new();
        let type_like = |s| unwrap_parse(parser.parse_type_like(s));

        // Basic form
        assert_eq!(
            parser.parse_new_expression("new int"),
            Ok(("", NewExpression::from(type_like("int"))))
        );

        // Rooted form
        assert_eq!(
            parser.parse_new_expression("::new double"),
            Ok((
                "",
                NewExpression {
                    rooted: true,
                    placement: None,
                    ty: type_like("double"),
                    constructor: None,
                }
            ))
        );

        // Placement parameters
        let function_call = |s| unwrap_parse(parser.parse_function_call(s));
        assert_eq!(
            parser.parse_new_expression("new (42) MyClass"),
            Ok((
                "",
                NewExpression {
                    rooted: false,
                    placement: Some(function_call("(42)")),
                    ty: type_like("MyClass"),
                    constructor: None,
                }
            ))
        );

        // Constructor parameters
        assert_eq!(
            parser.parse_new_expression("new MyClass('x')"),
            Ok((
                "",
                NewExpression {
                    rooted: false,
                    placement: None,
                    ty: type_like("MyClass"),
                    constructor: Some(function_call("('x')")),
                }
            ))
        );
    }
}
