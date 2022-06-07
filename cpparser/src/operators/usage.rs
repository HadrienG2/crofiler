//! Operator-related grammar that is only used when parsing expressions

use super::{Operator, Symbol};
use crate::{
    functions,
    types::{self, TypeLike},
    values::ValueLike,
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

/// Parse a binary operator that can be put between two expressions
///
/// We may sometimes not want to allow the comma , operator in order to avoid
/// confusing comma-delimited parsers like function calls, and may sometimes not
/// want to allow the greater > and shr >> operators in order to avoid confusing
/// the template parameter parser.
pub fn binary_expr_middle<
    const ALLOW_COMMA: bool,
    const ALLOW_GREATER: bool,
    IdentifierKey: Clone + Debug + PartialEq + Eq,
>(
    s: &str,
) -> IResult<Operator<IdentifierKey>> {
    // Most 1-character operators can be used in binary position, except for
    // the negation operators Not and BitNot
    let arith1 = super::arithmetic_or_comparison::<1, IdentifierKey>.verify(|op| match op {
        Operator::Basic {
            symbol,
            twice: false,
            equal: false,
        } => {
            use Symbol::*;
            match symbol {
                BitNot | Not => false,
                AddPlus | SubNeg | MulDeref | Div | Mod | Xor | AndRef | Or | AssignEq | Less => {
                    true
                }
                Comma => ALLOW_COMMA,
                Greater => ALLOW_GREATER,
            }
        }
        _ => unreachable!(),
    });

    // Most 2-character operators can be used in binary position, except for
    // increment and decrement, and shr in template contexts.
    let arith2 = super::arithmetic_or_comparison::<2, IdentifierKey>.verify(|op| match op {
        Operator::Basic {
            symbol,
            twice: true,
            equal: false,
        } => {
            use Symbol::*;
            match symbol {
                AddPlus | SubNeg => false,
                AndRef | Or | AssignEq | Less => true,
                Greater => ALLOW_GREATER,
                Xor | Mod | Div | MulDeref | BitNot | Not | Comma => unreachable!(),
            }
        }
        // This may need to be revised as C++ evolves
        _ => true,
    });

    // All 3-character operators can be used in binary position
    let arith3 = super::arithmetic_or_comparison::<3, IdentifierKey>;

    // No other operator can be used in binary position
    arith3.or(arith2).or(arith1).parse(s)
}

/// Parse an unary operator that can be applied to an expression in prefix position
pub fn unary_expr_prefix<IdentifierKey: Clone + Debug + PartialEq + Eq>(
    s: &str,
) -> IResult<Operator<IdentifierKey>> {
    use nom::{
        character::complete::{char, space0, space1},
        sequence::delimited,
    };
    use Symbol::*;

    let unary_symbol = super::symbol
        .verify(|s| [AddPlus, SubNeg, MulDeref, AndRef, BitNot, Not].contains(s))
        .map(Operator::from);

    let cast = delimited(char('('), types::type_like, char(')'))
        .map(|ty| Operator::Conversion(Box::new(ty)));

    // Must parse inc/dec before unary_symbol to prevent under-parsing
    (increment_decrement.or(unary_symbol).terminated(space0))
        .or((super::co_await.or(super::delete)).terminated(space1))
        .or(cast.terminated(space0))
        .parse(s)
}

/// Parse the increment/decrement operator
pub fn increment_decrement<IdentifierKey: Clone + Debug + PartialEq + Eq>(
    s: &str,
) -> IResult<Operator<IdentifierKey>> {
    use nom::combinator::map_opt;
    use Symbol::*;
    map_opt(
        super::symbol.and(super::symbol),
        |sym_pair| match sym_pair {
            (AddPlus, AddPlus) => Some(Operator::Basic {
                symbol: AddPlus,
                twice: true,
                equal: false,
            }),
            (SubNeg, SubNeg) => Some(Operator::Basic {
                symbol: SubNeg,
                twice: true,
                equal: false,
            }),
            _ => None,
        },
    )(s)
}

/// Parse new expression, i.e. usage of the new operator
pub fn new_expression(s: &str) -> IResult<NewExpression> {
    use nom::{
        character::complete::space0,
        combinator::opt,
        sequence::{preceded, tuple},
    };
    use nom_supreme::tag::complete::tag;
    let rooted = opt(tag("::").and(space0)).map(|o| o.is_some());
    (rooted.and(preceded(
        tag("new").and(space0),
        tuple((
            opt(functions::function_call).terminated(space0),
            types::type_like.terminated(space0),
            opt(functions::function_call),
        )),
    )))
    .map(|(rooted, (placement, ty, constructor))| NewExpression {
        rooted,
        placement,
        ty,
        constructor,
    })
    .parse(s)
}
//
/// New expression, i.e. usage of the new operator
// FIXME: This type appears in Box<T>, intern that once data is owned
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct NewExpression<'source> {
    /// Whether this new expression is rooted (starts with ::), which means that
    /// class-specific replacements will be ignored
    rooted: bool,

    /// Placement parameters
    placement: Option<Box<[ValueLike<'source>]>>,

    /// Type of values being created
    ty: TypeLike<'source>,

    /// Parameters to the values' constructor (if any)
    constructor: Option<Box<[ValueLike<'source>]>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn increment_decrement() {
        let parse_increment_decrement = super::increment_decrement::<&str>;
        assert_eq!(
            parse_increment_decrement("++"),
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
            parse_increment_decrement("--"),
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
        let parse_unary_expr_prefix = super::unary_expr_prefix::<&str>;

        // Lone symbol
        assert_eq!(
            parse_unary_expr_prefix("+"),
            Ok(("", Symbol::AddPlus.into()))
        );
        assert_eq!(
            parse_unary_expr_prefix("- "),
            Ok(("", Symbol::SubNeg.into()))
        );
        assert_eq!(
            parse_unary_expr_prefix("*"),
            Ok(("", Symbol::MulDeref.into()))
        );
        assert_eq!(
            parse_unary_expr_prefix("& "),
            Ok(("", Symbol::AndRef.into()))
        );
        assert_eq!(
            parse_unary_expr_prefix("~"),
            Ok(("", Symbol::BitNot.into()))
        );
        assert_eq!(parse_unary_expr_prefix("!"), Ok(("", Symbol::Not.into())));

        // Increment and decrement
        assert_eq!(
            parse_unary_expr_prefix("++"),
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
            parse_unary_expr_prefix("--"),
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
            parse_unary_expr_prefix("(float)"),
            Ok((
                "",
                Operator::Conversion(Box::new(force_parse(types::type_like, "float")))
            ))
        );

        // co_await
        assert_eq!(
            parse_unary_expr_prefix("co_await  "),
            Ok(("", Operator::CoAwait))
        );

        // delete
        assert_eq!(
            parse_unary_expr_prefix("delete[] "),
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
            super::binary_expr_middle::<false, false, &str>("="),
            Ok(("", Symbol::AssignEq.into()))
        );

        // Two-character, other than increment/decrement
        assert_eq!(
            super::binary_expr_middle::<false, false, &str>("+="),
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
            super::binary_expr_middle::<false, false, &str>("<=>"),
            Ok(("", Operator::Spaceship))
        );

        // Only accept comma if instructed to do so
        assert!(super::binary_expr_middle::<false, false, &str>(",").is_err());
        assert_eq!(
            super::binary_expr_middle::<true, false, &str>(","),
            Ok(("", Symbol::Comma.into()))
        );

        // Only accept greater sign if instructed to do so
        assert!(super::binary_expr_middle::<false, false, &str>(">").is_err());
        assert!(super::binary_expr_middle::<false, false, &str>(">>").is_err());
        assert_eq!(
            super::binary_expr_middle::<false, true, &str>(">"),
            Ok(("", Symbol::Greater.into()))
        );
        assert_eq!(
            super::binary_expr_middle::<false, true, &str>(">>"),
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
        // Basic form
        assert_eq!(
            super::new_expression("new int"),
            Ok((
                "",
                NewExpression {
                    ty: force_parse(types::type_like, "int"),
                    ..Default::default()
                }
            ))
        );

        // Rooted form
        assert_eq!(
            super::new_expression("::new double"),
            Ok((
                "",
                NewExpression {
                    rooted: true,
                    ty: force_parse(types::type_like, "double"),
                    ..Default::default()
                }
            ))
        );

        // Placement parameters
        assert_eq!(
            super::new_expression("new (42) MyClass"),
            Ok((
                "",
                NewExpression {
                    placement: Some(vec![42u8.into()].into()),
                    ty: force_parse(types::type_like, "MyClass"),
                    ..Default::default()
                }
            ))
        );

        // Constructor parameters
        assert_eq!(
            super::new_expression("new MyClass('x')"),
            Ok((
                "",
                NewExpression {
                    ty: force_parse(types::type_like, "MyClass"),
                    constructor: Some(vec!['x'.into()].into()),
                    ..Default::default()
                }
            ))
        );
    }
}
