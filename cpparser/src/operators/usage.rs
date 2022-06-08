//! Operator-related grammar that is only used when parsing expressions

use super::{Operator, Symbol};
use crate::{
    functions,
    names::atoms,
    types::{self, TypeLike},
    values::ValueLike,
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parse the increment/decrement operator
    pub fn parse_increment_decrement(
        s: &str,
    ) -> IResult<Operator<atoms::IdentifierKey, crate::PathKey>> {
        increment_decrement::<atoms::IdentifierKey, crate::PathKey>(s)
    }

    /// Parse an unary operator that can be applied to an expression in prefix position
    pub fn parse_unary_expr_prefix<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Operator<'source, atoms::IdentifierKey, crate::PathKey>> {
        unary_expr_prefix(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }

    /// Parse a binary operator that can be put between two expressions
    ///
    /// We may sometimes not want to allow the comma , operator in order to avoid
    /// confusing comma-delimited parsers like function calls, and may sometimes not
    /// want to allow the greater > and shr >> operators in order to avoid confusing
    /// the template parameter parser.
    ///
    pub fn parse_binary_expr_middle<const ALLOW_COMMA: bool, const ALLOW_GREATER: bool>(
        s: &str,
    ) -> IResult<Operator<atoms::IdentifierKey, crate::PathKey>> {
        binary_expr_middle::<ALLOW_COMMA, ALLOW_GREATER, atoms::IdentifierKey, crate::PathKey>(s)
    }

    /// Parse new expression, i.e. usage of the new operator
    pub fn parse_new_expression<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, NewExpression<'source, atoms::IdentifierKey, crate::PathKey>> {
        new_expression(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parse the increment/decrement operator
// TODO: Make private once users are migrated
pub fn increment_decrement<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
>(
    s: &str,
) -> IResult<Operator<IdentifierKey, PathKey>> {
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
// TODO: Make private once users are migrated
pub fn unary_expr_prefix<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, Operator<'source, IdentifierKey, PathKey>> {
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
        |s| types::type_like(s, parse_identifier, path_to_key),
        space0.and(char(')')),
    )
    .map(|ty| Operator::Conversion(Box::new(ty)));

    // Must parse inc/dec before unary_symbol to prevent under-parsing
    ((cast.or(increment_decrement).or(unary_symbol)).terminated(space0))
        .or((super::delete.or(super::co_await)).terminated(space1))
        .parse(s)
}

/// Parse a binary operator that can be put between two expressions
///
/// See EntityParser::parse_binary_expr_middle docs for detailed semantics
///
// TODO: Make private once users are migrated
pub fn binary_expr_middle<
    const ALLOW_COMMA: bool,
    const ALLOW_GREATER: bool,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
>(
    s: &str,
) -> IResult<Operator<IdentifierKey, PathKey>> {
    // Most 1-character operators can be used in binary position, except for
    // the negation operators Not and BitNot
    let arith1 =
        super::arithmetic_or_comparison::<1, IdentifierKey, PathKey>.verify(|op| match op {
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
                    Comma => ALLOW_COMMA,
                    Greater => ALLOW_GREATER,
                }
            }
            _ => unreachable!(),
        });

    // Most 2-character operators can be used in binary position, except for
    // increment and decrement, and shr in template contexts.
    let arith2 =
        super::arithmetic_or_comparison::<2, IdentifierKey, PathKey>.verify(|op| match op {
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
    let arith3 = super::arithmetic_or_comparison::<3, IdentifierKey, PathKey>;

    // No other operator can be used in binary position
    arith3.or(arith2).or(arith1).parse(s)
}

/// Parse new expression, i.e. usage of the new operator
// TODO: Make private once users are migrated
pub fn new_expression<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, NewExpression<'source, IdentifierKey, PathKey>> {
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
            opt(functions::function_call).terminated(space0),
            (|s| types::type_like(s, parse_identifier, path_to_key)).terminated(space0),
            opt(functions::function_call),
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
//
/// New expression, i.e. usage of the new operator
// FIXME: This type appears in Box<T>, intern that once data is owned
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NewExpression<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Whether this new expression is rooted (starts with ::), which means that
    /// class-specific replacements will be ignored
    rooted: bool,

    /// Placement parameters
    placement: Option<Box<[ValueLike<'source>]>>,

    /// Type of values being created
    ty: TypeLike<'source, IdentifierKey, PathKey>,

    /// Parameters to the values' constructor (if any)
    constructor: Option<Box<[ValueLike<'source>]>>,
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for NewExpression<'_, IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self {
            rooted: Default::default(),
            placement: Default::default(),
            ty: Default::default(),
            constructor: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    fn parse_type_like(s: &str) -> IResult<TypeLike<&str, &Path>> {
        types::type_like(s, &atoms::identifier, &Path::new)
    }

    #[test]
    fn increment_decrement() {
        let parse_increment_decrement = super::increment_decrement::<&str, &Path>;
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

    fn parse_unary_expr_prefix(s: &str) -> IResult<Operator<&str, &Path>> {
        super::unary_expr_prefix(s, &atoms::identifier, &Path::new)
    }

    #[test]
    fn unary_expr_prefix() {
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
                Operator::Conversion(Box::new(force_parse(parse_type_like, "float")))
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
            super::binary_expr_middle::<false, false, &str, &Path>("="),
            Ok(("", Symbol::AssignEq.into()))
        );

        // Two-character, other than increment/decrement
        assert_eq!(
            super::binary_expr_middle::<false, false, &str, &Path>("+="),
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
            super::binary_expr_middle::<false, false, &str, &Path>("<=>"),
            Ok(("", Operator::Spaceship))
        );

        // Only accept comma if instructed to do so
        assert!(super::binary_expr_middle::<false, false, &str, &Path>(",").is_err());
        assert_eq!(
            super::binary_expr_middle::<true, false, &str, &Path>(","),
            Ok(("", Symbol::Comma.into()))
        );

        // Only accept greater sign if instructed to do so
        assert!(super::binary_expr_middle::<false, false, &str, &Path>(">").is_err());
        assert!(super::binary_expr_middle::<false, false, &str, &Path>(">>").is_err());
        assert_eq!(
            super::binary_expr_middle::<false, true, &str, &Path>(">"),
            Ok(("", Symbol::Greater.into()))
        );
        assert_eq!(
            super::binary_expr_middle::<false, true, &str, &Path>(">>"),
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
        let parse_new_expression = |s| super::new_expression(s, &atoms::identifier, &Path::new);

        // Basic form
        assert_eq!(
            parse_new_expression("new int"),
            Ok((
                "",
                NewExpression {
                    ty: force_parse(parse_type_like, "int"),
                    ..Default::default()
                }
            ))
        );

        // Rooted form
        assert_eq!(
            parse_new_expression("::new double"),
            Ok((
                "",
                NewExpression {
                    rooted: true,
                    ty: force_parse(parse_type_like, "double"),
                    ..Default::default()
                }
            ))
        );

        // Placement parameters
        assert_eq!(
            parse_new_expression("new (42) MyClass"),
            Ok((
                "",
                NewExpression {
                    placement: Some(vec![42u8.into()].into()),
                    ty: force_parse(parse_type_like, "MyClass"),
                    ..Default::default()
                }
            ))
        );

        // Constructor parameters
        assert_eq!(
            parse_new_expression("new MyClass('x')"),
            Ok((
                "",
                NewExpression {
                    ty: force_parse(parse_type_like, "MyClass"),
                    constructor: Some(vec!['x'.into()].into()),
                    ..Default::default()
                }
            ))
        );
    }
}
