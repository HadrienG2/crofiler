//! Operator-related grammar that is only used when parsing expressions

use super::{Operator, Symbol};
use crate::{
    display::{CustomDisplay, DisplayState},
    subparsers::{
        functions::{FunctionArgumentsKey, FunctionArgumentsView},
        types::{TypeKey, TypeView},
    },
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

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
    pub fn parse_unary_expr_prefix<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, Operator> {
        self.parse_unary_expr_prefix_imut(s)
    }

    /// Implementation of parse_unary_expr_prefix using internal mutability
    pub(crate) fn parse_unary_expr_prefix_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Operator> {
        use nom::{
            character::complete::{char, multispace0, multispace1},
            sequence::delimited,
        };
        use Symbol::*;

        let unary_symbol = super::symbol
            .verify(|s| [AddPlus, SubNeg, MulDeref, AndRef, BitNot, Not].contains(s))
            .map(Operator::from);

        let cast = delimited(
            char('(').and(multispace0),
            |s| self.parse_type_like_imut(s),
            multispace0.and(char(')')),
        )
        .map(Operator::Conversion);

        // Must parse inc/dec before unary_symbol to prevent under-parsing
        ((cast.or(Self::parse_increment_decrement).or(unary_symbol)).terminated(multispace0))
            .or((super::delete.or(super::co_await)).terminated(multispace1))
            .parse(s)
    }

    /// Parse a binary operator that can be put between two expressions
    ///
    /// We may sometimes not want to allow the comma , operator in order to avoid
    /// confusing comma-delimited parsers like function calls, and may sometimes not
    /// want to allow the greater `>` and shr `>>` operators in order to avoid
    /// confusing the template parameter parser.
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
        &mut self,
        s: &'source str,
    ) -> IResult<'source, NewExpression> {
        self.parse_new_expression_imut(s)
    }

    /// Implementation of parse_new_expression using internal mutability
    pub(crate) fn parse_new_expression_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, NewExpression> {
        use nom::{
            character::complete::multispace0,
            combinator::opt,
            sequence::{separated_pair, tuple},
        };
        use nom_supreme::tag::complete::tag;
        let rooted = opt(tag("::")).map(|o| o.is_some());
        separated_pair(
            rooted,
            tag("new").and(multispace0),
            tuple((
                opt(|s| self.parse_function_call_imut(s)).terminated(multispace0),
                (|s| self.parse_type_like_imut(s)).terminated(multispace0),
                opt(|s| self.parse_function_call_imut(s)),
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

    /// Access a previously parsed new-expression
    pub fn new_expression(&self, ne: NewExpression) -> NewExpressionView {
        NewExpressionView::new(ne, self)
    }
}

/// New-expression, i.e. usage of the new operator
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

/// A view of a new-expression, i.e. usage of the new operator
pub struct NewExpressionView<'entities> {
    /// Wrapped NewExpression
    inner: NewExpression,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> NewExpressionView<'entities> {
    /// Build a new-expression view
    pub fn new(inner: NewExpression, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Whether this new expression is rooted (starts with ::), which means that
    /// class-specific replacements will be ignored
    pub fn rooted(&self) -> bool {
        self.inner.rooted
    }

    /// Placement parameters (if any)
    pub fn placement(&self) -> Option<FunctionArgumentsView> {
        self.inner
            .placement
            .map(|args| self.entities.function_arguments(args))
    }

    /// Type of values being created
    pub fn ty(&self) -> TypeView {
        self.entities.type_like(self.inner.ty)
    }

    /// Parameters to the values' constructor (if any)
    pub fn constructor(&self) -> Option<FunctionArgumentsView> {
        self.inner
            .constructor
            .map(|args| self.entities.function_arguments(args))
    }
}
//
impl<'entities> PartialEq for NewExpressionView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for NewExpressionView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for NewExpressionView<'entities> {
    fn recursion_depth(&self) -> usize {
        self.placement()
            .recursion_depth()
            .max(self.ty().recursion_depth())
            .max(self.constructor().recursion_depth())
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if self.rooted() {
            write!(f, "::")?;
        }
        write!(f, "new")?;
        self.placement().display_impl(f, state)?;
        write!(f, " ")?;
        self.ty().display_impl(f, state)?;
        self.constructor().display_impl(f, state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
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
        let mut parser = EntityParser::new();

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
        let mut parser = EntityParser::new();
        let type_like = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_type_like(s));
        let check_new_expression = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parser.parse_new_expression(input), Ok(("", expected)));
            check_custom_display(parser.new_expression(expected), displays);
        };

        // Basic form
        let mut expected = NewExpression::from(type_like(&mut parser, "int"));
        check_new_expression(&mut parser, "new int", expected, &["new int"]);

        // Rooted form
        expected = NewExpression {
            rooted: true,
            placement: None,
            ty: type_like(&mut parser, "double"),
            constructor: None,
        };
        check_new_expression(&mut parser, "::new double", expected, &["::new double"]);

        // Placement parameters
        let function_call =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_function_call(s));
        expected = NewExpression {
            rooted: false,
            placement: Some(function_call(&mut parser, "(42)")),
            ty: type_like(&mut parser, "MyClass"),
            constructor: None,
        };
        check_new_expression(
            &mut parser,
            "new (42) MyClass",
            expected,
            &["new(…) MyClass", "new(42) MyClass"],
        );

        // Constructor parameters
        expected = NewExpression {
            rooted: false,
            placement: None,
            ty: type_like(&mut parser, "MyClass"),
            constructor: Some(function_call(&mut parser, "('x')")),
        };
        check_new_expression(
            &mut parser,
            "new MyClass('x')",
            expected,
            &["new MyClass(…)", "new MyClass('x')"],
        );
    }
}
