//! Parsers for operator overloads

use crate::cpp::{
    atoms,
    templates::{self, TemplateParameters},
    types::{self, TypeLike},
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
            new_or_delete
                .or(co_await)
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
            arithmetic_or_comparison::<LEN>,
            opt(templates::template_parameters),
            peek(opt(symbol)),
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

/// Parse arithmetic and comparison operators
///
/// Unfortunately, the grammatically ambiguous nature of characters < and >
/// strikes here. If a template parameter list can be expected after this
/// operator (as in "operator<<void>"), you will need to call this parser with
/// LEN varying from 1 to 3 in a context where the validity of the overall parse
/// can be assessed.
fn arithmetic_or_comparison<const LEN: usize>(s: &str) -> IResult<Operator> {
    use nom::{combinator::map_opt, sequence::tuple};
    match LEN {
        // Single-character operator
        1 => symbol
            .map(|symbol| Operator::Basic {
                symbol,
                twice: false,
                equal: false,
            })
            .parse(s),

        // Two-character operator
        2 => map_opt(symbol.and(symbol), |symbol_pair| match symbol_pair {
            // Symbol with equal sign (includes == for consistency with comparisons)
            (symbol, Symbol::AssignEq) => Some(Operator::Basic {
                symbol,
                twice: false,
                equal: true,
            }),

            // Duplicate symbol other than ==
            (symbol, symbol2) if symbol2 == symbol => Some(Operator::Basic {
                symbol,
                twice: true,
                equal: false,
            }),

            // Pointer dereference
            (Symbol::SubNeg, Symbol::Greater) => Some(Operator::Deref { star: false }),

            // Anything else sounds bad
            _ => None,
        })
        .parse(s),

        // Three-character operator
        3 => map_opt(tuple((symbol, symbol, symbol)), |tuple| match tuple {
            // Duplicate symbol with assignment
            (symbol, symbol2, Symbol::AssignEq) if symbol2 == symbol => Some(Operator::Basic {
                symbol,
                twice: true,
                equal: true,
            }),

            // Dereference operators
            (Symbol::SubNeg, Symbol::Greater, Symbol::MulDeref) => {
                Some(Operator::Deref { star: true })
            }

            // Spaceship operator
            (Symbol::Less, Symbol::AssignEq, Symbol::Greater) => Some(Operator::Spaceship),

            // Anything else sounds bad
            _ => None,
        })
        .parse(s),

        _ => panic!("C++ does not have {LEN}-symbol operators (yet?)"),
    }
}

/// Parse bracket pair operators: calling and array indexing
fn call_or_index(s: &str) -> IResult<Operator> {
    use nom_supreme::tag::complete::tag;
    (tag("()").or(tag("[]")))
        .map(|s| Operator::CallIndex {
            is_index: s == "[]",
        })
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

/// Parse allocation and deallocation functions
fn new_or_delete(s: &str) -> IResult<Operator> {
    use nom::combinator::opt;
    use nom_supreme::tag::complete::tag;
    ((atoms::keyword("new")
        .value(false)
        .or(atoms::keyword("delete").value(true)))
    .and(opt(tag("[]"))))
    .map(|(is_delete, array)| Operator::NewDelete {
        is_delete,
        array: array.is_some(),
    })
    .parse(s)
}

/// Parse co_await
fn co_await(s: &str) -> IResult<Operator> {
    atoms::keyword("co_await").value(Operator::CoAwait).parse(s)
}

/// C++ operators that can be overloaded
#[derive(Debug, PartialEq, Clone)]
pub enum Operator<'source> {
    /// Basic grammar followed by most operators: a symbol that can appear
    /// twice, optionally followed by an equality sign.
    Basic {
        /// Base symbol at the beginning
        symbol: Symbol,

        /// Whether this symbol is repeated
        twice: bool,

        /// Whether this singleton/pair is followed by an equality sign
        equal: bool,
    },

    /// Dereference operators -> and ->*
    Deref {
        /// -> if this is false, ->* if this is true
        star: bool,
    },

    /// Spaceship operator <=>
    Spaceship,

    /// Bracketed operators () and []
    CallIndex {
        /// () if this is false, [] if this is true
        is_index: bool,
    },

    /// Custom literal operator (operator "" <suffix-identifier>)
    CustomLiteral(&'source str),

    /// Allocation/deallocation functions
    NewDelete {
        /// new if this is false, delete if this is true
        is_delete: bool,

        /// True if this targets arrays (e.g. "operator new[]")
        array: bool,
    },

    /// Overloaded co_await operator
    CoAwait,

    /// Type conversion operator ("operator <type>")
    Conversion(Box<TypeLike<'source>>),
}

/// Parser for symbols most commonly found in C++ operator names
fn symbol(s: &str) -> IResult<Symbol> {
    use nom::{character::complete::anychar, combinator::map_opt};
    use Symbol::*;
    map_opt(anychar, |c| match c {
        '+' => Some(Add),
        '-' => Some(SubNeg),
        '*' => Some(MulDeref),
        '/' => Some(Div),
        '%' => Some(Mod),
        '^' => Some(Xor),
        '&' => Some(AndRef),
        '|' => Some(Or),
        '~' => Some(BitNot),
        '!' => Some(Not),
        '=' => Some(AssignEq),
        '<' => Some(Less),
        '>' => Some(Greater),
        ',' => Some(Comma),
        _ => None,
    })(s)
}

/// Symbols most commonly found in C++ operator names
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Symbol {
    /// +
    Add,

    /// -
    SubNeg,

    /// *
    MulDeref,

    /// /
    Div,

    /// %
    Mod,

    /// ^
    Xor,

    /// &
    AndRef,

    /// |
    Or,

    /// ~
    BitNot,

    /// !
    Not,

    /// =
    AssignEq,

    /// <
    Less,

    /// >
    Greater,

    /// ,
    Comma,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpp::tests::force_parse_type;
    use pretty_assertions::assert_eq;

    #[test]
    fn symbol() {
        assert_eq!(super::symbol("+"), Ok(("", Symbol::Add)));
        assert_eq!(super::symbol("-"), Ok(("", Symbol::SubNeg)));
        assert_eq!(super::symbol("*"), Ok(("", Symbol::MulDeref)));
        assert_eq!(super::symbol("/"), Ok(("", Symbol::Div)));
        assert_eq!(super::symbol("%"), Ok(("", Symbol::Mod)));
        assert_eq!(super::symbol("^"), Ok(("", Symbol::Xor)));
        assert_eq!(super::symbol("&"), Ok(("", Symbol::AndRef)));
        assert_eq!(super::symbol("|"), Ok(("", Symbol::Or)));
        assert_eq!(super::symbol("~"), Ok(("", Symbol::BitNot)));
        assert_eq!(super::symbol("!"), Ok(("", Symbol::Not)));
        assert_eq!(super::symbol("="), Ok(("", Symbol::AssignEq)));
        assert_eq!(super::symbol("<"), Ok(("", Symbol::Less)));
        assert_eq!(super::symbol(">"), Ok(("", Symbol::Greater)));
        assert_eq!(super::symbol(","), Ok(("", Symbol::Comma)));
    }

    #[test]
    fn arithmetic_or_comparison() {
        // Lone symbol
        assert_eq!(
            super::arithmetic_or_comparison::<1>("+"),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::Add,
                    twice: false,
                    equal: false,
                }
            ))
        );

        // Symbol with equal sign
        assert_eq!(
            super::arithmetic_or_comparison::<1>("-="),
            Ok((
                "=",
                Operator::Basic {
                    symbol: Symbol::SubNeg,
                    twice: false,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            super::arithmetic_or_comparison::<2>("-="),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::SubNeg,
                    twice: false,
                    equal: true,
                }
            ))
        );

        // Duplicated symbol
        assert_eq!(
            super::arithmetic_or_comparison::<1>("<<"),
            Ok((
                "<",
                Operator::Basic {
                    symbol: Symbol::Less,
                    twice: false,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            super::arithmetic_or_comparison::<2>("<<"),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::Less,
                    twice: true,
                    equal: false,
                }
            ))
        );

        // Duplicated symbol with equal sign
        assert_eq!(
            super::arithmetic_or_comparison::<1>(">>="),
            Ok((
                ">=",
                Operator::Basic {
                    symbol: Symbol::Greater,
                    twice: false,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            super::arithmetic_or_comparison::<2>(">>="),
            Ok((
                "=",
                Operator::Basic {
                    symbol: Symbol::Greater,
                    twice: true,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            super::arithmetic_or_comparison::<3>(">>="),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::Greater,
                    twice: true,
                    equal: true,
                }
            ))
        );

        // Equality can, in principle, be parsed either as a duplicated symbol
        // or as a symbol with an equal sign. We go for consistency with other
        // comparison operators, which will be parsed as the latter.
        assert_eq!(
            super::arithmetic_or_comparison::<2>("=="),
            Ok((
                "",
                Operator::Basic {
                    symbol: Symbol::AssignEq,
                    twice: false,
                    equal: true,
                }
            ))
        );

        // Spaceship operator gets its own variant because it's too weird
        assert_eq!(
            super::arithmetic_or_comparison::<3>("<=>"),
            Ok(("", Operator::Spaceship))
        );

        // Same for dereference operator
        assert_eq!(
            super::arithmetic_or_comparison::<1>("->"),
            Ok((
                ">",
                Operator::Basic {
                    symbol: Symbol::SubNeg,
                    twice: false,
                    equal: false,
                }
            ))
        );
        assert_eq!(
            super::arithmetic_or_comparison::<2>("->"),
            Ok(("", Operator::Deref { star: false }))
        );
        assert_eq!(
            super::arithmetic_or_comparison::<3>("->*"),
            Ok(("", Operator::Deref { star: true }))
        );
    }

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
    fn new_or_delete() {
        assert_eq!(
            super::new_or_delete("new"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: false,
                    array: false
                }
            ))
        );
        assert_eq!(
            super::new_or_delete("new[]"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: false,
                    array: true
                }
            ))
        );
        assert_eq!(
            super::new_or_delete("delete"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: true,
                    array: false
                }
            ))
        );
        assert_eq!(
            super::new_or_delete("delete[]"),
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
    fn co_await() {
        assert_eq!(super::co_await("co_await"), Ok(("", Operator::CoAwait)));
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
                    Operator::Conversion(Box::new(force_parse_type("unsigned long long"))),
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
                    Some(Some(vec![force_parse_type("void").into()].into()))
                )
            ))
        );
    }
}
