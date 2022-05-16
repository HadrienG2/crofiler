//! Parsers for operator overloads

use crate::cpp::{
    atoms,
    types::{self, TypeLike},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parse any supported operator overload
pub fn operator_overload(
    s: &str,
    next_delimiter: impl Fn(&str) -> IResult<()>,
) -> IResult<Operator> {
    use nom::{character::complete::char, sequence::preceded};
    use nom_supreme::tag::complete::tag;
    let next_delimiter = &next_delimiter;
    let type_like = move |s| types::type_like(s, next_delimiter);
    preceded(
        tag("operator"),
        (arithmetic_or_comparison.or(call_index)).or(preceded(
            char(' '),
            new_delete
                .or(custom_literal)
                .or(co_await)
                // Must come last as it matches keywords
                .or(type_like.map(Operator::Conversion)),
        )),
    )
    .parse(s)
}

/// Parse arithmetic and comparison operators
fn arithmetic_or_comparison(s: &str) -> IResult<Operator> {
    use nom::{
        combinator::{map_opt, opt},
        sequence::tuple,
    };
    map_opt(
        tuple((symbol, opt(symbol), opt(symbol))),
        |tuple| match tuple {
            // Isolated symbol
            (symbol, None, None) => Some(Operator::Basic {
                symbol,
                twice: false,
                equal: false,
            }),

            // Duplicate symbol, including ==
            (symbol, Some(symbol2), None) if symbol2 == symbol => Some(Operator::Basic {
                symbol,
                twice: true,
                equal: false,
            }),

            // Arbitrary symbol with equal sign (not including ==)
            (symbol, Some(Symbol::AssignEq), None) => Some(Operator::Basic {
                symbol,
                twice: false,
                equal: true,
            }),

            // Duplicate symbol with assignment
            (symbol, Some(symbol2), Some(Symbol::AssignEq)) if symbol2 == symbol => {
                Some(Operator::Basic {
                    symbol,
                    twice: true,
                    equal: true,
                })
            }

            // Dereference operators
            (Symbol::SubNeg, Some(Symbol::Greater), None) => Some(Operator::Deref { star: false }),
            (Symbol::SubNeg, Some(Symbol::Greater), Some(Symbol::MulDeref)) => {
                Some(Operator::Deref { star: true })
            }

            // Spaceship operator
            (Symbol::Less, Some(Symbol::AssignEq), Some(Symbol::Greater)) => {
                Some(Operator::Spaceship)
            }

            // Anything else sounds bad
            _ => None,
        },
    )(s)
}

/// Parse bracket pair operators: calling and array indexing
fn call_index(s: &str) -> IResult<Operator> {
    use nom_supreme::tag::complete::tag;
    (tag("()").or(tag("[]")))
        .map(|s| Operator::CallIndex {
            is_index: s == "[]",
        })
        .parse(s)
}

/// Parse allocation and deallocation functions
fn new_delete(s: &str) -> IResult<Operator> {
    use nom::combinator::opt;
    use nom_supreme::tag::complete::tag;
    ((tag("new").or(tag("delete"))).and(opt(tag("[]"))))
        .map(|(new_delete, array)| Operator::NewDelete {
            is_delete: new_delete == "delete",
            array: array.is_some(),
        })
        .parse(s)
}

/// Parse custom literal
fn custom_literal(s: &str) -> IResult<Operator> {
    use nom_supreme::tag::complete::tag;
    (tag("\"\" ").and(atoms::identifier))
        .map(|(_, identifier)| Operator::CustomLiteral(identifier))
        .parse(s)
}

/// Parse co_await
fn co_await(s: &str) -> IResult<Operator> {
    use nom_supreme::tag::complete::tag;
    tag("co_await").value(Operator::CoAwait).parse(s)
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

    /// Allocation/deallocation functions
    NewDelete {
        /// new if this is false, delete if this is true
        is_delete: bool,

        /// True if this targets arrays (e.g. "operator new[]")
        array: bool,
    },

    /// Custom literal operator (operator "" <suffix-identifier>)
    CustomLiteral(&'source str),

    /// Overloaded co_await operator
    CoAwait,

    /// Type conversion operator ("operator <type>")
    Conversion(TypeLike<'source>),
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

// FIXME: Add tests
