//! Parsers for operator overloads

mod overloads;
pub mod usage;

use crate::{names::atoms::IdentifierKey, types::TypeKey, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

/// C++ operators that can be overloaded
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operator {
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

    /// Dereference operators `->` and `->*`
    Deref {
        /// `->` if this is false, `->*` if this is true
        star: bool,
    },

    /// Spaceship operator `<=>`
    Spaceship,

    /// Bracketed operators `()` and `[]`
    CallIndex {
        /// `()` if this is false, `[]` if this is true
        is_index: bool,
    },

    /// Custom literal operator (`operator "" <suffix-identifier>`)
    CustomLiteral(IdentifierKey),

    /// Allocation/deallocation functions
    NewDelete {
        /// `new` if this is false, `delete` if this is true
        is_delete: bool,

        /// True if this targets arrays (e.g. `operator new[]`)
        array: bool,
    },

    /// Overloaded `co_await` operator
    CoAwait,

    /// Type conversion operator (`operator <type>`)
    Conversion(TypeKey),
}
//
impl From<Symbol> for Operator {
    fn from(symbol: Symbol) -> Self {
        Self::Basic {
            symbol,
            twice: false,
            equal: false,
        }
    }
}
//
impl From<TypeKey> for Operator {
    fn from(t: TypeKey) -> Self {
        Self::Conversion(t)
    }
}

/// Parse arithmetic and comparison operators
///
/// Unfortunately, the grammatically ambiguous nature of characters `<` and `>`
/// strikes here. If a template parameter list can be expected after this
/// operator (as in `operator<<void>`), you will need to call this parser with
/// LEN varying from 1 to 3 in a context where the validity of the overall parse
/// can be assessed.
///
fn arithmetic_or_comparison<const LEN: usize>(s: &str) -> IResult<Operator> {
    use nom::{combinator::map_opt, sequence::tuple};
    match LEN {
        // Single-character operator
        1 => symbol.map(Operator::from).parse(s),

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

            // Pointer dereference ->
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

            // Dereference operator ->*
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

/// Parse deallocation function
fn delete(s: &str) -> IResult<Operator> {
    use nom::{combinator::opt, sequence::preceded};
    use nom_supreme::tag::complete::tag;
    preceded(EntityParser::keyword_parser("delete"), opt(tag("[]")))
        .map(|array| Operator::NewDelete {
            is_delete: true,
            array: array.is_some(),
        })
        .parse(s)
}

/// Parse co_await
fn co_await(s: &str) -> IResult<Operator> {
    EntityParser::keyword_parser("co_await")
        .value(Operator::CoAwait)
        .parse(s)
}

/// Parser for symbols most commonly found in C++ operator names
fn symbol(s: &str) -> IResult<Symbol> {
    use nom::{character::complete::anychar, combinator::map_opt};
    use Symbol::*;
    map_opt(anychar, |c| match c {
        '+' => Some(AddPlus),
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Symbol {
    /// +
    AddPlus,

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
    use pretty_assertions::assert_eq;

    #[test]
    fn symbol() {
        assert_eq!(super::symbol("+"), Ok(("", Symbol::AddPlus)));
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
            Ok(("", Symbol::AddPlus.into()))
        );

        // Symbol with equal sign
        assert_eq!(
            super::arithmetic_or_comparison::<1>("-="),
            Ok(("=", Symbol::SubNeg.into()))
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
            Ok(("<", Symbol::Less.into()))
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
            Ok((">=", Symbol::Greater.into()))
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
            Ok((">", Symbol::SubNeg.into()))
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
    fn delete() {
        assert_eq!(
            super::delete("delete"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: true,
                    array: false
                }
            ))
        );
        assert_eq!(
            super::delete("delete[]"),
            Ok((
                "",
                Operator::NewDelete {
                    is_delete: true,
                    array: true
                }
            ))
        );
    }
}
