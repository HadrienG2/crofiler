//! Parsers for operator overloads

use crate::cpp::types::TypeLike;

/// A symbol that can be used at the beginning of a C++ operator name
enum Symbol {
    /// +
    Add,

    /// -
    Sub,

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
    Assign,

    /// <
    Less,

    /// >
    Greater,

    /// ,
    Comma,
}

/// C++ operators that can be overloaded
enum Operator<'source> {
    /// Basic grammar followed by most operators: a symbol that can appear
    /// twice, optionally followed by an equality sign.
    Basic {
        /// Base symbol at the beginning
        symbol: Symbol,

        /// Whether this symbol is repeated
        twice: bool,

        /// Whether this is followed by an equality sign
        equal: bool,
    },

    /// Spaceship operator <=>
    Spaceship,

    /// Dereference operators -> and ->*
    Deref {
        /// -> if this is false, ->* if this is true
        star: bool,
    },

    /// Bracketed operators () and []
    CallIndex {
        /// () if this is false, [] if this is true
        is_index: bool,
    },

    /// Type conversion operator ("operator <type>")
    Conversion(TypeLike<'source>),

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
}

// FIXME: Add tests
