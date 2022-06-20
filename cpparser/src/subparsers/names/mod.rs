//! Id-expression related parsing
//!
//! In the C++ grammar, an id-expressions is a generalization of the notion of
//! identifier that includes qualified identifiers (`path::to::something`) and
//! templated names (`A<B>`).

pub mod atoms;
pub mod scopes;
pub mod unqualified;
