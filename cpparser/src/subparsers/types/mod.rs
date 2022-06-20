//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{
    declarators::{DeclaratorKey, DeclaratorView},
    specifiers::{TypeSpecifier, TypeSpecifierView},
};
use crate::{
    display::{CustomDisplay, DisplayState, RecursionDepths},
    interning::slice::SliceItemView,
    subparsers::functions::{FunctionArgumentsKey, FunctionArgumentsView},
    Entities, EntityParser, IResult,
};
use asylum::lasso::Spur;
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

/// Interned C++ type key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// types as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a type by passing this key to the
/// type_like() method of the Entities struct.
///
pub type TypeKey = Spur;
//
impl EntityParser {
    /// Parser recognizing types (and some values that are indistinguishable from
    /// types without extra context).
    pub fn parse_type_like<'source>(&self, s: &'source str) -> IResult<'source, TypeKey> {
        use nom::{
            character::complete::{char, space0},
            combinator::opt,
            sequence::{delimited, tuple},
        };
        use nom_supreme::tag::complete::tag;

        // GNU-style type attributes come first
        let attributes = opt(delimited(
            tag("__attribute__("),
            |s| self.parse_function_call(s),
            char(')'),
        ))
        .map(|opt| opt.unwrap_or_else(|| self.function_arguments.entry().intern()));

        // Then come the type specifier and declarator
        tuple((
            attributes.terminated(space0),
            (|s| self.parse_type_specifier(s)).terminated(space0),
            |s| self.parse_declarator(s),
        ))
        .map(|(attributes, type_specifier, declarator)| {
            self.types.borrow_mut().intern(TypeLike {
                attributes,
                type_specifier,
                declarator,
            })
        })
        .parse(s)
    }

    /// Retrieve a type previously parsed by parse_type_like
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    #[cfg(test)]
    pub(crate) fn type_like(&self, key: TypeKey) -> TypeLike {
        self.types.borrow().get(key).clone()
    }

    /// Tell how many unique types have been parsed so far
    pub fn num_types(&self) -> usize {
        self.types.borrow().len()
    }
}
//
impl Entities {
    /// Access a previously parsed type
    pub fn type_like(&self, t: TypeKey) -> TypeView {
        TypeView::new(t, self)
    }
}

/// A type name, or something looking close enough to it
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TypeLike {
    /// GNU-style attributes (`__attribute__((...))`)
    attributes: FunctionArgumentsKey,

    /// Type specifier
    type_specifier: TypeSpecifier,

    /// Declarator
    declarator: DeclaratorKey,
}

/// A view of a C++ type (or something else that honors the type grammar)
pub struct TypeView<'entities> {
    /// Key used to retrieve the type
    key: TypeKey,

    /// Wrapped TypeLike
    inner: &'entities TypeLike,

    /// Underlying interned entity storage
    entities: &'entities Entities,
}
//
impl<'entities> TypeView<'entities> {
    /// Set up a new C++ type view
    pub fn new(key: TypeKey, entities: &'entities Entities) -> Self {
        Self {
            key,
            inner: entities.types.get(key),
            entities,
        }
    }

    /// GNU-style attributes (`__attribute__((...))`)
    pub fn attributes(&self) -> FunctionArgumentsView {
        self.entities.function_arguments(self.inner.attributes)
    }

    /// Type specifier
    pub fn type_specifier(&self) -> TypeSpecifierView {
        self.entities.type_specifier(self.inner.type_specifier)
    }

    /// Declarator
    pub fn declarator(&self) -> DeclaratorView {
        self.entities.declarator(self.inner.declarator)
    }
}
//
impl<'entities> PartialEq for TypeView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const Entities == other.entities as *const Entities)
            && (self.key == other.key)
    }
}
//
impl<'entities> Display for TypeView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for TypeView<'entities> {
    fn recursion_depths(&self) -> RecursionDepths {
        self.attributes()
            .recursion_depths()
            .max(self.type_specifier().recursion_depths())
            .max(self.declarator().recursion_depths())
    }

    fn display(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        let attributes = self.attributes();
        if !attributes.is_empty() {
            write!(f, "__attribute__(")?;
            attributes.display(f, state)?;
            write!(f, ")")?;
        }
        self.type_specifier().display(f, state)?;
        let declarator = self.declarator();
        if !declarator.is_empty() {
            write!(f, " ")?;
            declarator.display(f, state)?;
        }
        Ok(())
    }
}
//
impl<'entities> SliceItemView<'entities> for TypeView<'entities> {
    type Inner = TypeKey;

    fn new(inner: Self::Inner, entities: &'entities Entities) -> Self {
        Self::new(inner, entities)
    }

    fn get_recursion_depth(depths: &mut RecursionDepths) -> &mut usize {
        &mut depths.function_parameters
    }

    const DISPLAY_HEADER: &'static str = "(";

    const DISPLAY_SEPARATOR: &'static str = ", ";

    const DISPLAY_TRAILER: &'static str = ")";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn type_like() {
        let parser = EntityParser::new();
        let parse_type_like = |s| {
            parser
                .parse_type_like(s)
                .map(|(rest, key)| (rest, parser.type_like(key)))
        };
        let attributes = |s| unwrap_parse(parser.parse_function_call(s));
        let type_specifier = |s| unwrap_parse(parser.parse_type_specifier(s));
        let declarator = |s| unwrap_parse(parser.parse_declarator(s));

        // Basic type specifier
        assert_eq!(
            parse_type_like("signed char"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("()"),
                    type_specifier: type_specifier("signed char"),
                    declarator: declarator("")
                }
            ))
        );

        // GNU-style attributes before
        assert_eq!(
            parse_type_like("__attribute__((unused)) long long"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("(unused)"),
                    type_specifier: type_specifier("long long"),
                    declarator: declarator("")
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            parse_type_like("something()"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("()"),
                    type_specifier: type_specifier("something"),
                    declarator: declarator("()"),
                }
            ))
        );

        // Fun template/expression ambiguity found during testing
        assert_eq!(
            parse_type_like("T<1>(U)"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("()"),
                    type_specifier: type_specifier("T<1>"),
                    declarator: declarator("(U)"),
                }
            ))
        );
    }
}
