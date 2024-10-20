//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{
    declarators::{DeclaratorKey, DeclaratorView},
    specifiers::{TypeSpecifier, TypeSpecifierView},
};
use crate::{
    display::{CustomDisplay, DisplayState},
    interning::slice::SliceItemView,
    subparsers::functions::{FunctionArgumentsKey, FunctionArgumentsView},
    EntityParser, IResult,
};
use asylum::lasso::Spur;
use nom::Parser;
use nom_supreme::ParserExt;
use reffers::ARef;
use std::fmt::{self, Display, Formatter};

/// Interned C++ type key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// types as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a type by passing this key to the
/// type_like() method of EntityParser.
///
pub type TypeKey = Spur;
//
impl EntityParser {
    /// Parser recognizing types (and some values that are indistinguishable from
    /// types without extra context).
    pub fn parse_type_like<'source>(&mut self, s: &'source str) -> IResult<'source, TypeKey> {
        self.parse_type_like_imut(s)
    }

    /// Implementation of parse_type_like using internal mutability
    pub(crate) fn parse_type_like_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TypeKey> {
        use nom::{
            character::complete::{char, multispace0},
            combinator::opt,
            sequence::{delimited, tuple},
        };
        use nom_supreme::tag::complete::tag;

        // GNU-style type attributes come first
        let attributes = opt(delimited(
            tag("__attribute__("),
            |s| self.parse_function_call_imut(s),
            char(')'),
        ))
        .map(|opt| opt.unwrap_or_else(|| self.function_arguments.entry().intern()));

        // Then come the type specifier and declarator
        tuple((
            attributes.terminated(multispace0),
            (|s| self.parse_type_specifier_imut(s)).terminated(multispace0),
            |s| self.parse_declarator_imut(s),
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

    /// Access a previously parsed type
    pub fn type_like(&self, t: TypeKey) -> TypeView {
        TypeView::new(t, self)
    }

    /// Retrieve a type previously parsed by parse_type_like
    pub(crate) fn raw_type_like(&self, key: TypeKey) -> ARef<TypeLike> {
        ARef::new(self.types.borrow()).map(|types| types.get(key))
    }

    /// Tell how many unique types have been parsed so far
    pub fn num_types(&self) -> usize {
        self.types.borrow().len()
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
    inner: ARef<'entities, TypeLike>,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> TypeView<'entities> {
    /// Set up a new C++ type view
    pub fn new(key: TypeKey, entities: &'entities EntityParser) -> Self {
        Self {
            key,
            inner: entities.raw_type_like(key),
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
impl PartialEq for TypeView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.key == other.key)
    }
}
//
impl Display for TypeView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for TypeView<'_> {
    fn recursion_depth(&self) -> usize {
        self.attributes()
            .recursion_depth()
            .max(self.type_specifier().recursion_depth())
            .max(self.declarator().recursion_depth())
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        let attributes = self.attributes();
        if !attributes.is_empty() {
            write!(f, "__attribute__(")?;
            attributes.display_impl(f, state)?;
            write!(f, ") ")?;
        }
        self.type_specifier().display_impl(f, state)?;
        let declarator = self.declarator();
        if !declarator.is_empty() {
            declarator.display_impl(f, state)?;
        }
        Ok(())
    }
}
//
impl<'entities> SliceItemView<'entities> for TypeView<'entities> {
    type Inner = TypeKey;

    fn new(inner: Self::Inner, entities: &'entities EntityParser) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "(";

    const DISPLAY_SEPARATOR: &'static str = ", ";

    const DISPLAY_TRAILER: &'static str = "";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn type_like() {
        // FIXME: Rework test harness to test CustomDisplay
        let mut parser = EntityParser::new();
        let attributes = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_function_call(s));
        let type_specifier =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_type_specifier(s));
        let declarator = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_declarator(s));
        let check_type_like = |parser: &mut EntityParser, input, expected, displays| {
            assert_matches!(parser.parse_type_like(input), Ok(("", key)) => {
                assert_eq!(parser.raw_type_like(key).clone(), expected);
                check_custom_display(parser.type_like(key), displays);
            });
        };

        // Basic type specifier
        let mut expected = TypeLike {
            attributes: attributes(&mut parser, "()"),
            type_specifier: type_specifier(&mut parser, "signed char"),
            declarator: declarator(&mut parser, ""),
        };
        check_type_like(&mut parser, "signed char", expected, &["signed char"]);

        // GNU-style attributes before
        expected = TypeLike {
            attributes: attributes(&mut parser, "(unused)"),
            type_specifier: type_specifier(&mut parser, "long long"),
            declarator: declarator(&mut parser, ""),
        };
        check_type_like(
            &mut parser,
            "__attribute__((unused)) long long",
            expected,
            &[
                "__attribute__((…)) long long",
                "__attribute__((unused)) long long",
            ],
        );

        // Basic function pointer
        expected = TypeLike {
            attributes: attributes(&mut parser, "()"),
            type_specifier: type_specifier(&mut parser, "something"),
            declarator: declarator(&mut parser, "()"),
        };
        check_type_like(
            &mut parser,
            "something()",
            expected,
            &["something…", "something()"],
        );

        // Fun template/expression ambiguity found during testing
        expected = TypeLike {
            attributes: attributes(&mut parser, "()"),
            type_specifier: type_specifier(&mut parser, "T<1>"),
            declarator: declarator(&mut parser, "(U)"),
        };
        check_type_like(
            &mut parser,
            "T<1>(U)",
            expected,
            &["T<…>…", "T<1>(…)", "T<1>(U)"],
        );
    }
}
