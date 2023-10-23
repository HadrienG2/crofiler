//! Handling of hierarchical scopes

use super::unqualified::{UnqualifiedId, UnqualifiedIdView};
use crate::{
    display::{CustomDisplay, DisplayState},
    interning::{
        recursion::SequenceEntry,
        slice::{SliceItemView, SliceView},
    },
    subparsers::{
        functions::{FunctionSignature, FunctionSignatureView},
        types::qualifiers::{ConstVolatile, Reference},
    },
    EntityParser, IResult,
};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

#[cfg(test)]
use reffers::ARef;

/// Interned Scope sequence key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// Scope sequences as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a scope sequence by passing this key
/// to the scopes() method of EntityParser.
///
pub type ScopesKey = SequenceKey<ScopesKeyImpl, SCOPES_LEN_BITS>;
type ScopesKeyImpl = Spur;
const SCOPES_LEN_BITS: u32 = 8;
//
impl EntityParser {
    /// Parser for id-expressions (= nested name-specifier + UnqualifiedId)
    pub fn parse_id_expression<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, IdExpression> {
        self.parse_id_expression_imut(s)
    }

    /// Implementation of parse_id_expression with internal mutability
    pub(crate) fn parse_id_expression_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, IdExpression> {
        use nom::combinator::map_opt;
        map_opt(
            |s| self.parse_proto_id_expression_imut(s),
            |(path, id_opt)| id_opt.map(|(_backtrack, id)| IdExpression { path, id }),
        )(s)
    }

    /// Access a previously parsed id-expression
    pub fn id_expression(&self, id: IdExpression) -> IdExpressionView {
        IdExpressionView::new(id, self)
    }

    /// Parser for nested name-specifiers (= sequences of scopes).
    ///
    /// If you expect an UnqualifiedId after your nested-name-specifier, then what
    /// you are looking for is an id-expression, and you should use the dedicated
    /// id_expression parser for optimal performance.
    ///
    pub fn parse_nested_name_specifier<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, NestedNameSpecifier> {
        self.parse_nested_name_specifier_imut(s)
    }

    /// Implementation of parse_nested_name_specifier using internal mutability
    pub(crate) fn parse_nested_name_specifier_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, NestedNameSpecifier> {
        match self.parse_proto_id_expression_imut(s) {
            Ok((_rest, (path, Some((backtrack, _id))))) => Ok((backtrack, path)),
            Ok((rest, (path, None))) => Ok((rest, path)),
            Err(error) => Err(error),
        }
    }

    /// Access a previously parsed nested name specifier
    pub fn nested_name_specifier(&self, nns: NestedNameSpecifier) -> NestedNameSpecifierView {
        NestedNameSpecifierView::new(nns, self)
    }

    /// Parser for a nested name-specifier, optionally followed by an UnqualifiedId,
    /// with an option to backtrack on that last UnqualifiedId if you don't want it.
    ///
    /// If you make the final UnqualifiedId mandatory, you get the id_expression
    /// grammar and if you backtrack on it you get the nested_name_specifier one.
    ///
    #[inline]
    fn parse_proto_id_expression_imut<'source>(
        &self,
        mut input: &'source str,
    ) -> IResult<'source, (NestedNameSpecifier, Option<(&'source str, UnqualifiedId)>)> {
        // Truth that the path starts at root scope (with a leading ::)
        let rooted = if let Some(rest) = input.strip_prefix("::") {
            input = rest;
            true
        } else {
            false
        };

        // Parse sequence of scope_or_unqualified_id, accumulating scopes
        let mut scopes = self.scope_sequences.entry();
        let make_output = |scopes: SequenceEntry<Scope, ScopesKey>, id_opt| {
            (
                NestedNameSpecifier {
                    rooted,
                    scopes: scopes.intern(),
                },
                id_opt,
            )
        };
        //
        while let Ok((rest, scope_or_id)) = self.parse_scope_or_unqualified_id_imut(input) {
            match scope_or_id {
                // As long as there are scopes, keep going
                ScopeOrUnqualifiedId::Scope(scope) => scopes.push(scope),

                // If a trailing UnqualifiedId is found, we reached the end of the
                // grammar, return it + input string to allow backtracking
                ScopeOrUnqualifiedId::UnqualifiedId(id) => {
                    return Ok((rest, make_output(scopes, Some((input, id)))));
                }
            }

            // Keep consuming input
            input = rest;
        }

        // If control reaches this point, no trailing unqualified-id was found, the
        // scope list ended on its own
        Ok((input, make_output(scopes, None)))
    }

    /// Access a previously parsed sequence of scopes
    pub(crate) fn scope_sequence(&self, key: ScopesKey) -> ScopesView {
        ScopesView::new(key, self.scope_sequences.borrow(), self)
    }

    /// Retrieve a scope sequence previously parsed by parse_proto_id_expression
    #[cfg(test)]
    pub(crate) fn raw_scope_sequence(&self, key: ScopesKey) -> ARef<[Scope]> {
        self.scope_sequences.get(key)
    }

    /// Total number of Scopes across all interned nested name specifiers so far
    pub fn num_scopes(&self) -> usize {
        self.scope_sequences.borrow().num_items()
    }

    /// Maximal number of scopes in a nested name specifier
    pub fn max_scope_sequence_len(&self) -> Option<usize> {
        self.scope_sequences.borrow().max_sequence_len()
    }

    /// This parses either the Scope syntax or the UnqualifiedId syntax, in a manner
    /// that avoids parsing the shared UnqualifiedId syntax twice.
    #[inline]
    fn parse_scope_or_unqualified_id_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, ScopeOrUnqualifiedId> {
        use nom::{character::complete::multispace0, combinator::opt, multi::many0_count};
        use nom_supreme::tag::complete::tag;
        // Parse the initial UnqualifiedId
        match self.parse_unqualified_id_imut(s) {
            // An UnqualifiedId was found, but is this actually a Scope?
            Ok((after_id, id)) => {
                match opt(|s| self.parse_function_signature_imut(s))
                    // Ignore any declarator other than a function signature:
                    // - T* does not have members so it is meaningless as a scope
                    // - T& has the same members as T, ditto for const T
                    // - T[] does not have members so its is meaningless as a scope
                    // - T() was accounted for above
                    // - Since no declarator makes sense (other than a signature)
                    //   a parenthesized declarator won't make sense either.
                    .terminated(
                        many0_count(
                            multispace0.and(
                                (Self::parse_cv
                                    .verify(|&cv| cv != ConstVolatile::default())
                                    .value(()))
                                .or(Self::parse_reference
                                    .verify(|&r| r != Reference::None)
                                    .value(())),
                            ),
                        )
                        .and(tag("::")),
                    )
                    .parse(after_id)
                {
                    // Yes, return the Scope
                    Ok((after_scope, function_signature)) => Ok((
                        after_scope,
                        ScopeOrUnqualifiedId::Scope(Scope {
                            id,
                            function_signature,
                        }),
                    )),

                    // No, return the initial UnqualifiedId as if nothing else happened
                    Err(_) => Ok((after_id, ScopeOrUnqualifiedId::UnqualifiedId(id))),
                }
            }

            // If there is no UnqualifiedId, there is also no Scope
            Err(error) => Err(error),
        }
    }

    /// Access a scope previously parsed by parse_scope_or_unqualified_id
    #[cfg(test)]
    fn scope(&self, scope: Scope) -> ScopeView {
        ScopeView::new(scope, self)
    }
}

/// C++ id-expression
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct IdExpression {
    /// Hierarchical scope
    path: NestedNameSpecifier,

    /// Inner unqualified id-expression
    id: UnqualifiedId,
}

/// View of an id-expression
pub struct IdExpressionView<'entities> {
    /// Wrapped IdExpression
    inner: IdExpression,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> IdExpressionView<'entities> {
    /// Build an id-expression view
    pub fn new(inner: IdExpression, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Hierarchical scope
    pub fn path(&self) -> NestedNameSpecifierView {
        self.entities.nested_name_specifier(self.inner.path)
    }

    /// Inner unqualified id-expression
    pub fn id(&self) -> UnqualifiedIdView {
        self.entities.unqualified_id(self.inner.id)
    }
}
//
impl<'entities> PartialEq for IdExpressionView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for IdExpressionView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for IdExpressionView<'entities> {
    fn recursion_depth(&self) -> usize {
        self.path()
            .recursion_depth()
            .max(self.id().recursion_depth())
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        self.path().display_impl(f, state)?;
        self.id().display_impl(f, state)
    }
}

/// A nested name specifier
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct NestedNameSpecifier {
    /// Truth that the path starts at the root scope (leading ::)
    rooted: bool,

    /// Sequence of inner scopes
    scopes: ScopesKey,
}
//
impl From<ScopesKey> for NestedNameSpecifier {
    fn from(scopes: ScopesKey) -> Self {
        Self {
            rooted: false,
            scopes,
        }
    }
}

/// View of a nested name specifier
pub struct NestedNameSpecifierView<'entities> {
    /// Wrapped NestedNameSpecifier
    inner: NestedNameSpecifier,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> NestedNameSpecifierView<'entities> {
    /// Build a nested name specifier view
    pub fn new(inner: NestedNameSpecifier, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Truth that the path starts at the root scope (leading ::)
    pub fn is_rooted(&self) -> bool {
        self.inner.rooted
    }

    /// Sequence of inner scopes
    pub fn scopes(&self) -> ScopesView {
        self.entities.scope_sequence(self.inner.scopes)
    }
}
//
impl<'entities> PartialEq for NestedNameSpecifierView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for NestedNameSpecifierView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for NestedNameSpecifierView<'entities> {
    fn recursion_depth(&self) -> usize {
        self.scopes().recursion_depth()
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if state.can_recurse() {
            if self.is_rooted() {
                write!(f, "::")?;
            }
            self.scopes().display_impl(f, state)?;
        } else if !self.scopes().is_empty() {
            write!(f, "…::")?;
        }
        Ok(())
    }
}

/// View of a sequence of scopes
pub type ScopesView<'entities> = SliceView<'entities, Scope, ScopeView<'entities>, ScopesKey>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::large_enum_variant)]
enum ScopeOrUnqualifiedId {
    Scope(Scope),
    UnqualifiedId(UnqualifiedId),
}

/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong) without recursion (that's NestedNameSpecifier).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Scope {
    /// What identifies the scope
    id: UnqualifiedId,

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    function_signature: Option<FunctionSignature>,
}
//
impl<T: Into<UnqualifiedId>> From<T> for Scope {
    fn from(id: T) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }
}

/// View of a Scope
pub struct ScopeView<'entities> {
    /// Wrapped Scope
    inner: Scope,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> ScopeView<'entities> {
    /// Build a scope view
    pub fn new(inner: Scope, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// What identifies the scope
    pub fn id(&self) -> UnqualifiedIdView {
        self.entities.unqualified_id(self.inner.id)
    }

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    pub fn function_signature(&self) -> Option<FunctionSignatureView> {
        self.inner
            .function_signature
            .map(|s| self.entities.function_signature(s))
    }
}
//
impl<'entities> PartialEq for ScopeView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for ScopeView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for ScopeView<'entities> {
    fn recursion_depth(&self) -> usize {
        (self.id().recursion_depth()).max(self.function_signature().recursion_depth())
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        self.id().display_impl(f, state)?;
        self.function_signature().display_impl(f, state)?;
        write!(f, "::")
    }
}
//
impl<'entities> SliceItemView<'entities> for ScopeView<'entities> {
    type Inner = Scope;

    fn new(inner: Self::Inner, entities: &'entities EntityParser) -> Self {
        Self::new(inner, entities)
    }

    const DISPLAY_HEADER: &'static str = "";

    const DISPLAY_SEPARATOR: &'static str = "";

    const DISPLAY_TRAILER: &'static str = "";
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
    use pretty_assertions::assert_eq;

    #[test]
    fn scope_or_unqualified_id() {
        let mut parser = EntityParser::new();
        let unqualified_id =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_unqualified_id(s));
        let check_scope = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(
                parser.parse_scope_or_unqualified_id_imut(input),
                Ok(("", ScopeOrUnqualifiedId::Scope(expected)))
            );
            check_custom_display(parser.scope(expected), displays);
        };

        // Without function signature
        let mut expected = unqualified_id(&mut parser, "std").into();
        check_scope(&mut parser, "std::", expected, &["std::"]);

        // With function signature
        expected = Scope {
            id: unqualified_id(&mut parser, "my_function"),
            function_signature: Some(unwrap_parse(parser.parse_function_signature("()"))),
        };
        check_scope(
            &mut parser,
            "my_function()::",
            expected,
            &["my_function()::"],
        );

        // Scope CV qualifiers are ignored (it's the same scope)
        expected = unqualified_id(&mut parser, "MyClass").into();
        check_scope(&mut parser, "MyClass const::", expected, &["MyClass::"]);

        // Reference qualifiers are ignored (it's the same scope)
        expected = unqualified_id(&mut parser, "Something").into();
        check_scope(&mut parser, "Something&::", expected, &["Something::"]);

        // Without scope terminator, that's an UnqualifiedId
        assert_eq!(
            parser.parse_scope_or_unqualified_id_imut("std"),
            Ok((
                "",
                ScopeOrUnqualifiedId::UnqualifiedId(unqualified_id(&mut parser, "std"))
            ))
        );
    }

    #[test]
    fn proto_id_expression() {
        let mut parser = EntityParser::new();
        let unqualified_id =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_unqualified_id(s));
        let scopes = |parser: &mut EntityParser, ss: &[&str]| -> ScopesKey {
            // Generate scope sequence
            let mut entry = parser.scope_sequences.entry();
            let scopes: Vec<Scope> = ss
                .iter()
                .map(|s| Scope::from(unwrap_parse(parser.parse_unqualified_id_imut(s))))
                .collect();

            // Intern it
            for &scope in scopes.iter() {
                entry.push(scope);
            }
            let key = entry.intern();

            // Make sure it can be correctly retrieved before returning it
            for (input, result) in scopes
                .into_iter()
                .zip(parser.raw_scope_sequence(key).to_vec())
            {
                assert_eq!(input, result);
            }
            key
        };

        assert_eq!(
            parser.parse_proto_id_expression_imut(""),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&mut parser, &[]),
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("something"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&mut parser, &[]),
                    },
                    Some(("something", unqualified_id(&mut parser, "something")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: scopes(&mut parser, &[]),
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("::x"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: scopes(&mut parser, &[]),
                    },
                    Some(("x", unqualified_id(&mut parser, "x")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("boost::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&mut parser, &["boost"]),
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("boost::y"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&mut parser, &["boost"]),
                    },
                    Some(("y", unqualified_id(&mut parser, "y")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("::std::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: scopes(&mut parser, &["std"])
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("::std::z 1"),
            Ok((
                " 1",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: scopes(&mut parser, &["std"])
                    },
                    Some(("z 1", unqualified_id(&mut parser, "z")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("boost::hana::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&mut parser, &["boost", "hana"]),
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression_imut("boost::hana::stuff"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&mut parser, &["boost", "hana"]),
                    },
                    Some(("stuff", unqualified_id(&mut parser, "stuff")))
                )
            ))
        );
    }

    #[test]
    fn nested_name_specifier() {
        let mut parser = EntityParser::new();
        let scopes = |parser: &mut EntityParser, ss: &[&str]| -> ScopesKey {
            let mut entry = parser.scope_sequences.entry();
            for scope in ss
                .iter()
                .map(|s| Scope::from(unwrap_parse(parser.parse_unqualified_id_imut(s))))
            {
                entry.push(scope);
            }
            entry.intern()
        };
        let check_nested_name_specifier = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parser.parse_nested_name_specifier(input), Ok(expected));
            check_custom_display(parser.nested_name_specifier(expected.1), displays);
        };

        let mut expected = (
            "",
            NestedNameSpecifier {
                rooted: false,
                scopes: scopes(&mut parser, &[]),
            },
        );
        check_nested_name_specifier(&mut parser, "", expected, &[""]);

        expected = (
            "",
            NestedNameSpecifier {
                rooted: false,
                scopes: scopes(&mut parser, &["boost", "hana"]),
            },
        );
        check_nested_name_specifier(
            &mut parser,
            "boost::hana::",
            expected,
            &["…::", "boost::hana::"],
        );

        expected = (
            "stuff",
            NestedNameSpecifier {
                rooted: true,
                scopes: scopes(&mut parser, &["boost", "hana"]),
            },
        );
        check_nested_name_specifier(
            &mut parser,
            "::boost::hana::stuff",
            expected,
            &["…::", "::boost::hana::"],
        );
    }

    #[test]
    fn id_expression() {
        let mut parser = EntityParser::new();
        let unqualified_id =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_unqualified_id(s));
        let check_id_expression = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parser.parse_id_expression(input), Ok(("", expected)));
            check_custom_display(parser.id_expression(expected), displays);
        };

        // Without any path
        let mut expected = IdExpression {
            path: NestedNameSpecifier {
                rooted: false,
                scopes: parser.scope_sequences.entry().intern(),
            },
            id: unqualified_id(&mut parser, "something"),
        };
        check_id_expression(&mut parser, "something", expected, &["something"]);

        // With a path
        expected = IdExpression {
            path: unwrap_parse(parser.parse_nested_name_specifier("boost::hana::")),
            id: unqualified_id(&mut parser, "to_t<unsigned long long>"),
        };
        check_id_expression(
            &mut parser,
            "boost::hana::to_t<unsigned long long>",
            expected,
            &["…::to_t<…>", "boost::hana::to_t<unsigned long long>"],
        );
    }
}
