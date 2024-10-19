//! Clang-provided names to C++ entities that don't have a language-defined name
//! including lambdas, anonymous classes, anonymous namespaces...

use crate::{
    display::{CustomDisplay, DisplayState},
    subparsers::{
        functions::{FunctionSignature, FunctionSignatureView},
        names::atoms::{IdentifierKey, IdentifierView},
    },
    EntityParser, IResult, InternedPath, PathKey,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

impl EntityParser {
    /// Parser for clang's `<unknown>` C++ entity, sometimes seen in ParseTemplate
    pub fn parse_unknown_entity(s: &str) -> IResult<()> {
        use nom_supreme::tag::complete::tag;
        tag("<unknown>").value(()).parse(s)
    }

    /// Parser for anonymous types including lambdas
    pub fn parse_anonymous<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, AnonymousEntity> {
        self.parse_anonymous_imut(s)
    }

    /// Access a previously parsed anonymous entity
    pub fn anonymous(&self, a: AnonymousEntity) -> AnonymousEntityView {
        AnonymousEntityView::new(a, self)
    }

    /// Implementation of parse_anonymous with internal mutability
    pub(crate) fn parse_anonymous_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, AnonymousEntity> {
        match s.as_bytes().first() {
            Some(b'(') => ((|s| self.parse_clang_lambda_imut(s)).map(AnonymousEntity::ClangLambda))
                .or((|s| self.parse_clang_anonymous_imut(s)).map(AnonymousEntity::ClangOther))
                .parse(s),
            _ => ((|s| self.parse_libiberty_lambda_imut(s)).map(AnonymousEntity::LibibertyLambda))
                .or((|s| self.parse_libiberty_unnamed_imut(s))
                    .map(AnonymousEntity::LibibertyUnnamed))
                .parse(s),
        }
    }

    /// Parser for clang lambda types `(lambda at <file path>:<line>:<col>)`
    fn parse_clang_lambda_imut<'source>(&self, s: &'source str) -> IResult<'source, ClangLambda> {
        use nom::{bytes::complete::tag, character::complete::char, sequence::delimited};
        delimited(
            tag("(lambda at "),
            |s| self.parse_source_location_imut(s),
            char(')'),
        )
        .map(ClangLambda)
        .parse(s)
    }

    /// Access a previously parsed clang-style lambda
    pub(crate) fn clang_lambda(&self, l: ClangLambda) -> ClangLambdaView {
        ClangLambdaView::new(l, self)
    }

    /// Parser for libiberty lambda types `{lambda(<param>, ...)#<id>}`
    fn parse_libiberty_lambda_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, LibibertyLambda> {
        use nom::{
            character::complete::{char, u32},
            sequence::{delimited, preceded},
        };
        use nom_supreme::tag::complete::tag;
        let signature = preceded(tag("{lambda"), |s| self.parse_function_signature_imut(s));
        let id = delimited(char('#'), u32, char('}'));
        signature
            .and(id)
            .map(|(signature, id)| LibibertyLambda { signature, id })
            .parse(s)
    }

    /// Access a previously parsed libiberty-style lambda
    pub(crate) fn libiberty_lambda(&self, l: LibibertyLambda) -> LibibertyLambdaView {
        LibibertyLambdaView::new(l, self)
    }

    /// Parser for anonymous clang entities like `(anonymous <stuff>)`
    pub(crate) fn parse_clang_anonymous_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, ClangAnonymousEntity> {
        use nom::{
            character::complete::char,
            combinator::opt,
            sequence::{delimited, preceded},
        };
        use nom_supreme::tag::complete::tag;
        delimited(
            char('(').and(tag("anonymous").or(tag("unnamed"))),
            opt(preceded(char(' '), |s| self.parse_identifier_imut(s)))
                .and(opt(preceded(tag(" at "), |s| {
                    self.parse_source_location_imut(s)
                }))),
            char(')'),
        )
        .map(|(identifier, location)| ClangAnonymousEntity {
            identifier,
            location,
        })
        .parse(s)
    }

    /// Access a previously parsed anonymous clang entity
    pub(crate) fn clang_anonymous(&self, a: ClangAnonymousEntity) -> ClangAnonymousEntityView {
        ClangAnonymousEntityView::new(a, self)
    }

    /// Parser for a libiberty-style anonymous type
    pub(crate) fn parse_libiberty_unnamed_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, LibibertyUnnamedType> {
        use nom::{
            character::complete::{char, u32},
            sequence::delimited,
        };
        use nom_supreme::tag::complete::tag;
        delimited(tag("{unnamed type#"), u32, char('}'))
            .map(LibibertyUnnamedType)
            .parse(s)
    }

    /// Parser for source code locations `<file path>:<line>:<col>`
    ///
    /// This will fail if the file path contains a ':' sign other than a
    /// Windows-style disk designator at the start, because I have no idea how
    /// to handle this inherent grammar ambiguity better...
    ///
    fn parse_source_location_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, SourceLocation> {
        use nom::{
            bytes::complete::take_till1,
            character::complete::{anychar, char, u32},
            combinator::{opt, recognize},
            sequence::separated_pair,
        };

        let location = separated_pair(u32, char(':'), u32);

        let disk_designator = anychar.and(char(':'));
        let path_str = recognize(opt(disk_designator).and(take_till1(|c| c == ':')));
        let path = path_str.map(|path| self.intern_path_imut(path));

        let source_location = separated_pair(path, char(':'), location);
        source_location
            .map(|(file, location)| SourceLocation { file, location })
            .parse(s)
    }

    /// Access a previously parsed source code location
    pub(crate) fn source_location(&self, sl: SourceLocation) -> SourceLocationView {
        SourceLocationView::new(sl, self)
    }
}

/// Anonymous entities including lambdas
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AnonymousEntity {
    /// Clang-style lambda with source location description `(lambda at ...)`
    ClangLambda(ClangLambda),

    /// Clang-style non-lambda anonymous entity `(anonymous <stuff>)`
    ClangOther(ClangAnonymousEntity),

    /// Libiberty-style lambda with parameter types and numeric ID `{lambda(...)#1}`
    LibibertyLambda(LibibertyLambda),

    /// Libiberty-style unnamed type `{unamed type#123}`
    LibibertyUnnamed(LibibertyUnnamedType),
}
//
impl Default for AnonymousEntity {
    fn default() -> Self {
        Self::ClangOther(ClangAnonymousEntity::default())
    }
}

/// View of a lambda function
#[derive(PartialEq)]
pub enum AnonymousEntityView<'entities> {
    /// Clang-style lambda with source location description `(lambda at ...)`
    ClangLambda(ClangLambdaView<'entities>),

    /// Clang-style non-lambda anonymous entity `(anonymous <stuff>)`
    ClangOther(ClangAnonymousEntityView<'entities>),

    /// Libiberty-style lambda with parameter types and numeric ID `{lambda(...)#1}`
    LibibertyLambda(LibibertyLambdaView<'entities>),

    /// Libiberty-style unnamed type `{unamed type#123}`
    LibibertyUnnamed(LibibertyUnnamedType),
}
//
impl<'entities> AnonymousEntityView<'entities> {
    /// Set up a lambda view
    pub(crate) fn new(inner: AnonymousEntity, entities: &'entities EntityParser) -> Self {
        match inner {
            AnonymousEntity::ClangLambda(c) => Self::ClangLambda(entities.clang_lambda(c)),
            AnonymousEntity::ClangOther(o) => Self::ClangOther(entities.clang_anonymous(o)),
            AnonymousEntity::LibibertyLambda(l) => {
                Self::LibibertyLambda(entities.libiberty_lambda(l))
            }
            AnonymousEntity::LibibertyUnnamed(u) => Self::LibibertyUnnamed(u),
        }
    }
}
//
impl Display for AnonymousEntityView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for AnonymousEntityView<'_> {
    fn recursion_depth(&self) -> usize {
        match self {
            // FIXME: Bring in simplified path display from crofiler
            Self::ClangLambda(_) => 0,
            Self::ClangOther(_) => 0,
            Self::LibibertyLambda(l) => l.recursion_depth(),
            Self::LibibertyUnnamed(_) => 0,
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            Self::ClangLambda(c) => write!(f, "{c}"),
            Self::ClangOther(o) => write!(f, "{o}"),
            Self::LibibertyLambda(l) => l.display_impl(f, state),
            Self::LibibertyUnnamed(u) => write!(f, "{u}"),
        }
    }
}

/// A location within the source code
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SourceLocation {
    /// Source file in which the object of interest is located
    file: PathKey,

    /// Object location within the file
    location: (Line, Column),
}
//
/// Line number within a file
pub type Line = u32;
//
/// Column number within a file
pub type Column = u32;

/// View of a source code location
pub struct SourceLocationView<'entities> {
    /// Wrapped ClangLambda
    inner: SourceLocation,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> SourceLocationView<'entities> {
    /// Build a source location view
    pub fn new(inner: SourceLocation, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Source file in which the lambda is declared
    pub fn file(&self) -> InternedPath {
        self.entities.path(self.inner.file)
    }

    /// Declaration location within the file
    pub fn location(&self) -> (Line, Column) {
        self.inner.location
    }
}
//
impl PartialEq for SourceLocationView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl Display for SourceLocationView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        let file = self.file();
        let (line, column) = self.location();
        write!(f, "{file}:{line}:{column}")
    }
}

/// Clang-style lambda featuring source location description
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ClangLambda(SourceLocation);

/// View of a clang-style lambda location description
pub struct ClangLambdaView<'entities> {
    /// Wrapped ClangLambda
    inner: ClangLambda,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> ClangLambdaView<'entities> {
    /// Build a clang lambda
    pub fn new(inner: ClangLambda, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Source code location in which the lambda is declared
    pub fn source_location(&self) -> SourceLocationView {
        self.entities.source_location(self.inner.0)
    }
}
//
impl PartialEq for ClangLambdaView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl Display for ClangLambdaView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "(lambda at {})", self.source_location())
    }
}

/// Libiberty-style lambda featuring parameter types and a numeric ID
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct LibibertyLambda {
    /// Parameter types
    signature: FunctionSignature,

    /// Numeric identifier
    id: u32,
}

/// View of a libiberty-style lambda
pub struct LibibertyLambdaView<'entities> {
    /// Wrapped LibibertyLambda
    inner: LibibertyLambda,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> LibibertyLambdaView<'entities> {
    /// Build a libiberty lambda view
    pub fn new(inner: LibibertyLambda, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Parameter types
    pub fn signature(&self) -> FunctionSignatureView {
        self.entities.function_signature(self.inner.signature)
    }

    /// Numeric identifier
    pub fn id(&self) -> u32 {
        self.inner.id
    }
}
//
impl PartialEq for LibibertyLambdaView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl Display for LibibertyLambdaView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for LibibertyLambdaView<'_> {
    fn recursion_depth(&self) -> usize {
        self.signature().recursion_depth()
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        write!(f, "{{lambda")?;
        self.signature().display_impl(f, state)?;
        write!(f, "#{}}}", self.id())
    }
}

/// Anonymous clang entity
///
/// This models clang's use of `(anonymous)`, `(anonymous <something>)` and
/// `(anonymous <something> at <source location>)` to point to C++ entities that
/// don't have names, with "anonymous" sometimes replaced with "unnamed".
///
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ClangAnonymousEntity {
    /// What we're talking about (namespace, class, enum, union...)
    identifier: Option<IdentifierKey>,

    /// Where in the source code that thing is located
    location: Option<SourceLocation>,
}

/// View of an anonymous clang entity
pub struct ClangAnonymousEntityView<'entities> {
    /// Wrapped AnonymousClangEntity
    inner: ClangAnonymousEntity,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> ClangAnonymousEntityView<'entities> {
    /// Build an anonymous entity view
    pub fn new(inner: ClangAnonymousEntity, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// Clarify what we're talking about (namespace, class, enum, union...), if
    /// clang provided this information
    pub fn identifier(&self) -> Option<IdentifierView> {
        self.inner
            .identifier
            .map(|identifier| self.entities.identifier(identifier))
    }

    /// Clarify where the entity is located in source code (if clang told us)
    pub fn source_location(&self) -> Option<SourceLocationView> {
        self.inner
            .location
            .map(|location| self.entities.source_location(location))
    }
}
//
impl PartialEq for ClangAnonymousEntityView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl Display for ClangAnonymousEntityView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "(anonymous")?;
        if let Some(id) = self.identifier() {
            write!(f, " {id}")?;
        } else if self.inner.location.is_some() {
            write!(f, " entity")?;
        }
        if let Some(location) = self.source_location() {
            write!(f, " at {location}")?;
        }
        write!(f, ")")
    }
}

/// Libiberty-style anonymous type `{unnamed type#N}`
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct LibibertyUnnamedType(u32);
//
impl Display for LibibertyUnnamedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{{unnamed type#{}}}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
    use pretty_assertions::assert_eq;

    #[test]
    fn unknown_entity() {
        assert_eq!(
            EntityParser::parse_unknown_entity("<unknown>"),
            Ok(("", ()))
        );
    }

    fn test_location(parser: &mut EntityParser) -> (String, SourceLocation) {
        let file_path = if cfg!(target_os = "windows") {
            "c:/source.cpp"
        } else {
            "/path/to/source.cpp"
        };
        let expected = SourceLocation {
            file: parser.intern_path(file_path),
            location: (123, 45),
        };
        let location = format!("{file_path}:123:45");
        (location, expected)
    }

    #[test]
    fn source_location() {
        let mut parser = EntityParser::new();
        let (location, expected) = test_location(&mut parser);
        assert_eq!(
            parser.parse_source_location_imut(&location),
            Ok(("", expected))
        );
        assert_eq!(parser.source_location(expected).to_string(), location);
    }

    #[test]
    fn clang_lambda() {
        let mut parser = EntityParser::new();
        let (location_str, location) = test_location(&mut parser);
        let lambda = format!("(lambda at {location_str})");
        let expected = ClangLambda(location);
        assert_eq!(parser.parse_clang_lambda_imut(&lambda), Ok(("", expected)));
        assert_eq!(parser.clang_lambda(expected).to_string(), lambda);
    }

    #[test]
    fn libiberty_lambda() {
        let mut parser = EntityParser::new();
        let expected = LibibertyLambda {
            signature: unwrap_parse(parser.parse_function_signature("(auto:1)")),
            id: 1,
        };
        assert_eq!(
            parser.parse_libiberty_lambda_imut("{lambda(auto:1)#1}"),
            Ok(("", expected))
        );
        check_custom_display(
            parser.libiberty_lambda(expected),
            &["{lambda(…)#1}", "{lambda(auto:1)#1}"],
        );
    }

    #[test]
    fn clang_anonymous() {
        let mut parser = EntityParser::new();
        let identifier = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_identifier(s));
        let check_anonymous = |parser: &mut EntityParser, input, expected, output| {
            assert_eq!(parser.parse_clang_anonymous_imut(input), Ok(("", expected)));
            assert_eq!(parser.clang_anonymous(expected).to_string(), output);
        };
        let check_anonymous_bijective = |parser: &mut EntityParser, input, expected| {
            check_anonymous(parser, input, expected, input);
        };

        check_anonymous_bijective(&mut parser, "(anonymous)", Default::default());
        check_anonymous(&mut parser, "(unnamed)", Default::default(), "(anonymous)");

        let mut expected = ClangAnonymousEntity {
            identifier: Some(identifier(&mut parser, "class")),
            location: None,
        };
        check_anonymous_bijective(&mut parser, "(anonymous class)", expected);
        expected = ClangAnonymousEntity {
            identifier: Some(identifier(&mut parser, "namespace")),
            location: None,
        };
        check_anonymous(
            &mut parser,
            "(unnamed namespace)",
            expected,
            "(anonymous namespace)",
        );

        let (location_str, location) = test_location(&mut parser);
        let anonymous = format!("(anonymous struct at {location_str})");
        let expected = ClangAnonymousEntity {
            identifier: Some(identifier(&mut parser, "struct")),
            location: Some(location),
        };
        check_anonymous_bijective(&mut parser, &anonymous, expected);
    }

    #[test]
    fn libiberty_unnamed() {
        let parser = EntityParser::new();
        let expected = LibibertyUnnamedType(42);
        assert_eq!(
            parser.parse_libiberty_unnamed_imut("{unnamed type#42}"),
            Ok(("", expected))
        );
        assert_eq!(format!("{expected}"), "{unnamed type#42}");
    }

    #[test]
    fn anonymous() {
        let mut parser = EntityParser::new();
        let (location_str, _location) = test_location(&mut parser);

        {
            let clang_lambda = format!("(lambda at {location_str})");
            let expected = AnonymousEntity::ClangLambda(unwrap_parse(
                parser.parse_clang_lambda_imut(&clang_lambda),
            ));
            assert_eq!(parser.parse_anonymous(&clang_lambda), Ok(("", expected)));
        }

        {
            let libiberty_lambda = "{lambda()#1}";
            let expected = AnonymousEntity::LibibertyLambda(unwrap_parse(
                parser.parse_libiberty_lambda_imut(libiberty_lambda),
            ));
            assert_eq!(parser.parse_anonymous(libiberty_lambda), Ok(("", expected)));
        }

        {
            let clang_anonymous = "(anonymous)";
            let expected = AnonymousEntity::ClangOther(unwrap_parse(
                parser.parse_clang_anonymous_imut(clang_anonymous),
            ));
            assert_eq!(parser.parse_anonymous(clang_anonymous), Ok(("", expected)));
        }

        {
            let libiberty_unnamed = "{unnamed type#666}";
            let expected = AnonymousEntity::LibibertyUnnamed(unwrap_parse(
                parser.parse_libiberty_unnamed_imut(libiberty_unnamed),
            ));
            assert_eq!(
                parser.parse_anonymous(libiberty_unnamed),
                Ok(("", expected))
            );
        }
    }
}
