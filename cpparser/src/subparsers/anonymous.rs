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

    /// Parser for arbitrary lambda representations
    pub fn parse_lambda<'source>(&mut self, s: &'source str) -> IResult<'source, Lambda> {
        self.parse_lambda_imut(s)
    }

    /// Access a previously parsed lambda
    pub fn lambda(&self, l: Lambda) -> LambdaView {
        LambdaView::new(l, self)
    }

    /// Implementation of parse_lambda with internal mutability
    pub(crate) fn parse_lambda_imut<'source>(&self, s: &'source str) -> IResult<'source, Lambda> {
        ((|s| self.parse_clang_lambda_imut(s)).map(Lambda::Clang))
            .or((|s| self.parse_libiberty_lambda_imut(s)).map(Lambda::Libiberty))
            .parse(s)
    }

    /// Parser for clang lambda types `(lambda at <file path>:<line>:<col>)`
    ///
    /// This will fail if the file path contains a ':' sign other than a
    /// Windows-style disk designator at the start, because I have no idea how
    /// to handle this inherent grammar ambiguity better...
    ///
    fn parse_clang_lambda_imut<'source>(&self, s: &'source str) -> IResult<'source, ClangLambda> {
        use nom::{
            bytes::complete::{tag, take_till1},
            character::complete::{anychar, char, u32},
            combinator::{opt, recognize},
            sequence::{delimited, separated_pair},
        };

        let location = separated_pair(u32, char(':'), u32);

        let disk_designator = anychar.and(char(':'));
        let path_str = recognize(opt(disk_designator).and(take_till1(|c| c == ':')));
        let path = path_str.map(|path| self.intern_path_imut(path));

        let file_location = separated_pair(path, char(':'), location);
        let lambda = file_location.map(|(file, location)| ClangLambda { file, location });
        delimited(tag("(lambda at "), lambda, char(')'))(s)
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

    /// Parser for other anonymous clang entities called `(anonymous <stuff>)`
    pub fn parse_anonymous<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, AnonymousEntity> {
        self.parse_anonymous_imut(s)
    }

    /// Implementation of parse_anonymous using internal mutability
    pub(crate) fn parse_anonymous_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, AnonymousEntity> {
        use nom::{
            character::complete::char,
            combinator::opt,
            sequence::{delimited, preceded},
        };
        use nom_supreme::tag::complete::tag;
        delimited(
            tag("(anonymous"),
            opt(preceded(char(' '), |s| self.parse_identifier_imut(s))),
            char(')'),
        )(s)
    }

    /// Access a previously parsed anonymous entity
    pub fn anonymous(&self, a: AnonymousEntity) -> AnonymousEntityView {
        AnonymousEntityView::new(a, self)
    }
}

/// Clang-style lambda featuring source location description
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ClangLambda {
    /// Source file in which the lambda is declared
    file: PathKey,

    /// Declaration location within the file
    location: (Line, Column),
}
//
/// Line number within a file
pub type Line = u32;
//
/// Column number within a file
pub type Column = u32;

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
impl<'entities> PartialEq for ClangLambdaView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const _ == other.entities as *const _) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for ClangLambdaView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        let (line, column) = self.location();
        write!(f, "(lambda at {}:{line}:{column})", self.file())
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
impl<'entities> PartialEq for LibibertyLambdaView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const _ == other.entities as *const _) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for LibibertyLambdaView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for LibibertyLambdaView<'entities> {
    fn recursion_depth(&self) -> usize {
        self.signature().recursion_depth()
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        write!(f, "{{lambda")?;
        self.signature().display_impl(f, state)?;
        write!(f, "#{}}}", self.id())
    }
}

/// Lambda function
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Lambda {
    /// Clang-style lambda with source location description
    Clang(ClangLambda),

    /// Libiberty-style lambda with parameter types and numeric ID
    Libiberty(LibibertyLambda),
}

/// View of a lambda function
#[derive(PartialEq)]
pub enum LambdaView<'entities> {
    /// Clang-style lambda with source location description
    Clang(ClangLambdaView<'entities>),

    /// Libiberty-style lambda with parameter types and numeric ID
    Libiberty(LibibertyLambdaView<'entities>),
}
//
impl<'entities> LambdaView<'entities> {
    /// Set up a lambda view
    pub(crate) fn new(inner: Lambda, entities: &'entities EntityParser) -> Self {
        match inner {
            Lambda::Clang(c) => Self::Clang(entities.clang_lambda(c)),
            Lambda::Libiberty(l) => Self::Libiberty(entities.libiberty_lambda(l)),
        }
    }
}
//
impl<'entities> Display for LambdaView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for LambdaView<'entities> {
    fn recursion_depth(&self) -> usize {
        match self {
            // FIXME: Bring in simplified path display from crofiler
            Self::Clang(_) => 0,
            Self::Libiberty(l) => l.recursion_depth(),
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            Self::Clang(c) => write!(f, "{c}"),
            Self::Libiberty(l) => l.display_impl(f, state),
        }
    }
}

/// Anonymous clang entity (known as `(anonymous)` or `(anonymous <something>)`)
pub type AnonymousEntity = Option<IdentifierKey>;

/// View of an anonymous clang entity (known as `(anonymous)` or `(anonymous <something>)`)
#[derive(PartialEq)]
pub struct AnonymousEntityView<'entities>(pub Option<IdentifierView<'entities>>);
//
impl<'entities> AnonymousEntityView<'entities> {
    /// Build a new-expression view
    pub fn new(inner: AnonymousEntity, entities: &'entities EntityParser) -> Self {
        Self(inner.map(|id| entities.identifier(id)))
    }
}
//
impl<'entities> Display for AnonymousEntityView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "(anonymous")?;
        if let Some(id) = &self.0 {
            write!(f, " {id}")?
        }
        write!(f, ")")
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

    #[test]
    fn clang_lambda() {
        let mut parser = EntityParser::new();
        if cfg!(target_os = "windows") {
            let expected = ClangLambda {
                file: parser.intern_path("c:/source.cpp"),
                location: (123, 45),
            };
            assert_eq!(
                parser.parse_clang_lambda_imut("(lambda at c:/source.cpp:123:45)"),
                Ok(("", expected))
            );
            assert_eq!(
                parser.clang_lambda(expected).to_string(),
                "(lambda at c:/source.cpp:123:45)",
            )
        } else {
            let expected = ClangLambda {
                file: parser.intern_path("/path/to/source.cpp"),
                location: (123, 45),
            };
            assert_eq!(
                parser.parse_clang_lambda_imut("(lambda at /path/to/source.cpp:123:45)"),
                Ok(("", expected))
            );
            assert_eq!(
                parser.clang_lambda(expected).to_string(),
                "(lambda at /path/to/source.cpp:123:45)",
            )
        }
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
            &["{lambda(???)#1}", "{lambda(auto:1)#1}"],
        );
    }

    #[test]
    fn anonymous() {
        let mut parser = EntityParser::new();
        let identifier = |parser: &mut EntityParser, s| unwrap_parse(parser.parse_identifier(s));
        let check_anonymous = |parser: &mut EntityParser, input, expected| {
            assert_eq!(parser.parse_anonymous(input), Ok(("", expected)));
            assert_eq!(format!("{}", parser.anonymous(expected)), input);
        };

        check_anonymous(&mut parser, "(anonymous)", None);

        let mut expected = Some(identifier(&mut parser, "class"));
        check_anonymous(&mut parser, "(anonymous class)", expected);

        expected = Some(identifier(&mut parser, "namespace"));
        check_anonymous(&mut parser, "(anonymous namespace)", expected);
    }
}
