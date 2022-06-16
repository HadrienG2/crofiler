//! Clang-provided names to C++ entities that don't have a language-defined name
//! including lambdas, anonymous classes, anonymous namespaces...

use crate::{
    names::atoms::{IdentifierKey, IdentifierView},
    Entities, EntityParser, IResult, InternedPath, PathKey,
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

    /// Parser for clang lambda types `(lambda at <file path>:<line>:<col>)`
    ///
    /// This will fail if the file path contains a ':' sign other than a
    /// Windows-style disk designator at the start, because I have no idea how
    /// to handle this inherent grammar ambiguity better...
    ///
    pub fn parse_lambda<'source>(&self, s: &'source str) -> IResult<'source, Lambda> {
        use nom::{
            bytes::complete::{tag, take_till1},
            character::complete::{anychar, char, u32},
            combinator::{opt, recognize},
            sequence::{delimited, separated_pair},
        };

        let location = separated_pair(u32, char(':'), u32);

        let disk_designator = anychar.and(char(':'));
        let path_str = recognize(opt(disk_designator).and(take_till1(|c| c == ':')));
        let path = path_str.map(|path| self.path_to_key(path));

        let file_location = separated_pair(path, char(':'), location);
        let lambda = file_location.map(|(file, location)| Lambda { file, location });
        delimited(tag("(lambda at "), lambda, char(')'))(s)
    }

    /// Parser for other anonymous clang entities called `(anonymous <stuff>)`
    pub fn parse_anonymous<'source>(&self, s: &'source str) -> IResult<'source, AnonymousEntity> {
        use nom::{
            character::complete::char,
            combinator::opt,
            sequence::{delimited, preceded},
        };
        use nom_supreme::tag::complete::tag;
        delimited(
            tag("(anonymous"),
            opt(preceded(char(' '), |s| self.parse_identifier(s))),
            char(')'),
        )(s)
    }
}
//
impl Entities {
    /// Access a previously parsed lambda
    pub fn lambda(&self, l: Lambda) -> LambdaView {
        LambdaView::new(l, self)
    }

    /// Access a previously parsed anonymous entity
    pub fn anonymous(&self, a: AnonymousEntity) -> AnonymousEntityView {
        AnonymousEntityView::new(a, self)
    }
}

/// Lambda location description
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Lambda {
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

/// View of a lambda location description
pub struct LambdaView<'entities> {
    /// Wrapped Lambda
    inner: Lambda,

    /// Underlying interned entity storage
    entities: &'entities Entities,
}
//
impl<'entities> LambdaView<'entities> {
    /// Build a new-expression view
    pub fn new(inner: Lambda, entities: &'entities Entities) -> Self {
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
impl<'entities> PartialEq for LambdaView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const Entities == other.entities as *const Entities)
            && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for LambdaView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        let (line, column) = self.location();
        write!(f, "(lambda at {}:{line}:{column})", self.file())
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
    pub fn new(inner: AnonymousEntity, entities: &'entities Entities) -> Self {
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
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn unknown_entity() {
        assert_eq!(
            EntityParser::parse_unknown_entity("<unknown>"),
            Ok(("", ()))
        );
    }

    #[test]
    fn lambda() {
        let parser = EntityParser::new();
        if cfg!(target_os = "windows") {
            assert_eq!(
                parser.parse_lambda("(lambda at c:/source.cpp:123:45)"),
                Ok((
                    "",
                    Lambda {
                        file: parser.path_to_key("c:/source.cpp"),
                        location: (123, 45)
                    }
                ))
            );
        } else {
            assert_eq!(
                parser.parse_lambda("(lambda at /path/to/source.cpp:123:45)"),
                Ok((
                    "",
                    Lambda {
                        file: parser.path_to_key("/path/to/source.cpp"),
                        location: (123, 45)
                    }
                ))
            );
        }
    }

    #[test]
    fn anonymous() {
        let parser = EntityParser::new();
        let identifier = |s| unwrap_parse(parser.parse_identifier(s));
        assert_eq!(parser.parse_anonymous("(anonymous)"), Ok(("", None)));
        assert_eq!(
            parser.parse_anonymous("(anonymous class)"),
            Ok(("", Some(identifier("class"))))
        );
        assert_eq!(
            parser.parse_anonymous("(anonymous namespace)"),
            Ok(("", Some(identifier("namespace"))))
        );
    }
}
