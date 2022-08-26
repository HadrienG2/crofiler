//! Atoms from the C++ entity grammar

use crate::{EntityParser, Error, IResult};
use asylum::lasso::MiniSpur;
use nom::{error::ErrorKind, Parser};
use nom_supreme::ParserExt;
use reffers::ARef;
use std::{
    fmt::{self, Display, Formatter},
    ops::Deref,
};

/// Interned C++ identifier key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// identifiers as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve an identifier by passing this key to the
/// identifier() method of EntityParser.
///
pub type IdentifierKey = MiniSpur;
//
impl EntityParser {
    /// Generate a parser recognizing a certain C++ keyword
    pub fn keyword_parser(word: &'static str) -> impl Fn(&str) -> IResult<()> {
        use nom_supreme::tag::complete::tag;
        move |s| tag(word).terminated(end_of_identifier).value(()).parse(s)
    }

    /// Generate a parser recognizing a set of C++ keywords, associating each of
    /// them with a corresponding output value.
    ///
    /// keyword_to_output can either be a direct keyword-to-output mapping of the
    /// [(keyword, output); LEN] form, or a list of keywords in [keyword; LEN] form.
    /// In the latter case, all keywords will be associated with the () output. This
    /// makes sense when we don't actually care about which keyword in a set is
    /// present, as with the "typename" and "class" keywords in C++ entity names.
    ///
    /// For optimal performance, keywords should be sorted in order of decreasing
    /// occurence frequency (most common keywords go first).
    ///
    pub fn keywords_parser<Output: Clone, const LEN: usize>(
        keyword_to_output: impl KeywordMappings<Output, LEN>,
    ) -> impl Fn(&str) -> IResult<Output> {
        // Compute keyword-to-output mappings and set up keyword parsers
        assert_ne!(LEN, 0, "Must provide at least one keyword to match");
        let parsers_to_output = keyword_to_output
            .mappings()
            .map(|(key, value)| (Self::keyword_parser(key), value));

        // Return the parser
        move |s| {
            let mut last_error = None;
            for (parser, output) in parsers_to_output.iter() {
                match parser(s) {
                    Ok((remainder, ())) => return Ok((remainder, output.clone())),
                    Err(error) => last_error = Some(error),
                }
            }
            Err(last_error.expect("By above assert_ne check, this option will be filled"))
        }
    }

    /// Parser recognizing any valid C++ identifier
    pub fn parse_identifier<'input>(
        &mut self,
        input: &'input str,
    ) -> IResult<'input, IdentifierKey> {
        self.parse_identifier_imut(input)
    }

    /// Implementation of parse_identifier with internal mutability
    pub(crate) fn parse_identifier_imut<'input>(
        &self,
        input: &'input str,
    ) -> IResult<'input, IdentifierKey> {
        let (rest, id) = identifier(input)?;
        let id_key = self.identifiers.borrow_mut().get_or_intern(id);
        Ok((rest, id_key))
    }

    /// Access a previously parsed identifier
    pub fn identifier(&self, key: IdentifierKey) -> IdentifierView {
        IdentifierView::new(key, self)
    }

    /// Retrieve an identifier previously parsed by parse_identifier
    pub(crate) fn raw_identifier(&self, key: IdentifierKey) -> ARef<str> {
        ARef::new(self.identifiers.borrow()).map(|identifiers| identifiers.resolve(&key))
    }

    /// Tell how many unique identifiers have been parsed so far
    pub fn num_identifiers(&self) -> usize {
        self.identifiers.borrow().len()
    }
}

/// Trait that maps parsed keywords to parser output
pub trait KeywordMappings<Output, const LEN: usize> {
    /// Emit the mapping
    fn mappings(self) -> [(&'static str, Output); LEN];
}
//
impl<Output, const LEN: usize> KeywordMappings<Output, LEN> for [(&'static str, Output); LEN] {
    fn mappings(self) -> Self {
        self
    }
}
//
impl<const LEN: usize> KeywordMappings<(), LEN> for [&'static str; LEN] {
    fn mappings(self) -> [(&'static str, ()); LEN] {
        self.map(|key| (key, ()))
    }
}

/// Parser recognizing any valid C++ identifier
fn identifier(s: &str) -> IResult<&str> {
    #[cfg(feature = "unicode_xid")]
    {
        use nom::{bytes::complete::take_while, character::complete::satisfy};
        (satisfy(is_id_start).and(take_while(is_id_continue)))
            .recognize()
            .parse(s)
    }
    #[cfg(not(feature = "unicode_xid"))]
    {
        debug_assert!(s.is_ascii());
        let bytes = s.as_bytes().split_first();
        match bytes {
            Some((first, others)) if is_id_start(*first) => {
                let first_nonid = others.iter().position(|&b| !is_id_continue(b));
                let (id, rest) = if let Some(first_nonid) = first_nonid {
                    s.split_at(first_nonid + 1)
                } else {
                    (s, "")
                };
                Ok((rest, id))
            }
            _ => Err(nom::Err::Error(Error::new(s, ErrorKind::Satisfy))),
        }
    }
}

/// Parser recognizing the end of an identifier, without consuming it
#[inline(always)]
fn end_of_identifier(s: &str) -> IResult<()> {
    #[cfg(feature = "unicode_xid")]
    {
        use nom::{
            character::complete::satisfy,
            combinator::{eof, not, peek},
        };
        peek(not(satisfy(is_id_continue)).or(eof.value(()))).parse(s)
    }
    #[cfg(not(feature = "unicode-xid"))]
    {
        debug_assert!(s.is_ascii());
        match s.as_bytes().first().copied() {
            Some(b) if is_id_continue(b) => Err(nom::Err::Error(Error::new(s, ErrorKind::Satisfy))),
            _ => Ok((s, ())),
        }
    }
}

/// Truth that a character can be used as the start of a C++ identifier
// This version supports Unicode identifiers
#[cfg(feature = "unicode-xid")]
#[inline(always)]
fn is_id_start(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_start(c) || c == '_'
}
//
/// Truth that a character can be used as the start of a C++ identifier
// This version assumes ASCII input
#[cfg(not(feature = "unicode-xid"))]
#[inline(always)]
fn is_id_start(b: u8) -> bool {
    debug_assert!(b.is_ascii());
    b.is_ascii_alphabetic() || (b == b'_') || (b == b'$')
}

/// Truth that a character can be used in the middle of a C++ identifier
// This version supports Unicode identifiers
#[cfg(feature = "unicode-xid")]
#[inline(always)]
fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}
//
/// Truth that a character can be used in the middle of a C++ identifier
// This version assumes ASCII input
#[cfg(not(feature = "unicode-xid"))]
#[inline(always)]
fn is_id_continue(b: u8) -> bool {
    debug_assert!(b.is_ascii());
    is_id_start(b) || b.is_ascii_digit()
}

/// A view of a C++ identifier
pub struct IdentifierView<'entities> {
    /// Key used to retrieve the identifier
    key: IdentifierKey,

    /// Wrapped identifier
    inner: ARef<'entities, str>,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> IdentifierView<'entities> {
    /// Set up a C++ identifier view
    pub fn new(key: IdentifierKey, entities: &'entities EntityParser) -> Self {
        Self {
            key,
            inner: entities.raw_identifier(key),
            entities,
        }
    }
}
//
impl<'entities> AsRef<str> for IdentifierView<'entities> {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}
//
impl<'entities> Deref for IdentifierView<'entities> {
    type Target = str;
    fn deref(&self) -> &str {
        &self.inner
    }
}
//
impl<'entities> PartialEq for IdentifierView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.key == other.key)
    }
}
//
impl<'entities> Display for IdentifierView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.inner.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn end_of_identifier() {
        assert_eq!(super::end_of_identifier(""), Ok(("", ())));
        assert_eq!(super::end_of_identifier("+"), Ok(("+", ())));
        assert!(super::end_of_identifier("x").is_err());
    }

    #[test]
    fn keyword() {
        assert_eq!(EntityParser::keyword_parser("abc")("abc"), Ok(("", ())));
        assert!(EntityParser::keyword_parser("abc")("x").is_err());
        assert!(EntityParser::keyword_parser("abc")("abcd").is_err());
    }

    #[test]
    fn keywords() {
        let keywords = EntityParser::keywords_parser([("five", 5), ("three", 3), ("four", 4)]);
        assert_eq!(keywords("three"), Ok(("", 3)));
        assert_eq!(keywords("four "), Ok((" ", 4)));
        assert!(keywords("fourth").is_err());
        assert_eq!(keywords("five"), Ok(("", 5)));
        assert!(keywords("one").is_err());
    }

    #[test]
    fn identifier() {
        let mut entities = EntityParser::new();
        const ID: &str = "_abczd_123904";
        let (rest, key) = entities
            .parse_identifier(ID)
            .expect("We know this is a valid identifier");
        assert_eq!(rest, "");
        assert_eq!(entities.num_identifiers(), 1);
        assert_eq!(&*entities.raw_identifier(key), ID);
        assert_eq!(entities.identifier(key).as_ref(), ID);

        let mut id_str = ID.to_string();
        id_str.push('*');
        assert_eq!(entities.parse_identifier(&id_str), Ok(("*", key)));
        assert_eq!(entities.num_identifiers(), 1);
        assert_eq!(&*entities.raw_identifier(key), ID);
    }
}
