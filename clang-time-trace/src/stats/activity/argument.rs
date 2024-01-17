//! Facilities for handling clang activity arguments

use crate::{ClangTrace, InternedPath, PathError, PathKey};
use cpp_demangle::{DemangleOptions, ParseOptions, Symbol as MangledSymbol};
use cpparser::{nom, EntityKey, EntityParser, EntityView};
use log::{info, warn};
use std::{cell::RefCell, path::Path, rc::Rc};
use thiserror::Error;

/// Tuned 10x above maximum observed requirement
const CPP_DEMANGLE_RECURSION_LIMIT: u32 = 1024;

/// Activity argument that underwent basic validation, but not yet full parsing
//
// Due to how the Rust borrow checker works, one needs to clone this data in
// order to parse it, so it has been made cheap to clone.
//
// At the JSON data level, this type represents the contents of the "detail"
// field, along with instructions on how it should be parsed. Prior validation
// has already assessed that if a detail field should be present, it is present,
// so the Option can be unwrapped with confidence.
//
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RawActivityArgument {
    /// How this activity argument should be parsed
    arg_type: ActivityArgumentType,

    /// What the "detail" string of the JSON source contained
    detail: Option<Rc<str>>,
}
//
impl RawActivityArgument {
    /// Record an activity argument, knowing its intended parsing logic and the
    /// "detail" string payload that was provided in the JSON file.
    pub(crate) fn new(arg_type: ActivityArgumentType, detail: Option<Rc<str>>) -> Self {
        Self { arg_type, detail }
    }

    /// Check how this activity argument is supposed to be parsed
    pub fn arg_type(&self) -> ActivityArgumentType {
        self.arg_type
    }

    /// Get a copy of the raw "detail" string from JSON
    pub fn detail(&self) -> Option<Rc<str>> {
        self.detail.clone()
    }

    /// Parse the activity argument
    pub fn parse(
        self,
        trace: &mut ClangTrace,
    ) -> Result<ParsedActivityArgument, ActivityArgumentError> {
        let (parser, demangling_buf) = trace.parser_and_demangling_buf();
        self.parse_impl(parser, demangling_buf)
    }

    /// Lower-level version of parse() used in unit tests
    fn parse_impl(
        self,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<ParsedActivityArgument, ActivityArgumentError> {
        let detail = RefCell::new(self.detail);
        let has_detail = || detail.borrow().is_some();
        let detail_opt = || detail.borrow_mut().take();
        let detail = || detail_opt().expect("Presence should be checked upstream");
        //
        let mut symbol_arg =
            |parser: &mut EntityParser| -> Result<ParsedSymbol, ActivityArgumentError> {
                Self::parse_symbol(detail(), parser, demangling_buf)
            };
        //
        let parse_unnamed_loop_arg =
            || -> Result<(), ActivityArgumentError> { Self::parse_unnamed_loop(detail()) };
        //
        match self.arg_type {
            ActivityArgumentType::Nothing => {
                debug_assert_eq!(detail_opt(), None);
                Ok(ParsedActivityArgument::Nothing)
            }

            ActivityArgumentType::String => Ok(ParsedActivityArgument::String(detail())),

            ActivityArgumentType::FilePathOrModule => {
                let detail = detail();
                if &*detail == "[module]" {
                    Ok(ParsedActivityArgument::Module)
                } else {
                    Ok(ParsedActivityArgument::FilePath(
                        parser.intern_path(&detail),
                    ))
                }
            }

            ActivityArgumentType::CppEntity => {
                Self::parse_entity(&detail(), parser).map(ParsedActivityArgument::CppEntity)
            }

            ActivityArgumentType::Symbol => symbol_arg(parser).map(ParsedActivityArgument::Symbol),

            ActivityArgumentType::SymbolOpt => {
                if has_detail() {
                    symbol_arg(parser).map(ParsedActivityArgument::Symbol)
                } else {
                    Ok(ParsedActivityArgument::Nothing)
                }
            }

            ActivityArgumentType::UnnamedLoop => {
                parse_unnamed_loop_arg().map(|()| ParsedActivityArgument::UnnamedLoop)
            }

            ActivityArgumentType::UnnamedLoopOpt => {
                if has_detail() {
                    parse_unnamed_loop_arg().map(|()| ParsedActivityArgument::UnnamedLoop)
                } else {
                    Ok(ParsedActivityArgument::Nothing)
                }
            }
        }
    }

    /// Parse a "detail" argument payload that contains a C++ entity name
    fn parse_entity(
        s: &str,
        parser: &mut EntityParser,
    ) -> Result<EntityKey, ActivityArgumentError> {
        parser.parse_entity(s).map_err(|e| {
            ActivityArgumentError::from(nom::error::Error::new(Box::<str>::from(e.input), e.code))
        })
    }

    /// Parse a "detail" argument payload that contains a C++ symbol
    fn parse_symbol(
        mut symbol: Rc<str>,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<ParsedSymbol, ActivityArgumentError> {
        // Clang recently got this great idea of surrounding symbol names with
        // parentheses, which we must undo if needed
        if symbol.starts_with('(') && symbol.ends_with(')') {
            symbol = Rc::from(&symbol[1..symbol.len() - 1]);
        }

        let demangling_result = MangledSymbol::new_with_options(
            &*symbol,
            &ParseOptions::default().recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
        )
        .map(|s| {
            demangling_buf.clear();
            s.structured_demangle(
                &mut *demangling_buf,
                &DemangleOptions::default()
                    .hide_expression_literal_types()
                    .no_return_type()
                    .recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
            )
        });

        let mut parse_demangled = |entity: Rc<str>| -> ParsedSymbol {
            if let Ok(parsed) = Self::parse_entity(&entity, parser) {
                ParsedSymbol::Parsed(parsed)
            } else {
                ParsedSymbol::Demangled(entity)
            }
        };
        let parsed = match demangling_result {
            // Mangled symbol was successfully demangled, intern it along with the rest
            Ok(Ok(())) => parse_demangled(demangling_buf.clone().into()),

            // Symbol failed to demangle, try some patterns that cpp_demangle
            // should not reject but actually does reject before giving up
            Ok(Err(_)) | Err(_) => match &*symbol {
                "main" | "__clang_call_terminate" => parse_demangled(symbol),
                _ => ParsedSymbol::MaybeMangled(symbol),
            },
        };
        match &parsed {
            ParsedSymbol::Parsed(_) => {}

            // Clang unfortunately occasionally emits mangled symbols that are
            // ill-formed (e.g. function parameters without types). cpp_demangle
            // will usually survive these, but produce output that we cannot
            // parse. Since this is a "normal" situation, an error is excessive,
            // but a warning seems warranted.
            ParsedSymbol::Demangled(d) => warn!("Failed to parse demangled symbol {d:?}"),

            // Some of the symbols emitted by clang are not mangled
            // (e.g. __cxx_global_var_init.1), and this is fine and expected, so
            // we don't want to flag these as warnings. But clang may also emit
            // ill-formed mangled symbols, so this event is still worth logging.
            ParsedSymbol::MaybeMangled(m) => info!("Failed to demangle symbol {m:?}"),
        }
        Ok(parsed)
    }

    /// Handling of the "<unnamed loop>" constant argument
    fn parse_unnamed_loop(loop_name: Rc<str>) -> Result<(), ActivityArgumentError> {
        if &*loop_name == "<unnamed loop>" {
            Ok(())
        } else {
            let loop_name: &str = &loop_name;
            Err(ActivityArgumentError::UnexpectedLoopName(loop_name.into()))
        }
    }
}

/// Empirically observed activity argument parsing logics for time-trace entries
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ActivityArgumentType {
    /// No argument
    Nothing,

    /// An arbitrary string
    String,

    /// An file path or the "[module]" constant string
    FilePathOrModule,

    /// A C++ entity (class, function, ...)
    CppEntity,

    /// A binary file symbol
    Symbol,

    /// Either a binary file symbol or nothing
    SymbolOpt,

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,

    /// Either the "<unnamed loop>" constant string or nothing
    UnnamedLoopOpt,
}
//
impl ActivityArgumentType {
    /// Try to infer the activity argument type for an activity with a detail argument
    pub(crate) fn infer_from_detail(detail: &str) -> Self {
        // Clang recently introduced parenthesized symbol names. To handle them
        // without duplicating all symbol name detection below, we need a
        // normalization pass that removes the parentheses
        let mut normalized_symbol_name = detail;
        if detail.starts_with('(') && detail.ends_with(')') {
            normalized_symbol_name = &normalized_symbol_name[1..normalized_symbol_name.len() - 1];
        }

        // Let's get rid of the easy grammars first
        if normalized_symbol_name.starts_with("_Z")
            || normalized_symbol_name == "main"
            || normalized_symbol_name == "__clang_call_terminate"
            || normalized_symbol_name.starts_with("__cxx_global_var_init")
            || normalized_symbol_name.starts_with("_GLOBAL__sub_I_")
        {
            // clang uses the Itanium ABI for C++ name mangling, so mangled
            // symbols will start with "_Z". We also treat the few non-mangled
            // symbols emitted by the compiler as mangled for simplicity
            return ActivityArgumentType::Symbol;
        } else if detail == "<unnamed loop>" {
            // Always true by definition
            return ActivityArgumentType::UnnamedLoop;
        } else if Path::new(detail).is_absolute() || detail == "[module]" {
            // Where older clangs only emitted absolute file paths, recent
            // clangs also started emitting the unhelpful "[module]" string.
            return ActivityArgumentType::FilePathOrModule;
        }

        // Alas, now we must discriminate between two very
        // flexible/complicated grammars:
        //
        // 1. Simple information text, e.g. "X86 DAG->DAG Instruction Selection"
        // 3. C++ entity name
        //
        // It is basically impossible to tell whether we're dealing with a C++
        // entity without invoking a C++ parser, which is expensive, so that
        // will be our default case when things does not look like info text.
        //
        if !detail
            .chars()
            .next()
            .expect("detail is not empty if detail_arg() returned Ok")
            .is_uppercase()
        {
            return ActivityArgumentType::CppEntity;
        }
        let mut can_be_string = true;
        'words: for word in detail.split(' ') {
            if word == "&" || word == "memcmp()" {
                continue;
            }
            let mut chars = word.chars();
            let first = chars.next();
            let last = chars.next_back();
            for side in first.into_iter().chain(last) {
                if !side.is_alphanumeric() {
                    can_be_string = false;
                    break 'words;
                }
            }
            let mut dash_before = false;
            let mut caps_before = false;
            for middle in chars {
                let dash_before = std::mem::replace(&mut dash_before, middle == '-');
                let caps_before = std::mem::replace(&mut caps_before, middle.is_uppercase());
                if !(middle.is_alphanumeric()
                    || middle == '-'
                    || (middle == '>' && dash_before)
                    || middle == '/'
                    || middle == '_' && caps_before)
                {
                    can_be_string = false;
                    break 'words;
                }
            }
        }
        if can_be_string {
            ActivityArgumentType::String
        } else {
            ActivityArgumentType::CppEntity
        }
    }
}

/// Stored data about an activity's argument
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParsedActivityArgument {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(Rc<str>),

    /// An interned file path
    FilePath(PathKey),

    /// A C++ entity (class, function, ...)
    CppEntity(EntityKey),

    /// A C++ symbol, likely mangled
    Symbol(ParsedSymbol),

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,

    /// The "[module]" constant string
    Module,
}
//
impl ParsedActivityArgument {
    /// Resolve associated ActivityArgumentView
    pub fn resolve<'a>(&'a self, trace: &'a ClangTrace) -> ActivityArgument<'a> {
        match self {
            ParsedActivityArgument::Nothing => ActivityArgument::Nothing,
            ParsedActivityArgument::String(s) => ActivityArgument::String(s),
            ParsedActivityArgument::FilePath(p) => ActivityArgument::FilePath(trace.file_path(*p)),
            ParsedActivityArgument::CppEntity(e) => ActivityArgument::CppEntity(trace.entity(*e)),
            ParsedActivityArgument::Symbol(m) => ActivityArgument::Symbol(m.resolve(trace)),
            ParsedActivityArgument::UnnamedLoop => ActivityArgument::UnnamedLoop,
            ParsedActivityArgument::Module => ActivityArgument::Module,
        }
    }
}
//
/// Concrete things that an activity can take as an argument
#[derive(PartialEq)]
pub enum ActivityArgument<'trace> {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(&'trace str),

    /// An interned file path
    FilePath(InternedPath<'trace>),

    /// A C++ entity (class, function, ...)
    CppEntity(EntityView<'trace>),

    /// A C++ symbol, likely mangled
    Symbol(Symbol<'trace>),

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,

    /// The "[module]" constant string
    Module,
}

/// Stored data about a C++ symbol that we tried to demangle and parse
///
/// See Symbol for more documentation
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParsedSymbol {
    /// Symbol was successfully demangled and interned
    Parsed(EntityKey),

    /// The symbol was demangled, but could not be parsed into an AST
    Demangled(Rc<str>),

    /// Demangling failed and the symbol was kept in its original form.
    MaybeMangled(Rc<str>),
}
//
impl ParsedSymbol {
    /// Resolve the full mangled symbol data
    pub fn resolve<'a>(&'a self, trace: &'a ClangTrace) -> Symbol<'a> {
        match self {
            ParsedSymbol::Parsed(key) => Symbol::Parsed(trace.entity(*key)),
            ParsedSymbol::Demangled(s) => Symbol::Demangled(s),
            ParsedSymbol::MaybeMangled(s) => Symbol::MaybeMangled(s),
        }
    }
}
//
/// A mangled C++ symbol that we tried to demangle and parse
#[derive(PartialEq)]
pub enum Symbol<'trace> {
    /// Symbol was successfully demangled and interned
    Parsed(EntityView<'trace>),

    /// The symbol was demangled, but could not be parsed into an AST
    ///
    /// This normally happens when the demangler emits ill-formed output such
    /// as `SomeTemplate<int, && >` or `()...`. If you find reasonable output
    /// which we do not parse, please submit it as a bug.
    ///
    Demangled(&'trace str),

    /// Demangling failed and the symbol was kept in its original form.
    ///
    /// Typical patterns that fail to demangle include
    /// - __cxx_global_var_init(.<number>)?
    /// - _GLOBAL__sub_I_<source file>
    ///
    MaybeMangled(&'trace str),
}

/// What can go wrong while parsing an Activity's argument
#[derive(Error, Debug, PartialEq)]
pub enum ActivityArgumentError {
    /// Encountered an unexpected activity file path
    #[error("failed to parse activity file path ({0})")]
    BadFilePath(#[from] PathError),

    /// Failed to parse a C++ entity name
    #[error("failed to parse C++ entity ({0})")]
    BadCppEntity(#[from] nom::error::Error<Box<str>>),

    /// Unexpected loop name
    #[error("unexpected loop name ({0})")]
    UnexpectedLoopName(Box<str>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nothing() {
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::Nothing,
                detail: None
            }
            .parse_impl(&mut EntityParser::new(), &mut String::new()),
            Ok(ParsedActivityArgument::Nothing)
        );
    }

    #[test]
    fn parse_string() {
        const MOCK_STR: &str = "X86 DAG->DAG Instruction Selection";
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::String,
                detail: Some(MOCK_STR.into())
            }
            .parse_impl(&mut EntityParser::new(), &mut String::new()),
            Ok(ParsedActivityArgument::String(MOCK_STR.into()))
        );
    }

    #[test]
    fn parse_path() {
        const MOCK_PATH: &str =
            "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
        let mut parser = EntityParser::new();
        let path_key = parser.intern_path(MOCK_PATH);
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::FilePathOrModule,
                detail: Some(MOCK_PATH.into())
            }
            .parse_impl(&mut parser, &mut String::new()),
            Ok(ParsedActivityArgument::FilePath(path_key))
        );
    }

    #[test]
    fn parse_entity() {
        const MOCK_ENTITY: &str = "Acts::Test::MeasurementCreator";
        let mut parser = EntityParser::new();
        let entity_key = parser
            .parse_entity(MOCK_ENTITY)
            .expect("Known-good parse, shouldn't fail");
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::CppEntity,
                detail: Some(MOCK_ENTITY.into())
            }
            .parse_impl(&mut parser, &mut String::new()),
            Ok(ParsedActivityArgument::CppEntity(entity_key))
        );
    }

    #[test]
    fn parse_symbol() {
        // Commonalities betwwen symbol tests
        let test_symbol =
            |entity_parser: &mut EntityParser, symbol: &str, expected_parse: ParsedSymbol| {
                for arg_type in [
                    ActivityArgumentType::Symbol,
                    ActivityArgumentType::SymbolOpt,
                ] {
                    assert_eq!(
                        RawActivityArgument {
                            arg_type,
                            detail: Some(symbol.into())
                        }
                        .parse_impl(&mut *entity_parser, &mut String::new()),
                        Ok(ParsedActivityArgument::Symbol(expected_parse.clone()))
                    );
                }
            };
        let mut parser = EntityParser::new();

        // Symbol that demangles
        const VALID: &str = "_ZN4Acts4Test29comb_kalman_filter_zero_field11test_methodEv";
        let key = parser
            .parse_entity("Acts::Test::comb_kalman_filter_zero_field::test_method()")
            .expect("Known-good parse, shouldn't fail");
        test_symbol(&mut parser, VALID, ParsedSymbol::Parsed(key));

        // Symbol that doesn't demangle
        const INVALID: &str = "__cxx_global_var_init.1";
        test_symbol(
            &mut parser,
            INVALID,
            ParsedSymbol::MaybeMangled(INVALID.into()),
        );

        // Optional symbol that isn't present
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::SymbolOpt,
                detail: None
            }
            .parse_impl(&mut parser, &mut String::new()),
            Ok(ParsedActivityArgument::Nothing)
        );
    }

    #[test]
    fn parse_unnamed_loop() {
        // Unnamed loop argument
        for arg_type in [
            ActivityArgumentType::UnnamedLoop,
            ActivityArgumentType::UnnamedLoopOpt,
        ] {
            assert_eq!(
                RawActivityArgument {
                    arg_type,
                    detail: Some("<unnamed loop>".into())
                }
                .parse_impl(&mut EntityParser::new(), &mut String::new()),
                Ok(ParsedActivityArgument::UnnamedLoop)
            );
        }

        // Optional unnamed loop argument that isn't present
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::UnnamedLoopOpt,
                detail: None
            }
            .parse_impl(&mut EntityParser::new(), &mut String::new()),
            Ok(ParsedActivityArgument::Nothing)
        );
    }

    #[test]
    fn infer_type_from_detail() {
        let inferred_type = ActivityArgumentType::infer_from_detail;
        use ActivityArgumentType::*;

        assert_eq!(
            inferred_type(
                "_ZNSt6vectorIfSaIfEE6insertEN9__gnu_cxx17__normal_iteratorIPKfS1_EERS4_"
            ),
            Symbol
        );
        assert_eq!(inferred_type("__cxx_global_var_init"), Symbol);
        assert_eq!(inferred_type("__cxx_global_var_init.8"), Symbol);
        assert_eq!(inferred_type("main"), Symbol);
        assert_eq!(
            inferred_type("_GLOBAL__sub_I_AdaptiveMultiVertexFinderTests.cpp"),
            Symbol
        );

        assert_eq!(inferred_type("<unnamed loop>"), UnnamedLoop);

        assert_eq!(inferred_type("/test.cpp"), FilePathOrModule);

        assert_eq!(
            inferred_type("Prologue/Epilogue Insertion & Frame Finalization"),
            String
        );
        assert_eq!(inferred_type("Expand memcmp() to load/stores"), String);
        assert_eq!(inferred_type("Post-Dominator Tree Construction"), String);
        assert_eq!(inferred_type("X86 DAG->DAG Instruction Selection"), String);
        assert_eq!(inferred_type("Lower AMX type for load/store"), String);
        assert_eq!(inferred_type("Live DEBUG_VALUE analysis"), String);

        assert_eq!(inferred_type("double"), CppEntity);
        assert_eq!(inferred_type("ZeroFieldKalmanAlignment_invoker"), CppEntity);
    }
}
