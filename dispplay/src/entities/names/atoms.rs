//! Atoms from the C++ entity grammar

use crate::{Context, Tree, TreeDisplay};
use cpparser::subparsers::names::atoms::IdentifierView;
use std::fmt::Write;

impl TreeDisplay for IdentifierView<'_> {
    fn raw_tree(&self, context: &mut Context) -> Tree {
        let mut s = context.new_leaf();
        write!(s, "{self}").expect("Actually infaillible");
        Tree::Leaf(s)
    }

    fn tag() -> &'static str {
        "Identifier"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cpparser::EntityParser;

    #[test]
    fn identifier() {
        let mut parser = EntityParser::new();
        const IDENTIFIER: &str = "idiotifier";
        let (_rem, key) = parser.parse_identifier(IDENTIFIER).unwrap();
        let view = parser.identifier(key);

        let mut context = Context::new();
        assert_eq!(
            view.tree(&mut context),
            (context.id("Identifier"), Tree::Leaf(IDENTIFIER.to_owned()))
        );

        // TODO: Add display test and factor out test code commonalities
    }
}
