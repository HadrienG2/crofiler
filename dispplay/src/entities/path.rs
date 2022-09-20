//! File path handling

use crate::{Context, Separator, Tree, TreeDisplay};
use asylum::{
    lasso::{Key, Spur},
    path::{InternedComponent, InternedPath, PathResolver},
};
use std::fmt::Write;

impl<'parent, ComponentKey: Key> TreeDisplay for InternedComponent<'parent, ComponentKey> {
    fn raw_tree(&self, context: &mut Context) -> Tree {
        let mut buf = context.new_leaf();
        let s: &str = self.as_ref();
        write!(buf, "{s}").expect("Actually infaillible");
        Tree::Leaf(buf)
    }

    fn tag() -> &'static str {
        "Path.Component"
    }
}

impl<'parent, Parent: PathResolver + ?Sized> TreeDisplay for InternedPath<'parent, Parent> {
    fn raw_tree(&self, context: &mut Context) -> Tree {
        let mut items = context.new_list();
        for component in self.components() {
            items.push(component.raw_tree(context));
        }
        Tree::List {
            header_trailer: None,
            item_id: InternedComponent::<Spur>::id(context),
            items,
            separator: Separator::Path,
        }
    }

    fn tag() -> &'static str {
        "Path"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cpparser::EntityParser;

    // FIXME: Add tests, factor out wrt identifier
}
