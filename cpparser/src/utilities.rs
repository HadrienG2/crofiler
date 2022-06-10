//! Miscellaneous tooling used as part of cpparser's implementation

use asylum::{
    lasso::Key,
    sequence::{SequenceInterner, SequenceKey},
};
use std::{
    cell::{Ref, RefCell},
    hash::Hash,
    ops::Deref,
};

/// Extension of SequenceInterner which provides an entry() API that can be
/// called recursively using shared references
#[derive(Clone)]
pub struct RecursiveSequenceInterner<Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32>(
    RefCell<(SequenceInterner<Item, KeyImpl, LEN_BITS>, Vec<Vec<Item>>)>,
);
//
impl<Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32>
    RecursiveSequenceInterner<Item, KeyImpl, LEN_BITS>
{
    // Set up a sequence interner
    pub fn new() -> Self {
        Self(RefCell::new((SequenceInterner::new(), Vec::new())))
    }

    // Access the inner SequenceInterner
    pub fn borrow(&self) -> SequenceInternerRef<Item, KeyImpl, LEN_BITS> {
        SequenceInternerRef(self.0.borrow())
    }

    // Extract the inner SequenceInterner
    pub fn into_inner(self) -> SequenceInterner<Item, KeyImpl, LEN_BITS> {
        self.0.into_inner().0
    }

    // Prepare to intern a sequence in an iterative fashion, item by item
    pub fn entry(&self) -> SequenceEntry<Item, KeyImpl, LEN_BITS> {
        let sequence = {
            let mut inner = self.0.borrow_mut();
            if let Some(mut sequence) = inner.1.pop() {
                sequence.clear();
                sequence
            } else {
                Vec::new()
            }
        };
        SequenceEntry {
            interner: &self.0,
            sequence,
        }
    }
}
//
impl<Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> Default
    for RecursiveSequenceInterner<Item, KeyImpl, LEN_BITS>
{
    fn default() -> Self {
        Self::new()
    }
}

/// Shared reference to a sequence interner
pub struct SequenceInternerRef<
    'interner,
    Item: Clone + Eq + Hash,
    KeyImpl: Key,
    const LEN_BITS: u32,
>(Ref<'interner, (SequenceInterner<Item, KeyImpl, LEN_BITS>, Vec<Vec<Item>>)>);
//
impl<'interner, Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> Deref
    for SequenceInternerRef<'interner, Item, KeyImpl, LEN_BITS>
{
    type Target = SequenceInterner<Item, KeyImpl, LEN_BITS>;
    fn deref(&self) -> &Self::Target {
        &(*self.0).0
    }
}

/// Mechanism to gradually intern a sequence element by element
pub struct SequenceEntry<'interner, Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> {
    /// Underlying sequence interner
    interner: &'interner RefCell<(SequenceInterner<Item, KeyImpl, LEN_BITS>, Vec<Vec<Item>>)>,

    /// Buffer for a sequence that is in the process of being interned
    sequence: Vec<Item>,
}
//
impl<'interner, Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32>
    SequenceEntry<'interner, Item, KeyImpl, LEN_BITS>
{
    /// Add an item to the sequence that is being interned
    pub fn push(&mut self, item: Item) {
        self.sequence.push(item);
    }

    /// Finish the interning transaction
    pub fn intern(self) -> SequenceKey<KeyImpl, LEN_BITS> {
        self.interner.borrow_mut().0.intern(&self.sequence[..])
    }
}
//
impl<'interner, Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> Drop
    for SequenceEntry<'interner, Item, KeyImpl, LEN_BITS>
{
    fn drop(&mut self) {
        self.interner
            .borrow_mut()
            .1
            .push(std::mem::take(&mut self.sequence))
    }
}
