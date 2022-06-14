//! Miscellaneous tooling used as part of cpparser's implementation

use asylum::{
    lasso::{Key, Spur},
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
pub struct RecursiveSequenceInterner<
    Item: Clone + Eq + Hash,
    KeyImpl: Key = Spur,
    const LEN_BITS: u32 = 8,
>(RefCell<(SequenceInterner<Item, KeyImpl, LEN_BITS>, Vec<Vec<Item>>)>);
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
    KeyImpl: Key = Spur,
    const LEN_BITS: u32 = 8,
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
pub struct SequenceEntry<
    'interner,
    Item: Clone + Eq + Hash,
    KeyImpl: Key = Spur,
    const LEN_BITS: u32 = 8,
> {
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

#[cfg(test)]
mod tests {
    use super::*;

    type TestedInterner = RecursiveSequenceInterner<bool>;

    #[test]
    fn basic() {
        for input in [[].as_slice(), [false].as_slice(), [true, false].as_slice()] {
            // Check that the interner works
            let interner = TestedInterner::new();
            let (storage_ptr, storage_capacity) = {
                // Set up an entry
                let mut entry = interner.entry();
                for &item in input {
                    entry.push(item);
                }

                // Memorize storage properties
                let storage_ptr = entry.sequence.as_ptr();
                let storage_capacity = entry.sequence.capacity();

                // Intern the entry and check it can be retrieved
                let key = entry.intern();
                assert_eq!(interner.borrow().get(key), input);
                (storage_ptr, storage_capacity)
            };

            // Check that storage is reused
            let state = interner.0.borrow();
            let storage = &state.1;
            assert_eq!(storage.len(), 1);
            assert_eq!(storage[0].as_ptr(), storage_ptr);
            assert_eq!(storage[0].capacity(), storage_capacity);
        }
    }

    #[test]
    fn recursive() {
        let inputs = [[].as_slice(), [false].as_slice(), [true, false].as_slice()];
        for input1 in inputs {
            for input2 in inputs {
                let interner = TestedInterner::new();

                // Set up an entry for input1
                let mut entry1 = interner.entry();
                for &item in input1 {
                    entry1.push(item);
                }

                // Set up an entry for input2
                let mut entry2 = interner.entry();
                for &item in input2 {
                    entry2.push(item);
                }

                // Intern entry2 and check it can be retrieved
                let key2 = entry2.intern();
                assert_eq!(interner.borrow().get(key2), input2);

                // Intern entry1 and check that both entries can be retrieved
                let key1 = entry1.intern();
                assert_eq!(interner.borrow().get(key1), input1);
                assert_eq!(interner.borrow().get(key2), input2);

                // Check that both vectors seem to be reused
                assert_eq!(interner.0.borrow().1.len(), 2);
            }
        }
    }
}
