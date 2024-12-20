//! Extension of SequenceInterner which provides an entry() API that can be
//! called recursively using shared references
// FIXME: Move to asylum

use asylum::{
    lasso::Spur,
    sequence::{SequenceInterner, SequenceKey},
    InternerKey,
};
use reffers::ARef;
use std::{cell::RefCell, hash::Hash, ops::Range};

/// SequenceInterner with interior mutability
#[derive(Clone)]
pub struct RecursiveSequenceInterner<
    Item: Clone + Eq + Hash,
    Key: InternerKey<ImplKey = Range<usize>> = SequenceKey<Spur, 8>,
>(RefCell<State<Item, Key>>);
//
#[derive(Clone)]
struct State<
    Item: Clone + Eq + Hash,
    Key: InternerKey<ImplKey = Range<usize>> = SequenceKey<Spur, 8>,
> {
    /// Underlying SequenceInterner
    interner: SequenceInterner<Item, Key>,

    /// Storage for WIP sequences
    storage: Vec<Vec<Item>>,
}
//
impl<Item: Clone + Eq + Hash, Key: InternerKey<ImplKey = Range<usize>>>
    RecursiveSequenceInterner<Item, Key>
{
    // Set up a sequence interner
    pub fn new() -> Self {
        Self(RefCell::new(State {
            interner: SequenceInterner::new(),
            storage: Vec::new(),
        }))
    }

    // Access the inner SequenceInterner
    pub fn borrow(&self) -> ARef<SequenceInterner<Item, Key>> {
        ARef::new(self.0.borrow()).map(|state| &state.interner)
    }

    // Extract the inner SequenceInterner
    #[allow(unused)]
    pub fn into_inner(self) -> SequenceInterner<Item, Key> {
        self.0.into_inner().interner
    }

    // Access an interned sequence
    #[allow(unused)]
    pub fn get(&self, key: Key) -> ARef<[Item]> {
        self.borrow().map(|interner| interner.get(key))
    }

    // Intern a sequence
    #[allow(unused)]
    pub fn intern(&self, sequence: &[Item]) -> Key {
        self.0.borrow_mut().interner.intern(sequence)
    }

    // Prepare to intern a sequence in an iterative fashion, item by item
    #[allow(unknown_lints, clippy::manual_unwrap_or_default)]
    pub fn entry(&self) -> SequenceEntry<Item, Key> {
        let sequence = {
            let mut inner = self.0.borrow_mut();
            if let Some(mut sequence) = inner.storage.pop() {
                sequence.clear();
                sequence
            } else {
                Vec::new()
            }
        };
        SequenceEntry {
            interner: self,
            sequence,
        }
    }
}
//
impl<Item: Clone + Eq + Hash, Key: InternerKey<ImplKey = Range<usize>>> Default
    for RecursiveSequenceInterner<Item, Key>
{
    fn default() -> Self {
        Self::new()
    }
}

/// Mechanism to gradually intern a sequence element by element
pub struct SequenceEntry<
    'interner,
    Item: Clone + Eq + Hash,
    Key: InternerKey<ImplKey = Range<usize>> = SequenceKey<Spur, 8>,
> {
    /// Underlying sequence interner
    interner: &'interner RecursiveSequenceInterner<Item, Key>,

    /// Buffer for a sequence that is in the process of being interned
    sequence: Vec<Item>,
}
//
impl<Item: Clone + Eq + Hash, Key: InternerKey<ImplKey = Range<usize>>>
    SequenceEntry<'_, Item, Key>
{
    /// Add an item to the sequence that is being interned
    pub fn push(&mut self, item: Item) {
        self.sequence.push(item);
    }

    /// Finish the interning transaction
    pub fn intern(self) -> Key {
        self.interner
            .0
            .borrow_mut()
            .interner
            .intern(&self.sequence[..])
    }
}
//
impl<Item: Clone + Eq + Hash, Key: InternerKey<ImplKey = Range<usize>>> Drop
    for SequenceEntry<'_, Item, Key>
{
    fn drop(&mut self) {
        self.interner
            .0
            .borrow_mut()
            .storage
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
            assert_eq!(state.storage.len(), 1);
            assert_eq!(state.storage[0].as_ptr(), storage_ptr);
            assert_eq!(state.storage[0].capacity(), storage_capacity);
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
                assert_eq!(interner.0.borrow().storage.len(), 2);
            }
        }
    }
}
