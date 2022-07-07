//! Tools for interning more than just strings

#![deny(missing_docs)]

use ahash::RandomState;
use hashbrown::raw::RawTable;
use lasso::{Key, Spur};
use std::{
    hash::{BuildHasher, Hash, Hasher},
    marker::PhantomData,
};

pub mod path;
pub mod sequence;

/// Re-export used crates to avoid duplicate dependencies
pub use lasso;

/// Interned things
#[derive(Clone, Debug, PartialEq)]
pub struct Interned<Item, K: Key = Spur>(Box<[Item]>, PhantomData<K>);
//
impl<Item, K: Key> Interned<Item, K> {
    /// Retrieve a previously interned thing
    pub fn get(&self, key: K) -> &Item {
        &self.0[key.into_usize()]
    }
}

/// Interner for arbitrary things
//
// Holds unique items received so far, along with interning order
//
#[derive(Clone)]
pub struct Interner<Item: Clone + Eq + Hash, K: Key = Spur> {
    /// Sequence of interned items
    items: Vec<Item>,

    /// Hasher factory used by the sequence RawTable
    random_state: RandomState,

    /// Keys to items interned so far in `concatenated`
    keys: RawTable<K>,
}
//
impl<Item: Clone + Eq + Hash, K: Key> Interner<Item, K> {
    /// Set up an interner
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            random_state: RandomState::new(),
            keys: RawTable::new(),
        }
    }

    /// Intern a new item
    ///
    /// You can pass the resulting key to Interned::get() to access the item.
    ///
    /// You can also use it to compare items more efficiently than via direct
    /// comparison, as it is guaranteed that two keys are the same if and only
    /// if the underlying items are the same.
    ///
    pub fn intern(&mut self, item: Item) -> K {
        // Hash the item
        let item_hash = hash(&self.random_state, &item);

        // If this item was interned before, return the same key
        self.keys
            .get(item_hash, |key| self.items[key.into_usize()] == item)
            .cloned()
            .unwrap_or_else(|| {
                // Push new item back, use its position as a key
                let key = K::try_from_usize(self.items.len())
                    .expect("Key space exhausted, must use a larger K");
                self.items.push(item);

                // Take note that this item was interned
                self.keys.insert(item_hash, key, |key| {
                    hash(&self.random_state, &self.items[key.into_usize()])
                });
                key
            })
    }

    /// Retrieve a previously interned item
    pub fn get(&self, key: K) -> &Item {
        &self.items[key.into_usize()]
    }

    /// Truth that no sequence has been interned yet
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Query number of interned items
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Finalize the collection of sequences, keeping all keys valid
    pub fn finalize(self) -> Interned<Item, K> {
        Interned(self.items.into(), PhantomData)
    }
}
//
/// Hash an item
fn hash<Item: Clone + Eq + Hash>(random_state: &RandomState, item: &Item) -> u64 {
    let mut hasher = random_state.build_hasher();
    item.hash(&mut hasher);
    hasher.finish()
}
//
impl<Item: Clone + Eq + Hash> Default for Interner<Item> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    type TestedInterner = Interner<bool>;

    fn test_final_state(inputs: &[bool], interner: TestedInterner) {
        let sequences = interner.finalize();
        assert_eq!(sequences.0, inputs.into());
        for (idx, item) in inputs.iter().enumerate() {
            assert_eq!(
                sequences.get(
                    Spur::try_from_usize(idx)
                        .expect("If interning succeeded, item index should fit in a Spur")
                ),
                item
            );
        }
    }

    #[test]
    fn initial() {
        let interner = TestedInterner::new();
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
        test_final_state(&[], interner);
    }

    #[test]
    fn intern_one() {
        let test_intern = |input: bool| {
            let mut interner = TestedInterner::new();

            let key1 = interner.intern(input);
            assert_eq!(key1.into_usize(), 0);
            assert_eq!(interner.items, &[input]);
            assert_eq!(interner.len(), 1);
            assert!(!interner.is_empty());
            assert_eq!(interner.get(key1), &input);
            test_final_state(&[input], interner.clone());

            assert_eq!(interner.intern(input), key1);
            assert_eq!(interner.items, &[input]);
            assert_eq!(interner.len(), 1);
            assert!(!interner.is_empty());
            assert_eq!(interner.get(key1), &input);
            test_final_state(&[input], interner);
        };

        for input in [false, true] {
            test_intern(input);
        }
    }

    #[test]
    fn intern_two() {
        let test_intern = |input1: bool, input2: bool| {
            let mut interner = TestedInterner::new();
            let key1 = interner.intern(input1);

            let key2 = interner.intern(input2);
            assert_eq!(key2.into_usize(), 1);
            assert_eq!(interner.items, &[input1, input2]);
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
            assert_eq!(interner.get(key1), &input1);
            assert_eq!(interner.get(key2), &input2);
            test_final_state(&[input1, input2], interner.clone());

            assert_eq!(interner.intern(input1), key1);
            assert_eq!(interner.items, &[input1, input2]);
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
            assert_eq!(interner.get(key1), &input1);
            assert_eq!(interner.get(key2), &input2);
            test_final_state(&[input1, input2], interner.clone());

            assert_eq!(interner.intern(input2), key2);
            assert_eq!(interner.items, &[input1, input2]);
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
            assert_eq!(interner.get(key1), &input1);
            assert_eq!(interner.get(key2), &input2);
            test_final_state(&[input1, input2], interner.clone());
        };

        let test_dataset = [[false, true], [true, false]];
        for [input1, input2] in test_dataset {
            test_intern(input1, input2);
        }
    }
}
