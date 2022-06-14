//! Tools for interning more than just strings

#![deny(missing_docs)]

use hashbrown::HashMap;
use lasso::{Key, Spur};
use std::{hash::Hash, marker::PhantomData};

pub mod path;
pub mod sequence;

/// Re-export used crates to avoid duplicate dependencies
pub use lasso;

/// Interned things
#[derive(Clone, Debug, PartialEq)]
pub struct Interned<Item, K: Key = Spur>(Box<[Item]>, PhantomData<*const K>);
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Interner<Item: Clone + Eq + Hash, K: Key = Spur>(
    HashMap<Item, usize>,
    PhantomData<*const K>,
);
//
impl<Item: Clone + Eq + Hash, K: Key> Interner<Item, K> {
    /// Set up an interner
    pub fn new() -> Self {
        Self(HashMap::new(), PhantomData)
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
        let new_key = self.0.len();
        K::try_from_usize(*self.0.entry(item).or_insert(new_key))
            .expect("Went above key capacity, please use a bigger key")
    }

    /// Retrieve a previously interned thing
    ///
    /// May not be optimal, meant for validation use only
    ///
    pub fn get(&self, key: K) -> &Item {
        self.0
            .iter()
            .find(|(_item, idx)| **idx == key.into_usize())
            .map(|(item, _idx)| item)
            .expect("Key not found")
    }

    /// Truth that no sequence has been interned yet
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Query number of interned items
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Finalize the collection of sequences, keeping all keys valid
    pub fn finalize(self) -> Interned<Item, K> {
        // Retrieve interned items, put them in interning order
        let mut items_and_keys = self.0.into_iter().collect::<Vec<_>>();
        items_and_keys.sort_unstable_by_key(|(_item, key)| *key);

        // Drop keys
        let items = items_and_keys
            .into_iter()
            .map(|(item, _key)| item)
            .collect();

        // ...and we're done
        Interned(items, PhantomData)
    }
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
            assert_eq!(sequences.get(Spur::try_from_usize(idx).unwrap()), item);
        }
    }

    #[test]
    fn initial() {
        let interner = TestedInterner::new();
        assert!(interner.0.is_empty());
        assert_eq!(interner.len(), 0);
        test_final_state(&[], interner);
    }

    #[test]
    fn intern_one() {
        let test_intern = |input: bool| {
            let mut interner = TestedInterner::new();

            let key1 = interner.intern(input);
            assert_eq!(key1.into_usize(), 0);
            let mut expected_data = HashMap::new();
            expected_data.insert(input, 0);
            assert_eq!(interner.0, expected_data);
            assert!(!interner.is_empty());
            assert_eq!(interner.len(), 1);
            assert_eq!(interner.get(key1), &input);

            let old_interner = interner.clone();
            assert_eq!(interner.intern(input).into_usize(), 0);
            assert_eq!(interner, old_interner);

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
            interner.intern(input1);
            let interner1 = interner.clone();

            let key2 = interner.intern(input2);
            assert_eq!(key2.into_usize(), 1);
            let mut expected_items = interner1.0.clone();
            expected_items.insert(input2, 1);
            assert_eq!(interner.0, expected_items);
            assert!(!interner.is_empty());
            assert_eq!(interner.len(), 2);
            assert_eq!(interner.get(key2), &input2);

            let interner2 = interner.clone();
            assert_eq!(interner.intern(input1).into_usize(), 0);
            assert_eq!(interner, interner2);
            assert_eq!(interner.intern(input2).into_usize(), 1);
            assert_eq!(interner, interner2);

            test_final_state(&[input1, input2], interner);
        };

        let test_dataset = [[false, true], [true, false]];
        for [input1, input2] in test_dataset {
            test_intern(input1, input2);
        }
    }
}
