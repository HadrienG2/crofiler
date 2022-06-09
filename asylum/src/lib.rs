//! Tools for interning more than just strings

#![deny(missing_docs)]

use std::{collections::HashMap, hash::Hash};

pub mod path;
pub mod sequence;

/// Re-export used crates to avoid duplicate dependencies
pub use lasso;

/// Interned things
#[derive(Clone, Debug, PartialEq)]
pub struct Interned<Item>(Box<[Item]>);
//
impl<Item> Interned<Item> {
    /// Retrieve a previously interned thing
    pub fn get(&self, key: Key) -> &Item {
        &self.0[key]
    }
}

/// Key to retrieve a previously interned item
///
/// You can pass this key to Interned::get() to access the item.
///
/// You can also use it to compare items more efficiently than via direct
/// comparison, as it is guaranteed that two keys are the same if and only if
/// the underlying items are the same.
///
/// Depending on your concrete use case, you may be able to get away with a
/// smaller item key type. For example, if you're expecting no more than
/// 65536 interned items, you can convert this into a u16, then convert that
/// back to usize at access time, for 75% space savings.
///
pub type Key = usize;

/// Interner for arbitrary things
//
// Holds unique items received so far, along with interning order
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Interner<Item: Clone + Eq + Hash>(HashMap<Item, usize>);
//
impl<Item: Clone + Eq + Hash> Interner<Item> {
    /// Set up an interner
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Intern a new item
    pub fn intern(&mut self, item: Item) -> Key {
        // If the item was interned before, return the associated key
        if let Some(&key) = self.0.get(&item) {
            key
        } else {
            // Otherwise, record the new item
            let key = self.0.len();
            self.0.insert(item, key);
            key
        }
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
    pub fn finalize(self) -> Interned<Item> {
        // Retrieve interned items, put them in interning order
        let mut items_and_keys = self.0.into_iter().collect::<Vec<_>>();
        items_and_keys.sort_unstable_by_key(|(_item, key)| *key);

        // Drop keys
        let items = items_and_keys
            .into_iter()
            .map(|(item, _key)| item)
            .collect();

        // ...and we're done
        Interned(items)
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
            assert_eq!(sequences.get(idx), item);
        }
    }

    #[test]
    fn initial() {
        let interner = TestedInterner::new();
        assert!(interner.0.is_empty());
        test_final_state(&[], interner);
    }

    #[test]
    fn intern_one() {
        let test_intern = |input: bool| {
            let mut interner = TestedInterner::new();

            assert_eq!(interner.intern(input), 0);
            assert_eq!(
                interner.0,
                maplit::hashmap! {
                    input => 0,
                }
            );

            let old_interner = interner.clone();
            assert_eq!(interner.intern(input), 0);
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

            assert_eq!(interner.intern(input2), 1);
            let mut expected_items = interner1.0.clone();
            expected_items.insert(input2, 1);
            assert_eq!(interner.0, expected_items);

            let interner2 = interner.clone();
            assert_eq!(interner.intern(input1), 0);
            assert_eq!(interner, interner2);
            assert_eq!(interner.intern(input2), 1);
            assert_eq!(interner, interner2);

            test_final_state(&[input1, input2], interner);
        };

        let test_dataset = [[false, true], [true, false]];
        for [input1, input2] in test_dataset {
            test_intern(input1, input2);
        }
    }
}
