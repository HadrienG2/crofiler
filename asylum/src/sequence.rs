//! Interning sequences of things

use ahash::RandomState;
use hashbrown::raw::RawTable;
use std::{
    hash::{BuildHasher, Hash, Hasher},
    ops::Range,
};

/// Interned sequence of things
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InternedSequences<Item> {
    /// Concatened sequence of all interned sequences
    concatenated: Box<[Item]>,
}
//
impl<Item> InternedSequences<Item> {
    /// Retrieve a previously interned sequence of things
    pub fn get(&self, key: SequenceKey) -> &[Item] {
        &self.concatenated[key]
    }
}

/// Key to retrieve a previously interned sequence
///
/// You can pass this key to InternedSequences::get() to access the sequence.
///
/// You can also use it to compare sequences more efficiently than via direct
/// comparison, as it is guaranteed that two keys are the same if and only if
/// the underlying sequences are the same.
///
/// Depending on your concrete use case, you may be able to get away with a
/// smaller sequence key type. For example, if you're expecting no more than
/// 65536 interned sequences, each no more than 255 elements long, you can
/// convert this into a (u24, u8) pair representing (base, length) for storage,
/// packed into a single u32, then convert that back to Range<usize> at access
/// time, for 75% space savings.
///
pub type SequenceKey = Range<usize>;

/// Interner for sequence of things
#[derive(Clone)]
pub struct SequenceInterner<Item: Clone + Eq + Hash> {
    /// Concatenated interned sequences
    concatenated: Vec<Item>,

    /// Hasher factory used by the sequence RawTable
    random_state: RandomState,

    /// Keys to sequences interned so far in `concatenated`
    sequences: RawTable<SequenceKey>,
}
//
impl<Item: Clone + Eq + Hash> SequenceInterner<Item> {
    /// Set up a sequence interner
    pub fn new() -> Self {
        Self {
            concatenated: Vec::new(),
            random_state: RandomState::new(),
            sequences: RawTable::new(),
        }
    }

    /// Intern a new sequence
    pub fn intern(&mut self, sequence: &[Item]) -> SequenceKey {
        // Hash the input sequence
        let hash = |seq: &[Item]| {
            let mut hasher = self.random_state.build_hasher();
            seq.hash(&mut hasher);
            hasher.finish()
        };
        let sequence_hash = hash(sequence);

        // If this sequence was interned before, return the same key
        self.sequences
            .get(sequence_hash, |key| {
                &self.concatenated[key.clone()] == sequence
            })
            .cloned()
            .unwrap_or_else(|| {
                // Otherwise intern the sequence
                let start = self.concatenated.len();
                let key = start..start + sequence.len();
                self.concatenated.extend_from_slice(sequence);
                self.sequences.insert(sequence_hash, key.clone(), |key| {
                    hash(&self.concatenated[key.clone()])
                });
                key
            })
    }

    /// Truth that no sequence has been interned yet
    pub fn is_empty(&self) -> bool {
        self.sequences.is_empty()
    }

    /// Query number of interned sequences
    pub fn len(&self) -> usize {
        self.sequences.len()
    }

    /// Query total number of interned items across all interned sequences
    pub fn num_items(&self) -> usize {
        self.concatenated.len()
    }

    /// Finalize the collection of sequences, keeping all keys valid
    pub fn finalize(self) -> InternedSequences<Item> {
        InternedSequences {
            concatenated: self.concatenated.into_boxed_slice(),
        }
    }
}
//
impl<Item: Copy + Eq + Hash> Default for SequenceInterner<Item> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    type TestedInterner = SequenceInterner<bool>;

    fn test_final_state(inputs: &[&[bool]], interner: TestedInterner) {
        let sequences = interner.finalize();
        assert_eq!(
            sequences.concatenated,
            inputs
                .iter()
                .flat_map(|input| input.iter())
                .copied()
                .collect()
        );
        let mut seen_so_far = 0;
        for input in inputs {
            assert_eq!(
                sequences.get(seen_so_far..seen_so_far + input.len()),
                *input
            );
            seen_so_far += input.len();
        }
    }

    #[test]
    fn initial() {
        let interner = TestedInterner::new();
        assert_eq!(interner.num_items(), 0);
        assert!(interner.sequences.is_empty());
        test_final_state(&[], interner);
    }

    #[test]
    fn intern_one() {
        let test_intern = |input: &[bool]| {
            let mut interner = TestedInterner::new();

            let expected_key = 0..input.len();
            assert_eq!(interner.intern(input), expected_key);
            assert_eq!(interner.concatenated, input);
            assert_eq!(interner.len(), 1);
            assert_eq!(interner.num_items(), input.len());
            test_final_state(&[input], interner.clone());

            assert_eq!(interner.intern(input), expected_key);
            assert_eq!(interner.concatenated, input);
            assert_eq!(interner.len(), 1);
            assert_eq!(interner.num_items(), input.len());
            test_final_state(&[input], interner);
        };

        for input in [[].as_ref(), [false].as_ref(), [true, false].as_ref()] {
            test_intern(input);
        }
    }

    #[test]
    fn intern_two() {
        let test_intern = |input1: &[bool], input2: &[bool]| {
            let mut interner = TestedInterner::new();
            let key1 = interner.intern(input1);
            let interner1 = interner.clone();

            let key2 = interner.num_items()..interner.num_items() + input2.len();
            assert_eq!(interner.intern(input2), key2);
            assert_eq!(interner.len(), 2);
            assert_eq!(interner.num_items(), interner1.num_items() + input2.len());
            test_final_state(&[input1, input2], interner.clone());

            assert_eq!(interner.intern(input1), key1);
            assert_eq!(interner.len(), 2);
            assert_eq!(interner.num_items(), interner1.num_items() + input2.len());
            test_final_state(&[input1, input2], interner.clone());

            assert_eq!(interner.intern(input2), key2);
            assert_eq!(interner.len(), 2);
            assert_eq!(interner.num_items(), interner1.num_items() + input2.len());
            test_final_state(&[input1, input2], interner);
        };

        let test_dataset = [[].as_ref(), [false].as_ref(), [true, false].as_ref()];
        for input1 in test_dataset {
            for input2 in test_dataset {
                if input2 == input1 {
                    continue;
                } else {
                    test_intern(input1, input2);
                }
            }
        }
    }
}
