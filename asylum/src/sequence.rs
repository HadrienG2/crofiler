//! Interning sequences of things

use ahash::RandomState;
use hashbrown::raw::RawTable;
use lasso::Key;
use std::{
    hash::{BuildHasher, Hash, Hasher},
    marker::PhantomData,
    ops::Range,
};

/// Key to retrieve a previously interned sequence
///
/// You can pass this key to InternedSequences::get() to access the sequence.
///
/// You can also use it to compare sequences more efficiently than via direct
/// comparison, as it is guaranteed that two keys are the same if and only if
/// the underlying sequences are the same.
///
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SequenceKey<Inner: Key, const LEN_BITS: u32>(Inner);
//
impl<Inner: Key, const LEN_BITS: u32> SequenceKey<Inner, LEN_BITS> {
    /// Check that the configuration is sensible
    const fn check_configuration() {
        let inner_bits = std::mem::size_of::<usize>() * 8;
        assert!(LEN_BITS < inner_bits as u32);
    }

    /// Returns the `Range<usize>` that represents the current key
    pub fn into_range_usize(self) -> Range<usize> {
        let packed = self.0.into_usize();
        let len = packed & ((1 << LEN_BITS) - 1);
        let offset = packed >> LEN_BITS;
        offset..offset + len
    }

    /// Attempts to create a key from a `Range<usize>`, returning `None` if it fails
    pub fn try_from_range_usize(range: Range<usize>) -> Option<Self> {
        // Type-level sanity checks that should be validated at compile time
        Self::check_configuration();

        // Ensure that the range offset fits in the bit budget
        let offset = range.start;
        if offset.leading_zeros() < LEN_BITS {
            return None;
        }

        // Ensure that the range length fits in the bit budget
        let len = range.count();
        if len > (1 << LEN_BITS) {
            return None;
        }

        // Bit-pack the range offset with the range length
        let packed = (offset << LEN_BITS) | len;
        Inner::try_from_usize(packed).map(Self)
    }
}

/// Interned sequence of things
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InternedSequences<Item, KeyImpl: Key, const LEN_BITS: u32> {
    /// Concatened sequence of all interned sequences
    concatenated: Box<[Item]>,

    /// Added to please rustc
    phantom: PhantomData<*const KeyImpl>,
}
//
impl<Item, KeyImpl: Key, const LEN_BITS: u32> InternedSequences<Item, KeyImpl, LEN_BITS> {
    /// Retrieve a previously interned sequence of things
    pub fn get(&self, key: SequenceKey<KeyImpl, LEN_BITS>) -> &[Item] {
        &self.concatenated[key.into_range_usize()]
    }
}

/// Interner for sequence of things
#[derive(Clone)]
pub struct SequenceInterner<Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> {
    /// Concatenated interned sequences
    concatenated: Vec<Item>,

    /// Hasher factory used by the sequence RawTable
    random_state: RandomState,

    /// Keys to sequences interned so far in `concatenated`
    sequences: RawTable<SequenceKey<KeyImpl, LEN_BITS>>,
}
//
impl<Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32>
    SequenceInterner<Item, KeyImpl, LEN_BITS>
{
    /// Set up a sequence interner
    pub fn new() -> Self {
        Self {
            concatenated: Vec::new(),
            random_state: RandomState::new(),
            sequences: RawTable::new(),
        }
    }

    /// Intern a new sequence
    pub fn intern(&mut self, sequence: &[Item]) -> SequenceKey<KeyImpl, LEN_BITS> {
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
                &self.concatenated[key.into_range_usize()] == sequence
            })
            .cloned()
            .unwrap_or_else(|| {
                // Otherwise intern the sequence
                let start = self.concatenated.len();
                let key = SequenceKey::<KeyImpl, LEN_BITS>::try_from_range_usize(
                    start..start + sequence.len(),
                )
                .unwrap();
                self.concatenated.extend_from_slice(sequence);
                self.sequences.insert(sequence_hash, key.clone(), |key| {
                    hash(&self.concatenated[key.into_range_usize()])
                });
                key
            })
    }

    /// Truth that no sequence has been interned yet
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Query number of interned sequences
    pub fn len(&self) -> usize {
        self.sequences.len()
    }

    /// Query total number of interned items across all interned sequences
    pub fn num_items(&self) -> usize {
        self.concatenated.len()
    }

    /// Query maximal inner sequence length
    pub fn max_sequence_len(&self) -> Option<usize> {
        unsafe {
            self.sequences
                .iter()
                .map(|bucket| {
                    let range = bucket.read();
                    range.into_range_usize().count()
                })
                .max()
        }
    }

    /// Finalize the collection of sequences, keeping all keys valid
    pub fn finalize(self) -> InternedSequences<Item, KeyImpl, LEN_BITS> {
        InternedSequences {
            concatenated: self.concatenated.into_boxed_slice(),
            phantom: PhantomData,
        }
    }
}
//
impl<Item: Copy + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> Default
    for SequenceInterner<Item, KeyImpl, LEN_BITS>
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    type TestedKey = SequenceKey<lasso::Spur, 8>;
    type TestedInterner = SequenceInterner<bool, lasso::Spur, 8>;

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
                sequences.get(
                    TestedKey::try_from_range_usize(seen_so_far..seen_so_far + input.len())
                        .unwrap()
                ),
                *input
            );
            seen_so_far += input.len();
        }
    }

    #[test]
    fn initial() {
        let interner = TestedInterner::new();
        assert_eq!(interner.num_items(), 0);
        assert!(interner.is_empty());
        test_final_state(&[], interner);
    }

    #[test]
    fn intern_one() {
        let test_intern = |input: &[bool]| {
            let mut interner = TestedInterner::new();

            let expected_key = TestedKey::try_from_range_usize(0..input.len()).unwrap();
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

            let key2 = TestedKey::try_from_range_usize(
                interner.num_items()..interner.num_items() + input2.len(),
            )
            .unwrap();
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
