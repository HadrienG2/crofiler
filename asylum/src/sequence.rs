//! Interning sequences of things

use std::{collections::HashMap, hash::Hash, ops::Range};

/// Interned sequence of things
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, Default, PartialEq)]
pub struct SequenceInterner<Item: Clone + Eq + Hash> {
    /// Number of items that was interned so far
    num_items: usize,

    /// Unique sequences received so far, along with associated position in the
    /// overall sequence of all interned sequences.
    sequences: HashMap<Box<[Item]>, usize>,
}
//
impl<Item: Clone + Eq + Hash> SequenceInterner<Item> {
    /// Set up a sequence interner
    pub fn new() -> Self {
        Self {
            num_items: 0,
            sequences: HashMap::new(),
        }
    }

    /// Intern a new sequence
    pub fn intern(&mut self, sequence: &[Item]) -> SequenceKey {
        // If the sequence was interned before, return the associated key
        if let Some(&key) = self.sequences.get(sequence) {
            key..key + sequence.len()
        } else {
            // Otherwise, record the new sequence
            self.sequences
                .insert(sequence.to_vec().into_boxed_slice(), self.num_items);
            let key = self.num_items..self.num_items + sequence.len();
            self.num_items += sequence.len();
            key
        }
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
        self.num_items
    }

    /// Finalize the collection of sequences, keeping all keys valid
    pub fn finalize(self) -> InternedSequences<Item> {
        // Retrieve interned sequences, put them in interning order
        let mut sequences_and_starts = self.sequences.into_iter().collect::<Vec<_>>();
        sequences_and_starts.sort_unstable_by_key(|(_seq, start)| *start);

        // Concatenate them
        let mut concatenated = Vec::with_capacity(self.num_items);
        for (seq, _start) in sequences_and_starts {
            concatenated.extend(Vec::from(seq));
        }

        // ...and we're done
        InternedSequences {
            concatenated: concatenated.into_boxed_slice(),
        }
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
        assert_eq!(interner.num_items, 0);
        assert!(interner.sequences.is_empty());
        test_final_state(&[], interner);
    }

    #[test]
    fn intern_one() {
        let test_intern = |input: &[bool]| {
            let mut interner = TestedInterner::new();

            let expected_key = 0..input.len();
            assert_eq!(interner.intern(input), expected_key);
            assert_eq!(interner.num_items, input.len());
            assert_eq!(
                interner.sequences,
                maplit::hashmap! {
                    input.into() => 0,
                }
            );

            let old_interner = interner.clone();
            assert_eq!(interner.intern(input), expected_key);
            assert_eq!(interner, old_interner);

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

            let key2 = interner.num_items..interner.num_items + input2.len();
            assert_eq!(interner.intern(input2), key2);
            assert_eq!(interner.num_items, interner1.num_items + input2.len());
            let mut expected_sequences = interner1.sequences.clone();
            expected_sequences.insert(input2.into(), interner1.num_items);
            assert_eq!(interner.sequences, expected_sequences);

            let interner2 = interner.clone();
            assert_eq!(interner.intern(input1), key1);
            assert_eq!(interner, interner2);
            assert_eq!(interner.intern(input2), key2);
            assert_eq!(interner, interner2);

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
