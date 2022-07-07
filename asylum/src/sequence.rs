//! Interning sequences of things

use ahash::RandomState;
use hashbrown::raw::RawTable;
use lasso::{Key, Spur};
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
/// Internally, it is implemented as an extended version of lasso's Key that
/// reserves some key bits for inline sequence length information.
///
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SequenceKey<Inner: Key = Spur, const LEN_BITS: u32 = 8>(Inner);
//
impl<Inner: Key, const LEN_BITS: u32> SequenceKey<Inner, LEN_BITS> {
    /// Check that the configuration is sensible
    const fn check_configuration() {
        let usize_bits = std::mem::size_of::<usize>() * 8;
        assert!(LEN_BITS < usize_bits as u32);
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
        if len >= (1 << LEN_BITS) {
            return None;
        }

        // Bit-pack the range offset with the range length
        let packed = (offset << LEN_BITS) | len;
        Inner::try_from_usize(packed).map(Self)
    }
}

/// Interned sequence of things
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InternedSequences<Item, KeyImpl: Key = Spur, const LEN_BITS: u32 = 8> {
    /// Concatened sequence of all interned sequences
    concatenated: Box<[Item]>,

    /// Added to please rustc
    phantom: PhantomData<KeyImpl>,
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
pub struct SequenceInterner<Item: Clone + Eq + Hash, KeyImpl: Key = Spur, const LEN_BITS: u32 = 8> {
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

    /// Retrieve a previously interned input sequence's key, if it exists
    fn get_sequence_key(
        &self,
        sequence_hash: u64,
        sequence: &[Item],
    ) -> Option<SequenceKey<KeyImpl, LEN_BITS>> {
        self.sequences
            .get(sequence_hash, |key| {
                &self.concatenated[key.into_range_usize()] == sequence
            })
            .cloned()
    }

    /// Record a previously keyed/stored sequence into the sequence interner
    ///
    /// This assumes that the sequence has not been interned before, and that
    /// self.concatenated has been set up as the key suggests.
    ///
    fn insert_sequence_key(
        &mut self,
        sequence_hash: u64,
        sequence_key: SequenceKey<KeyImpl, LEN_BITS>,
    ) {
        self.sequences.insert(sequence_hash, sequence_key, |key| {
            hash(
                &self.random_state,
                &self.concatenated[key.into_range_usize()],
            )
        });
    }

    /// Prepare to intern a sequence in an iterative fashion, item by item
    pub fn entry(&mut self) -> SequenceEntry<Item, KeyImpl, LEN_BITS> {
        let initial_concatenated_len = self.concatenated.len();
        SequenceEntry {
            interner: self,
            initial_concatenated_len,
        }
    }

    /// Intern a new sequence
    pub fn intern(&mut self, sequence: &[Item]) -> SequenceKey<KeyImpl, LEN_BITS> {
        // Hash the input sequence
        let hash = hash(&self.random_state, sequence);

        // If this sequence was interned before, return the same key
        self.get_sequence_key(hash, sequence).unwrap_or_else(|| {
            // Otherwise intern the sequence
            let start = self.concatenated.len();
            let key = SequenceKey::<KeyImpl, LEN_BITS>::try_from_range_usize(
                start..start + sequence.len(),
            )
            .expect("Key space exhausted, must use a larger KeyImpl or more LEN_BITS");
            self.concatenated.extend_from_slice(sequence);
            self.insert_sequence_key(hash, key);
            key
        })
    }

    /// Retrieve a previously interned sequence
    pub fn get(&self, key: SequenceKey<KeyImpl, LEN_BITS>) -> &[Item] {
        &self.concatenated[key.into_range_usize()]
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
impl<Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> Default
    for SequenceInterner<Item, KeyImpl, LEN_BITS>
{
    fn default() -> Self {
        Self::new()
    }
}
//
/// Hash an input sequence
fn hash<Item: Clone + Eq + Hash>(random_state: &RandomState, sequence: &[Item]) -> u64 {
    let mut hasher = random_state.build_hasher();
    sequence.hash(&mut hasher);
    hasher.finish()
}

/// Mechanism to gradually intern a sequence element by element
pub struct SequenceEntry<
    'interner,
    Item: Clone + Eq + Hash,
    KeyImpl: Key = Spur,
    const LEN_BITS: u32 = 8,
> {
    /// Underlying sequence interner
    interner: &'interner mut SequenceInterner<Item, KeyImpl, LEN_BITS>,

    /// Initial length of interner.concatenated
    initial_concatenated_len: usize,
}
//
impl<'interner, Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32>
    SequenceEntry<'interner, Item, KeyImpl, LEN_BITS>
{
    /// Add an item to the sequence that is being interned
    pub fn push(&mut self, item: Item) {
        self.interner.concatenated.push(item);
    }

    /// Remove the last item from the sequence
    pub fn pop(&mut self) -> Option<Item> {
        if self.interner.concatenated.len() > self.initial_concatenated_len {
            self.interner.concatenated.pop()
        } else {
            None
        }
    }

    /// Number of items that were added to the sequence
    pub fn len(&self) -> usize {
        self.interner.concatenated.len() - self.initial_concatenated_len
    }

    /// Finish the interning transaction
    pub fn intern(self) -> SequenceKey<KeyImpl, LEN_BITS> {
        // Hash the input sequence and figure what its interning key would be
        let sequence = &self.interner.concatenated[self.initial_concatenated_len..];
        let hash = hash(&self.interner.random_state, sequence);
        let start = self.initial_concatenated_len;
        let new_key =
            SequenceKey::<KeyImpl, LEN_BITS>::try_from_range_usize(start..start + sequence.len())
                .expect("Went above sequence key capacity, please use a bigger sequence key");

        // If this sequence was interned before, return the same key
        if let Some(old_key) = self.interner.get_sequence_key(hash, sequence) {
            old_key
        } else {
            // Otherwise, intern the sequence, and inhibit drop so that the
            // items that we just interned are not discarded
            self.interner.insert_sequence_key(hash, new_key);
            std::mem::forget(self);
            new_key
        }
    }
}
//
impl<'interner, Item: Clone + Eq + Hash, KeyImpl: Key, const LEN_BITS: u32> Drop
    for SequenceEntry<'interner, Item, KeyImpl, LEN_BITS>
{
    fn drop(&mut self) {
        // Undo any previous interning work
        self.interner
            .concatenated
            .truncate(self.initial_concatenated_len);
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use lasso::{Key, LargeSpur, MicroSpur, Spur};
    use pretty_assertions::assert_eq;
    use std::fmt::Debug;

    const fn num_bits<T>() -> u32 {
        (std::mem::size_of::<T>() * 8) as u32
    }

    #[test]
    #[should_panic]
    fn bad_sequence_key_bits() {
        SequenceKey::<LargeSpur, { num_bits::<usize>() }>::check_configuration()
    }

    fn test_sequence_key<Inner: Key + Debug, const LEN_BITS: u32>() {
        // Empty ranges should be supported
        let test_round_trip = |input: Range<usize>| {
            let key = SequenceKey::<Inner, LEN_BITS>::try_from_range_usize(input.clone());
            assert_eq!(key.map(SequenceKey::into_range_usize), Some(input));
        };
        test_round_trip(0..0);

        // In the middle of the range, maximal length should be supported
        let offset_bits = num_bits::<Inner>() - LEN_BITS;
        let max_offset = (1 << offset_bits) - 1;
        let max_len = (1 << LEN_BITS) - 1;
        test_round_trip((max_offset / 2)..(max_offset / 2 + max_len));

        // At the end of the range, only max_len - 1 may be supported, due to
        // the implementation of lasso Spurs using NonZero integers
        if max_len >= 1 {
            test_round_trip(max_offset..(max_offset + max_len - 1));
        }

        // An excessive length or offset should error out cleanly
        let test_failed_conversion = |input: Range<usize>| {
            assert_eq!(
                SequenceKey::<Inner, LEN_BITS>::try_from_range_usize(input),
                None
            );
        };
        test_failed_conversion(0..(max_len + 1));
        test_failed_conversion((max_offset + 1)..(max_offset + 2));
    }

    #[test]
    fn sequence_key() {
        test_sequence_key::<MicroSpur, 0>();
        test_sequence_key::<MicroSpur, 1>();
        test_sequence_key::<MicroSpur, 2>();
        test_sequence_key::<MicroSpur, 3>();
        test_sequence_key::<MicroSpur, 4>();
        test_sequence_key::<MicroSpur, 5>();
        test_sequence_key::<MicroSpur, 6>();
        test_sequence_key::<MicroSpur, 7>();
        test_sequence_key::<Spur, 8>();
    }

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
                        .expect("If input could be interned, its key should fit in TestedKey")
                ),
                *input
            );
            seen_so_far += input.len();
        }
    }

    #[test]
    fn initial() {
        let interner = TestedInterner::new();
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
        assert_eq!(interner.num_items(), 0);
        assert_eq!(interner.max_sequence_len(), None);
        test_final_state(&[], interner);
    }

    fn intern_via_entry(interner: &mut TestedInterner, input: &[bool]) -> TestedKey {
        // Prepare to check interner state across operations
        let old_interner = interner.clone();
        let check_same = |expected: &TestedInterner, actual: &TestedInterner| {
            assert_eq!(expected.concatenated, actual.concatenated);
            assert_eq!(expected.len(), actual.len());
            assert_eq!(expected.is_empty(), actual.is_empty());
            assert_eq!(expected.num_items(), actual.num_items());
            assert_eq!(expected.max_sequence_len(), actual.max_sequence_len());
        };

        // Check newly created entry
        let interner_ptr = &*interner as *const TestedInterner;
        let initial_concatenated_len = interner.concatenated.len();
        {
            // Create the entry
            let mut entry = interner.entry();
            assert_eq!(&*entry.interner as *const TestedInterner, interner_ptr);
            assert_eq!(entry.initial_concatenated_len, initial_concatenated_len);
            check_same(&*entry.interner, &old_interner);

            // Check empty entry length
            assert_eq!(entry.len(), 0);
            check_same(&*entry.interner, &old_interner);

            // Check empty entry popping
            assert_eq!(entry.pop(), None);
            assert_eq!(entry.initial_concatenated_len, initial_concatenated_len);
            check_same(&*entry.interner, &old_interner);
        }

        // Make sure dropping the entry without interning has no side effect
        check_same(&old_interner, &*interner);

        // Play with push and pop
        {
            let mut entry = interner.entry();
            for (idx, &item) in input.iter().enumerate() {
                // Push item
                entry.push(item);
                assert_eq!(entry.initial_concatenated_len, initial_concatenated_len);
                assert_eq!(
                    entry.interner.concatenated.len(),
                    initial_concatenated_len + idx + 1
                );
                assert_eq!(
                    &entry.interner.concatenated[initial_concatenated_len..],
                    &input[..=idx]
                );

                // Pop it back
                assert_eq!(entry.pop(), Some(item));
                assert_eq!(entry.initial_concatenated_len, initial_concatenated_len);
                assert_eq!(
                    entry.interner.concatenated.len(),
                    initial_concatenated_len + idx
                );
                assert_eq!(
                    &entry.interner.concatenated[initial_concatenated_len..],
                    &input[..idx]
                );

                // Push it again
                entry.push(item);
            }
        }

        // Again, make sure dropping the without interning has no side effect
        check_same(&old_interner, &*interner);

        // Now seriously make the entry we want
        let mut entry = interner.entry();
        for &item in input {
            entry.push(item);
        }
        entry.intern()
    }

    #[test]
    fn intern_one() {
        fn test_intern(
            input: &[bool],
            mut intern: impl FnMut(&mut TestedInterner, &[bool]) -> TestedKey,
        ) {
            let mut interner = TestedInterner::new();

            let expected_key = TestedKey::try_from_range_usize(0..input.len())
                .expect("The test dataset is chosen so that keys fit in TestedKey");
            assert_eq!(intern(&mut interner, input), expected_key);
            assert_eq!(interner.concatenated, input);
            assert_eq!(interner.len(), 1);
            assert!(!interner.is_empty());
            assert_eq!(interner.num_items(), input.len());
            assert_eq!(interner.max_sequence_len(), Some(input.len()));
            assert_eq!(interner.get(expected_key), input);
            test_final_state(&[input], interner.clone());

            assert_eq!(intern(&mut interner, input), expected_key);
            assert_eq!(interner.concatenated, input);
            assert_eq!(interner.len(), 1);
            assert!(!interner.is_empty());
            assert_eq!(interner.num_items(), input.len());
            assert_eq!(interner.max_sequence_len(), Some(input.len()));
            assert_eq!(interner.get(expected_key), input);
            test_final_state(&[input], interner);
        }

        for input in [[].as_ref(), [false].as_ref(), [true, false].as_ref()] {
            test_intern(input, |interner, input| interner.intern(input));
            test_intern(input, intern_via_entry);
        }
    }

    #[test]
    fn intern_two() {
        fn test_intern(
            input1: &[bool],
            input2: &[bool],
            mut intern: impl FnMut(&mut TestedInterner, &[bool]) -> TestedKey,
        ) {
            let mut interner = TestedInterner::new();
            let key1 = interner.intern(input1);
            let interner1 = interner.clone();

            let key2 = TestedKey::try_from_range_usize(
                interner.num_items()..interner.num_items() + input2.len(),
            )
            .expect("The test dataset is chosen so that keys fit in TestedKey");
            assert_eq!(intern(&mut interner, input2), key2);
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
            assert_eq!(interner.num_items(), interner1.num_items() + input2.len());
            assert_eq!(
                interner.max_sequence_len(),
                Some(input1.len().max(input2.len()))
            );
            assert_eq!(interner.get(key1), input1);
            assert_eq!(interner.get(key2), input2);
            test_final_state(&[input1, input2], interner.clone());

            assert_eq!(intern(&mut interner, input1), key1);
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
            assert_eq!(interner.num_items(), interner1.num_items() + input2.len());
            assert_eq!(
                interner.max_sequence_len(),
                Some(input1.len().max(input2.len()))
            );
            assert_eq!(interner.get(key1), input1);
            assert_eq!(interner.get(key2), input2);
            test_final_state(&[input1, input2], interner.clone());

            assert_eq!(intern(&mut interner, input2), key2);
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
            assert_eq!(interner.num_items(), interner1.num_items() + input2.len());
            assert_eq!(
                interner.max_sequence_len(),
                Some(input1.len().max(input2.len()))
            );
            assert_eq!(interner.get(key1), input1);
            assert_eq!(interner.get(key2), input2);
            test_final_state(&[input1, input2], interner);
        }

        let test_dataset = [[].as_ref(), [false].as_ref(), [true, false].as_ref()];
        for input1 in test_dataset {
            for input2 in test_dataset {
                if input2 == input1 {
                    continue;
                } else {
                    test_intern(input1, input2, |interner, input| interner.intern(input));
                    test_intern(input1, input2, intern_via_entry);
                }
            }
        }
    }
}
