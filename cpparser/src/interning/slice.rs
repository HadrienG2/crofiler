//! Tools for dealing with interned slices

use crate::{
    asylum::{lasso::Spur, sequence::SequenceKey, InternerKey, Resolver},
    display::{CustomDisplay, DisplayState},
    EntityParser,
};
use reffers::ARef;
use std::{
    fmt::{self, Display, Formatter},
    hash::Hash,
    iter::{DoubleEndedIterator, FusedIterator},
    marker::PhantomData,
    ops::Range,
};

/// A view of an interned slice
pub struct SliceView<
    'entities,
    Item: Clone + Eq + Hash,
    ItemView: SliceItemView<'entities, Inner = Item>,
    Key: InternerKey<ImplKey = Range<usize>> = SequenceKey<Spur, 8>,
> {
    /// Key used to retrieve the slice
    key: Key,

    /// Wrapped slice
    inner: ARef<'entities, [Item]>,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,

    /// Making rustc happy
    view: PhantomData<*const ItemView>,
}
//
impl<
        'entities,
        Item: Clone + Eq + Hash,
        ItemView: SliceItemView<'entities, Inner = Item>,
        Key: InternerKey<ImplKey = Range<usize>>,
    > SliceView<'entities, Item, ItemView, Key>
{
    /// Set up a new slice view
    pub fn new<SeqResolver>(
        key: Key,
        sequences: impl Into<ARef<'entities, SeqResolver>>,
        entities: &'entities EntityParser,
    ) -> Self
    where
        SeqResolver: Resolver<Key = Key, Item = [Item]> + 'entities,
    {
        let sequences: ARef<'entities, SeqResolver> = sequences.into();
        Self {
            key,
            inner: sequences.map(|sequences| sequences.get(key)),
            entities,
            view: PhantomData,
        }
    }

    /// Truth that the slice is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Number of contained items
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Iterate over items from the slice view
    pub fn iter(
        &self,
    ) -> impl DoubleEndedIterator<Item = ItemView>
           + Clone
           + ExactSizeIterator
           + FusedIterator
           + '_
           + Captures<'entities> {
        self.inner
            .iter()
            .map(move |item| ItemView::new(item.clone(), self.entities))
    }
}
//
/// Workaround for impl Trait limitation
pub trait Captures<'a> {}
impl<T: ?Sized> Captures<'_> for T {}
//
impl<
        'entities,
        Item: Clone + Eq + Hash,
        ItemView: SliceItemView<'entities, Inner = Item>,
        Key: InternerKey<ImplKey = Range<usize>>,
    > PartialEq for SliceView<'entities, Item, ItemView, Key>
{
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.key == other.key)
    }
}
//
impl<
        'entities,
        Item: Clone + Eq + Hash,
        ItemView: SliceItemView<'entities, Inner = Item>,
        Key: InternerKey<ImplKey = Range<usize>>,
    > CustomDisplay for SliceView<'entities, Item, ItemView, Key>
{
    fn recursion_depth(&self) -> usize {
        self.iter()
            .map(|item| item.recursion_depth() + 1)
            .max()
            .unwrap_or(0)
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        write!(f, "{}", ItemView::DISPLAY_HEADER)?;
        if let Ok(_guard) = state.recurse() {
            let mut iterator = self.iter().peekable();
            while let Some(view) = iterator.next() {
                view.display_impl(f, state)?;
                if iterator.peek().is_some() {
                    write!(f, "{}", ItemView::DISPLAY_SEPARATOR)?;
                }
            }
        } else if self.iter().count() > 0 {
            write!(f, "…")?;
        }
        write!(f, "{}", ItemView::DISPLAY_TRAILER)
    }
}
//
impl<
        'entities,
        Item: Clone + Eq + Hash,
        ItemView: SliceItemView<'entities, Inner = Item>,
        Key: InternerKey<ImplKey = Range<usize>>,
    > Display for SliceView<'entities, Item, ItemView, Key>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::new(usize::MAX))
    }
}

/// Trait to be implemented by views of all items that appear in a slice
pub trait SliceItemView<'entities>: CustomDisplay + PartialEq {
    /// Underlying storage type that this view wraps
    type Inner: 'entities;

    /// Wrap the storage type
    fn new(inner: Self::Inner, entities: &'entities EntityParser) -> Self;

    /// String to be used at the beginning of the display
    const DISPLAY_HEADER: &'static str;

    /// String to be used inbetween two displayed elements
    const DISPLAY_SEPARATOR: &'static str;

    /// String to be used at the end of the display
    const DISPLAY_TRAILER: &'static str;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::CustomDisplayMock, EntityParser};
    use asylum::sequence::{SequenceInterner, SequenceKey};

    // Fake slice item to test SliceView
    type TestItem = usize;
    //
    type TestItemView = CustomDisplayMock;
    //
    impl<'entities> SliceItemView<'entities> for TestItemView {
        type Inner = TestItem;

        fn new(inner: Self::Inner, _entities: &'entities EntityParser) -> Self {
            Self(inner)
        }

        const DISPLAY_HEADER: &'static str = "^";

        const DISPLAY_SEPARATOR: &'static str = "~";

        const DISPLAY_TRAILER: &'static str = "$";
    }

    /// Sequence interning setup
    type TestSequenceInterner = SequenceInterner<TestItem, SequenceKey>;
    type TestSliceView<'entities> = SliceView<'entities, TestItem, TestItemView, SequenceKey>;

    /// Sequences to be tested
    static TEST_SEQUENCES: &[&[TestItem]] = &[&[], &[42], &[0, 7], &[2, 5, 1]];

    /// Test harness for slices
    fn setup_and_check(check: impl Fn(&[TestItem], TestSliceView)) {
        let entities = EntityParser::new();
        for sequence in TEST_SEQUENCES {
            let mut sequences = TestSequenceInterner::new();
            let key = sequences.intern(sequence);
            let view = TestSliceView::new(key, &sequences, &entities);
            check(sequence, view)
        }
    }

    /// Test basic initial state
    #[test]
    fn accessors() {
        setup_and_check(|sequence, view| {
            assert_eq!(view.is_empty(), sequence.is_empty());
            assert_eq!(view.len(), sequence.len());

            let expected_recursion_depth = sequence.iter().max().map(|x| x + 1).unwrap_or(0);
            assert_eq!(view.recursion_depth(), expected_recursion_depth);
        })
    }

    fn expected_custom_display(sequence: &[TestItem], recursion_depth: usize) -> String {
        let mut expected = String::new();
        expected.push('^');
        if recursion_depth > 0 {
            for (idx, item) in sequence.iter().cloned().enumerate() {
                expected.push_str(
                    &CustomDisplayMock(item)
                        .display(&DisplayState::new(recursion_depth - 1))
                        .to_string(),
                );
                if idx != sequence.len() - 1 {
                    expected.push('~');
                }
            }
        } else if !sequence.is_empty() {
            expected.push('…');
        }
        expected.push('$');
        expected
    }

    /// Test full (non-customized) display
    #[test]
    fn display() {
        setup_and_check(|sequence, view| {
            assert_eq!(
                expected_custom_display(sequence, usize::MAX),
                view.to_string()
            );
        })
    }

    /// Test customized display
    #[test]
    fn custom_display() {
        setup_and_check(|sequence, view| {
            for recursion_depth in 0..sequence.iter().max().unwrap_or(&0) + 1 {
                assert_eq!(
                    expected_custom_display(sequence, recursion_depth),
                    view.display(&DisplayState::new(recursion_depth))
                        .to_string()
                );
            }
        })
    }

    /// Test equality operator
    #[test]
    fn equality() {
        let entities1 = EntityParser::new();
        let mut entities2 = EntityParser::new();
        entities2
            .parse_entity("<unknown>")
            .expect("This is a known-good parse which should not fail");

        for sequence1 in TEST_SEQUENCES {
            for sequence2 in TEST_SEQUENCES {
                let mut sequences = TestSequenceInterner::new();
                let key1 = sequences.intern(sequence1);
                let key2 = sequences.intern(sequence2);

                let view11 = TestSliceView::new(key1, &sequences, &entities1);
                let view21 = TestSliceView::new(key2, &sequences, &entities1);
                let view12 = TestSliceView::new(key1, &sequences, &entities2);

                assert_eq!(view11 == view21, sequence1 == sequence2);
                assert!(view11 != view12);
                assert!(view21 != view12);
            }
        }
    }
}
