//! Tools for dealing with interned slices

use crate::{
    asylum::{
        lasso::{Key, Spur},
        sequence::{InternedSequences, SequenceKey},
    },
    display::{CustomDisplay, RecursionDepths},
    Entities,
};
use std::{
    fmt::{self, Display, Formatter},
    iter::{DoubleEndedIterator, FusedIterator},
    marker::PhantomData,
};

/// A view of an interned slice
pub struct SliceView<
    'entities,
    Item: Clone,
    ItemView: SliceItemView<'entities, Inner = Item>,
    KeyImpl: Key = Spur,
    const LEN_BITS: u32 = 8,
> {
    /// Key used to retrieve the slice
    key: SequenceKey<KeyImpl, LEN_BITS>,

    /// Wrapped slice
    inner: &'entities [Item],

    /// Underlying interned entity storage
    entities: &'entities Entities,

    /// Making rustc happy
    view: PhantomData<*const ItemView>,
}
//
impl<
        'entities,
        Item: Clone,
        ItemView: SliceItemView<'entities, Inner = Item>,
        KeyImpl: Key,
        const LEN_BITS: u32,
    > SliceView<'entities, Item, ItemView, KeyImpl, LEN_BITS>
{
    /// Set up a new slice view
    pub fn new(
        key: SequenceKey<KeyImpl, LEN_BITS>,
        sequences: &'entities InternedSequences<Item, KeyImpl, LEN_BITS>,
        entities: &'entities Entities,
    ) -> Self {
        Self {
            key,
            inner: sequences.get(key),
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
    ) -> impl Iterator<Item = ItemView>
           + Clone
           + DoubleEndedIterator
           + ExactSizeIterator
           + FusedIterator
           + 'entities {
        self.inner
            .iter()
            .map(|item| ItemView::new(item.clone(), self.entities))
    }
}
//
impl<
        'entities,
        Item: Clone,
        ItemView: SliceItemView<'entities, Inner = Item>,
        KeyImpl: Key,
        const LEN_BITS: u32,
    > PartialEq for SliceView<'entities, Item, ItemView, KeyImpl, LEN_BITS>
{
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const Entities == other.entities as *const Entities)
            && (self.key == other.key)
    }
}
//
impl<
        'entities,
        Item: Clone,
        ItemView: SliceItemView<'entities, Inner = Item>,
        KeyImpl: Key,
        const LEN_BITS: u32,
    > CustomDisplay for SliceView<'entities, Item, ItemView, KeyImpl, LEN_BITS>
{
    fn recursion_depths(&self) -> RecursionDepths {
        let mut depths = self
            .iter()
            .map(|item| item.recursion_depths())
            .fold(RecursionDepths::NONE, |acc, item| acc.max(item));
        *ItemView::get_recursion_depth(&mut depths) += (self.iter().count() != 0) as usize;
        depths
    }
}
//
impl<
        'entities,
        Item: Clone,
        ItemView: SliceItemView<'entities, Inner = Item>,
        KeyImpl: Key,
        const LEN_BITS: u32,
    > Display for SliceView<'entities, Item, ItemView, KeyImpl, LEN_BITS>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", ItemView::DISPLAY_HEADER)?;
        let mut iterator = self.iter().peekable();
        while let Some(view) = iterator.next() {
            write!(f, "{view}")?;
            if iterator.peek().is_some() {
                write!(f, "{}", ItemView::DISPLAY_SEPARATOR)?;
            }
        }
        write!(f, "{}", ItemView::DISPLAY_TRAILER)
    }
}

/// Trait to be implemented by views of all items that appear in a slice
// FIXME: Remove Display bound once CustomDisplay can replace it
pub trait SliceItemView<'entities>: CustomDisplay + Display + PartialEq {
    /// Underlying storage type that this view wraps
    type Inner: 'entities;

    /// Wrap the storage type
    fn new(inner: Self::Inner, entities: &'entities Entities) -> Self;

    /// Find the recursion depth for this in the RecursionDepths struct
    fn get_recursion_depth(depths: &mut RecursionDepths) -> &mut usize;

    /// String to be used at the beginning of the display
    const DISPLAY_HEADER: &'static str;

    /// String to be used inbetween two displayed elements
    const DISPLAY_SEPARATOR: &'static str;

    /// String to be used at the end of the display
    const DISPLAY_TRAILER: &'static str;
}
