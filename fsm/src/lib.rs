#[allow(clippy::len_without_is_empty)]
pub trait State: Sized
where
    Self: Clone + Copy + PartialEq + PartialOrd,
    Self: Into<u8> + std::convert::TryFrom<u8>,
{
    const MAX: u8;

    fn start() -> Self {
        Self::try_from(0).ok().unwrap()
    }

    fn end() -> Self {
        Self::try_from(Self::MAX - 1).ok().unwrap()
    }

    fn next(&mut self) -> Self {
        let t = (*self).into();
        let next = (t + 1) % Self::MAX;
        std::mem::replace(self, Self::try_from(next).ok().unwrap())
    }

    fn previous(&mut self) -> Self {
        let t = (*self).into();
        let prev = if t == 0 { Self::MAX } else { t } - 1;
        std::mem::replace(self, Self::try_from(prev).ok().unwrap())
    }

    fn goto(&mut self, other: Self) -> Self {
        std::mem::replace(self, other)
    }

    fn len(&self) -> usize {
        Self::MAX as usize
    }

    fn into_iter(self) -> StateIter<Self> {
        let pos: u8 = self.into();
        StateIter {
            item: self,
            pos: pos as usize,
        }
    }
}

pub trait StateFlip: State {
    fn flip(&mut self) -> Self {
        self.next()
    }
}

pub struct StateIter<T>
where
    T: State,
{
    item: T,
    pos: usize,
}

impl<T> Iterator for StateIter<T>
where
    T: State,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.item.next();
        let p: u8 = next.into();
        self.pos = p as usize;
        Some(next)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.pos, Some(T::MAX as usize))
    }
}

impl<T> ExactSizeIterator for StateIter<T> where T: State {}

impl<T> DoubleEndedIterator for StateIter<T>
where
    T: State,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let next = self.item.previous();
        let p: u8 = next.into();
        self.pos = p as usize;
        Some(next)
    }
}
