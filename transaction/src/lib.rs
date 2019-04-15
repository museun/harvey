pub struct Transaction<'a, T: Clone> {
    initial: &'a mut T,
    scratch: T,
    save: bool,
}

impl<'a, T: Clone> Transaction<'a, T> {
    pub fn new(item: &'a mut T) -> Self {
        Self {
            scratch: item.clone(), // <-- replace this with an arc swap or an unsafe cell
            initial: item,
            save: false,
        }
    }

    pub fn commit(mut self) {
        self.save = true;
    }
}

impl<'a, T: Clone> Drop for Transaction<'a, T> {
    fn drop(&mut self) {
        if self.save {
            std::mem::swap(self.initial, &mut self.scratch);
        }
    }
}

impl<'a, T: Clone> std::ops::Deref for Transaction<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.scratch
    }
}

impl<'a, T: Clone> std::ops::DerefMut for Transaction<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.scratch
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transaction() {
        #[derive(Clone, Debug)]
        struct S {
            d: u32,
            list: Vec<bool>,
        }

        let mut s = S {
            d: 42,
            list: vec![false; 3],
        };

        assert_eq!(s.d, 42);
        assert_eq!(s.list, vec![false; 3]);

        {
            let mut tx = Transaction::new(&mut s);
            tx.d = 5;
            tx.commit()
        }
        assert_eq!(s.d, 5);
        assert_eq!(s.list, vec![false; 3]);

        {
            let mut tx = Transaction::new(&mut s);
            tx.list[1] = true;
        }
        assert_eq!(s.d, 5);
        assert_eq!(s.list, vec![false; 3]);

        {
            let mut tx = Transaction::new(&mut s);
            tx.d = 7;
            tx.list[1] = true;
            tx.commit()
        }
        assert_eq!(s.d, 7);
        assert_eq!(s.list, vec![false, true, false]);
    }
}
