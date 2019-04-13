use super::{print_error, PrintError};
use std::ops::RangeInclusive;

#[derive(Debug, PartialEq)]
pub enum Error {
    BadIndex(usize),
    BadRange(RangeInclusive<usize>),
    EmptyHistory,
}

impl PrintError for Error {
    fn print(&self) {
        match self {
            Error::BadIndex(index) => print_error!("bad index", "{}", index),
            Error::BadRange(range) => {
                print_error!("bad range", "{}..{}", range.start(), range.end())
            }
            Error::EmptyHistory => print_error!("empty history"),
        }
    }
}

// TODO mock out the eprintln

#[derive(Clone, Debug)]
pub struct History(Vec<(usize, String)>);

impl Default for History {
    fn default() -> Self {
        Self(vec![])
    }
}

impl History {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    pub fn get(&self, index: usize) -> Result<&String, Error> {
        self.0
            .get(index)
            .map(|(_, c)| c)
            .ok_or_else(|| Error::BadIndex(index))
    }

    pub fn show_index(&self, index: usize) -> Result<(), Error> {
        if self.0.is_empty() {
            return Err(Error::EmptyHistory);
        }
        self.0
            .get(index)
            .ok_or_else(|| Error::BadIndex(index))
            .map(|(_, item)| {
                for line in item.split('\n') {
                    eprintln!("  {}", line.trim_end());
                }
            })
    }

    pub fn show_range(&self, range: RangeInclusive<usize>) -> Result<(), Error> {
        if self.0.is_empty() {
            return Err(Error::EmptyHistory);
        }
        self.0
            .get(range.clone())
            .ok_or_else(|| Error::BadRange(range.clone()))
            .map(Self::show_many)
    }

    pub fn show_all(&self) -> Result<(), Error> {
        if self.0.is_empty() {
            return Err(Error::EmptyHistory);
        }
        const MAX: usize = 3;
        let start = self.0.len().saturating_sub(MAX);
        self.0
            .get(start..std::cmp::min(self.0.len(), start + MAX))
            .ok_or_else(|| Error::EmptyHistory)
            .map(Self::show_many)
    }

    pub fn zap_index(&mut self, index: usize) -> Result<(), Error> {
        if self.0.is_empty() {
            return Err(Error::EmptyHistory);
        }
        if self.0.len() > index {
            self.0.remove(index);
            return Ok(());
        }
        Err(Error::BadIndex(index))
    }

    pub fn zap_range(&mut self, range: RangeInclusive<usize>) -> Result<(), Error> {
        if self.0.is_empty() {
            return Err(Error::EmptyHistory);
        }

        if *range.end() < self.len() {
            self.0.drain(range.clone()).for_each(|e| ());
            return Ok(());
        }

        Err(Error::BadRange(range))
    }

    pub fn zap_all(&mut self) -> Result<(), Error> {
        if self.0.is_empty() {
            return Err(Error::EmptyHistory);
        }
        self.0.clear();
        Ok(())
    }

    pub(crate) fn add_item(&mut self, index: usize, item: String) {
        self.0.push((index, item))
    }

    pub(crate) fn clear(&mut self) {
        self.0.clear();
    }

    fn show_many(items: &[(usize, String)]) {
        let pad = super::util::count_digits(items.len() as u64);
        for (i, item) in items.iter().rev() {
            for line in item.split('\n') {
                eprintln!("  {: <pad$}  {}", i, line.trim_end(), pad = pad)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::prelude::*;

    fn generate_history() -> History {
        let mut history = History::default();
        let mut rng = thread_rng();
        for cmd in std::iter::repeat_with(|| {
            std::iter::once(':')
                .chain(rng.sample_iter(&rand::distributions::Alphanumeric).take(4))
                .collect::<String>()
        })
        .take(10)
        .enumerate()
        {
            let (i, cmd) = cmd;
            history.add_item(i, cmd);
        }
        history
    }

    #[test]
    fn show_index() {
        let mut history = generate_history();
        history.show_index(0).unwrap();

        assert_eq!(
            history.show_index(999).unwrap_err(), //
            Error::BadIndex(999)
        );

        history.clear();
        assert_eq!(
            history.show_index(0).unwrap_err(), //
            Error::EmptyHistory,
        );
    }

    #[test]
    fn show_range() {
        let mut history = generate_history();
        history.show_range(0..=5).unwrap();

        assert_eq!(
            history.show_range(9..=10).unwrap_err(), //
            Error::BadRange(9..=10)
        );

        assert_eq!(
            history.show_range(10..=500).unwrap_err(), //
            Error::BadRange(10..=500)
        );

        history.clear();
        assert_eq!(
            history.show_range(0..=5).unwrap_err(), //
            Error::EmptyHistory,
        );
    }

    #[test]
    fn show_all() {
        let mut history = generate_history();
        history.show_all().unwrap();
        history.clear();

        assert_eq!(
            history.show_all().unwrap_err(), //
            Error::EmptyHistory,
        );
    }

    #[test]
    fn zap_index() {
        let mut history = generate_history();
        history.zap_index(3).unwrap();
        assert_eq!(history.len(), 9);

        assert_eq!(
            history.zap_index(10).unwrap_err(), //
            Error::BadIndex(10),
        );

        history.clear();
        assert_eq!(
            history.zap_index(3).unwrap_err(), //
            Error::EmptyHistory,
        );
    }

    #[test]
    fn zap_range() {
        let mut history = generate_history();
        history.zap_range(3..=9).unwrap();
        assert_eq!(history.len(), 3);

        assert_eq!(
            history.zap_range(5..=6).unwrap_err(), //
            Error::BadRange(5..=6),
        );

        history.clear();
        assert_eq!(
            history.zap_range(0..=1).unwrap_err(), //
            Error::EmptyHistory,
        );
    }

    #[test]
    fn zap_all() {
        let mut history = generate_history();
        history.zap_all().unwrap();
        assert_eq!(history.len(), 0);

        assert_eq!(
            history.zap_all().unwrap_err(), //
            Error::EmptyHistory,
        );

        history.clear();
        assert_eq!(
            history.zap_all().unwrap_err(), //
            Error::EmptyHistory,
        );
    }
}
