use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span<F: SpanFile> {
    file: F,
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) line: usize,
}

impl<F: SpanFile> Span<F> {
    pub fn new(file: F, start: usize, end: usize, line: usize) -> Self {
        assert!(end >= start, "{} < {}", end, start);
        Self {
            file,
            start,
            end,
            line,
        }
    }

    /// Empty span pointing to the start of the file
    pub fn initial(file: F) -> Self {
        Self::new(file, 0, 0, 0)
    }

    /// Empty span pointing at the end of the file
    pub fn eof(file: F, text: &Text) -> Self {
        let len = text.len();
        Self::new(file, len, len, 0)
    }

    /// Extend this span to the end of the other span
    pub fn extend(self, other: Span<F>) -> Self {
        assert_eq!(self.file, other.file);
        assert!(self.start <= other.end);
        Self::new(self.file, self.start, other.end, other.line)
    }

    pub fn file(&self) -> F {
        self.file
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains(&self, span: Span<F>) -> bool {
        self.start >= span.start && self.end < span.end
    }

    pub fn contains_index(&self, index: usize) -> bool {
        self.start <= index && index < self.end
    }
}

impl<F: SpanFile> std::ops::Index<Span<F>> for str {
    type Output = str;
    fn index(&self, range: Span<F>) -> &Self::Output {
        &self[range.start..range.end]
    }
}
