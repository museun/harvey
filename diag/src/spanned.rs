use super::{SpanFile, Text};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span<F: SpanFile> {
    file: F,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl<F: SpanFile> Span<F> {
    pub fn new(file: F, start: usize, end: usize) -> Self {
        assert!(end >= start, "{} < {}", end, start);
        Self { file, start, end }
    }

    /// Empty span pointing to the start of the file
    pub fn initial(file: F) -> Self {
        Self::new(file, 0, 0)
    }

    /// Empty span pointing at the end of the file
    pub fn eof(file: F, text: &Text) -> Self {
        let len = text.len();
        Self::new(file, len, len)
    }

    /// Extend this span to the end of the other span
    pub fn extend(self, other: Span<F>) -> Self {
        assert_eq!(self.file, other.file);
        assert!(self.start <= other.end);
        Self::new(self.file, self.start, other.end)
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T, F: SpanFile> {
    pub value: T,
    pub span: Span<F>,
    pub location: Location,
}

impl<T: std::fmt::Debug, F: SpanFile> std::fmt::Debug for Spanned<T, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("Spanned")
                .field("value", &self.value)
                .field("line", &self.location.line)
                .field("start", &self.span.start())
                .field("end", &self.span.end())
                .finish()
        } else {
            write!(
                f,
                "{:?} ({}:{}..{})",
                self.value,
                self.location.line,
                self.span.start(),
                self.span.end(),
            )
        }
    }
}

impl<T, F: SpanFile> Spanned<T, F> {
    pub fn new(value: T, span: Span<F>, location: Location) -> Self {
        Self {
            value,
            span,
            location,
        }
    }

    pub fn map<U>(self, value: impl FnOnce(T) -> U) -> Spanned<U, F> {
        Spanned {
            value: value(self.value),
            span: self.span,
            location: self.location,
        }
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        self.location.column - (self.span.end - self.span.start) + 1..self.location.column
    }
}

impl<T, F: SpanFile> std::ops::Deref for Spanned<T, F> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    /// 1 based line number
    pub line: usize,
    /// 1 based column number
    pub column: usize,
    /// Byte index into file
    pub index: usize,
}

impl Location {
    pub fn empty() -> Self {
        Self {
            line: 0,
            column: 0,
            index: 0,
        }
    }
}
