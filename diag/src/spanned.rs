use super::*;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T, F: SpanFile> {
    pub value: T,
    pub span: Span<F>,
}

impl<T: std::fmt::Debug, F: SpanFile> std::fmt::Debug for Spanned<T, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("Spanned")
                .field("value", &self.value)
                .field("line", &self.span.line())
                .field("start", &self.span.start())
                .field("end", &self.span.end())
                .finish()
        } else {
            write!(
                f,
                "{:?} ({}:{}..{})",
                self.value,
                self.span.line(),
                self.span.start(),
                self.span.end(),
            )
        }
    }
}

impl<T, F: SpanFile> Spanned<T, F> {
    pub fn new(value: T, span: Span<F>) -> Self {
        Self { value, span }
    }

    pub fn map<U>(self, value: impl FnOnce(T) -> U) -> Spanned<U, F> {
        Spanned {
            value: value(self.value),
            span: self.span,
        }
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        self.span.start()..self.span.end()
        // self.location.column - (self.span.end - self.span.start) + 1..self.location.column
    }
}

impl<T, F: SpanFile> std::ops::Deref for Spanned<T, F> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
