use super::*;

// TODO make this work with Printer
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Diagnostic {
    pub span: Span<FileName>,
    pub message: String,
}

impl Diagnostic {
    pub fn new(span: Span<FileName>, message: impl ToString) -> Self {
        Self {
            span,
            message: message.to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Level {
    Info,
    Warning,
    Error, // TODO error code
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Flag<F: SpanFile> {
    pub level: Level,
    pub label: Spanned<Label, F>,
}

impl<F: SpanFile> Flag<F> {
    pub fn new(level: Level, span: Span<F>, label: impl ToString) -> Self {
        Self {
            level,
            label: Spanned::new(
                Label {
                    label: label.to_string(),
                },
                span,
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Label {
    pub label: String,
}
