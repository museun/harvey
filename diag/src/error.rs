use super::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ErrorReported(pub Span<FileName>);

impl ErrorReported {
    pub fn at_span(span: Span<FileName>) -> Self {
        Self(span)
    }

    pub fn at_diagnostic(s: &Diagnostic) -> Self {
        Self(s.span)
    }

    pub fn at_diagnostics(s: &[Diagnostic]) -> Self {
        assert!(!s.is_empty());
        Self(s[0].span)
    }

    pub fn span(&self) -> Span<FileName> {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct WithError<T> {
    pub value: T,
    pub errors: Vec<Diagnostic>,
}

impl<T> WithError<T> {
    pub fn ok(value: T) -> Self {
        Self {
            value,
            errors: vec![],
        }
    }

    pub fn report_error(label: impl ToString, span: Span<FileName>) -> WithError<T>
    where
        T: Sentinel,
    {
        let diag = Diagnostic::new(span, label);
        Self {
            value: T::sentinel(ErrorReported::at_diagnostic(&diag)),
            errors: vec![diag],
        }
    }

    pub fn accumulate(self, vec: &mut Vec<Diagnostic>) -> T {
        vec.extend(self.errors);
        self.value
    }

    pub fn into_value(self) -> T {
        self.value
    }

    pub fn into_result(self) -> Result<T, ErrorReported> {
        if !self.errors.is_empty() {
            Err(ErrorReported::at_diagnostics(&self.errors))
        } else {
            Ok(self.into_value())
        }
    }

    pub fn map<U>(self, op: impl FnOnce(T) -> U) -> WithError<U> {
        WithError {
            value: op(self.value),
            errors: self.errors,
        }
    }
}

pub trait Sentinel {
    fn sentinel(report: ErrorReported) -> Self;
}

impl<T> Sentinel for Result<T, ErrorReported> {
    fn sentinel(report: ErrorReported) -> Self {
        Err(report)
    }
}

impl<T> Sentinel for WithError<T>
where
    T: Sentinel,
{
    fn sentinel(report: ErrorReported) -> Self {
        WithError::ok(T::sentinel(report))
    }
}

impl<T> Sentinel for Vec<T>
where
    T: Sentinel,
{
    fn sentinel(report: ErrorReported) -> Self {
        vec![T::sentinel(report)]
    }
}
