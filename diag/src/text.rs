use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use super::Span;

/// like a slice but owned and cheaply clonable
#[derive(Clone)]
pub struct Text {
    text: Rc<String>,
    start: usize,
    end: usize,
}

impl Text {
    /// narrow the string to a range
    pub fn select(&mut self, range: Range<usize>) {
        let len = range.end - range.start;
        let start = self.start + range.start;
        let end = start + len;
        assert!(end <= self.end);
        self.start = start;
        self.end = end;
    }

    pub fn extract(&self, range: Range<usize>) -> Self {
        let mut this = self.clone();
        this.select(range);
        this
    }

    pub fn get<F: super::SpanFile>(&self, span: Span<F>) -> Option<&str> {
        if span.len() >= self.len() {
            return None;
        }
        Some(&self[span])
    }
}

impl<F: super::SpanFile> std::ops::Index<Span<F>> for Text {
    type Output = str;
    fn index(&self, span: Span<F>) -> &Self::Output {
        let s: &str = self;
        &s[span]
    }
}

impl From<Rc<String>> for Text {
    fn from(text: Rc<String>) -> Self {
        let end = text.len();
        Self {
            text,
            start: 0,
            end,
        }
    }
}

impl From<String> for Text {
    fn from(text: String) -> Self {
        Text::from(Rc::new(text))
    }
}

impl From<&String> for Text {
    fn from(text: &String) -> Self {
        Text::from(Rc::new(text.clone()))
    }
}

impl From<&str> for Text {
    fn from(text: &str) -> Self {
        Text::from(Rc::new(text.to_owned()))
    }
}

impl std::borrow::Borrow<str> for Text {
    fn borrow(&self) -> &str {
        &*self
    }
}

impl std::ops::Deref for Text {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.text[self.start..self.end]
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        &*self
    }
}

impl std::fmt::Debug for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <str as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <str as std::fmt::Display>::fmt(self, f)
    }
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        let this: &str = self;
        let other: &str = other;
        this == other
    }
}

impl Eq for Text {}

impl PartialEq<str> for Text {
    fn eq(&self, other: &str) -> bool {
        let this: &str = self;
        this == other
    }
}

impl PartialEq<String> for Text {
    fn eq(&self, other: &String) -> bool {
        let this: &str = self;
        let other: &str = other;
        this == other
    }
}

impl PartialEq<Text> for str {
    fn eq(&self, other: &Text) -> bool {
        self == other
    }
}

impl PartialEq<Text> for String {
    fn eq(&self, other: &Text) -> bool {
        self == other
    }
}

impl<T: ?Sized> PartialEq<&T> for Text
where
    Text: PartialEq<T>,
{
    fn eq(&self, other: &&T) -> bool {
        self == *other
    }
}

impl Hash for Text {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <str as Hash>::hash(self, state)
    }
}
