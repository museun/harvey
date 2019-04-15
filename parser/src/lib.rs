#![allow(dead_code)]
use diag::{Diagnostic, ErrorReported, FileName, Span, Spanned, Text};

mod syntax;
pub(crate) use syntax::Syntax;

mod parsers;
mod syntaxes;

mod parser;
pub use self::parser::Parser;
