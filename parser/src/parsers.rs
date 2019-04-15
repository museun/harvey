use crate::{Parser, Syntax};
use diag::ErrorReported;
use lexer::{self, Keyword, Sigil, Token};

type Result<T> = std::result::Result<T, ErrorReported>;

macro_rules! use_this {
    ($($m:ident);* $(;)?) => {
        $(
            mod $m;
            pub use self::$m::*;
        )*
    };
}

use_this! {
    delimited;
    guard;
    list;
    sink;
    surround;
    tokens;
    or;
}
