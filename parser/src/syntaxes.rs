use crate::{Parser, Syntax,parsers};
use diag::ErrorReported;
use lexer::{Token, Lexer, Keyword, Sigil, Primitive,UnitToken};

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
    path;
    import;
    identifier;
}
