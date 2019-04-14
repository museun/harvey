use crate::{parsers::*, Parser, Syntax};
use diag::ErrorReported;
use lexer::*;

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
    directive;
    variable;
    // consts;
    // literal;
}
