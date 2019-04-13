use crate::{Parser, Syntax};
use diag::ErrorReported;

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
    sink;
    surround;
    token;
    tokens;
    sigil;
}
