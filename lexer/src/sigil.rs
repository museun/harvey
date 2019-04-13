macro_rules! impl_as_sigil {
    ($($name:ident = $token:expr);* $(;)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Hash)]
        pub enum Sigil {
            $($name,)*
        }

        impl Sigil {
            pub fn lookup(s: &str) -> Option<Self> {
                Some(match s {
                   $($token => Sigil::$name,)*
                   _ => return None,
                })
            }

            #[inline(always)]
            #[allow(dead_code)]
            pub(crate) fn len(self) -> usize {
                match self {
                    $(Sigil::$name => $token.len(),)*
                }
            }
        }

        impl std::fmt::Display for Sigil {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Sigil::$name => write!(f, "{}", $token),)*
                }
            }
        }
    };
}

impl_as_sigil! {
    Unit         = "()";
    OpenBrace    = "{";
    CloseBrace   = "}";
    OpenParen    = "(";
    CloseParen   = ")";
    OpenBracket  = "[";
    CloseBracket = "]";
    Question     = "?";
    Bang         = "!";
    Pipe         = "|";
    Greater      = ">";
    Less         = "<";
    Comma        = ",";
    Dot          = ".";
    Prime        = "'";
    Tick         = "`";
    Star         = "*";
    Backslash    = "\\";
    Slash        = "/";
    Minus        = "-";
    Equal        = "=";
    EqualEqual   = "==";
    BangEqual    = "!=";
    GreaterEqual = ">=";
    LessEqual    = "<=";
    Underscore   = "_";
    Plus         = "+";
    Colon        = ":";
    SemiColon    = ";";
    Arrow        = "->";
    BackArrow    = "<-";
}

impl From<Sigil> for crate::Token {
    fn from(sigil: Sigil) -> Self {
        crate::Token::Sigil(sigil)
    }
}

impl PartialEq<crate::Token> for Sigil {
    fn eq(&self, other: &crate::Token) -> bool {
        if let crate::Token::Sigil(sig) = other {
            return self.eq(sig);
        }
        false
    }
}
