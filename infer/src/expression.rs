use super::Result;
use super::{Context, Literal, RcString, State, Type};

use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression<Lit: Literal> {
    Var(RcString),
    Decl(RcString, Box<Self>),
    Apply(Box<Self>, Box<Self>),
    Anno(Box<Self>, Type<Lit>),
    Let(RcString, Box<Self>, Box<Self>),
    Tuple(Box<Self>, Box<Self>),
    Literal(Lit),
}

impl<Lit: Literal> Display for Expression<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Var(binding) => write!(f, "{}", binding),
            Decl(binding, expr) => write!(f, "(\\{} -> {})", binding, expr), // higher rank
            Apply(left, right) => write!(f, "{} {}", left, right),
            Anno(expr, ty) => write!(f, "({}: {})", expr, ty),
            Let(var, expr, body) => write!(f, "let {} = {} in {}", var, expr, body),
            Tuple(fst, snd) => write!(f, "({}, {})", fst, snd),
            Literal(lit) => write!(f, "{}", lit),
        }
    }
}

impl<Lit: Literal> Expression<Lit> {
    pub fn synthesize(&self, state: &mut State<Lit>) -> Result<Type<Lit>, Lit> {
        let (ty, ctx) = state.synthesize(&Context::new(), self)?;
        let ty_s = format!("{} =>", ty);
        log::debug!("{}", ty_s);
        log::debug!("{}{}", " ".repeat(ty_s.len()), format!("{}", ctx));
        let ty = ty.apply(&ctx);
        log::debug!("app: {}", ty);
        Ok(ty)
    }
}
