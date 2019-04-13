#![allow(clippy::many_single_char_names)]
use super::{Expression, Literal, RcString, Type};
use std::fmt::Display;

#[derive(Debug)]
pub enum Error<Lit: Literal> {
    InvalidCheck(Expression<Lit>, Type<Lit>),
    CannotSynthesize(SynthError<Lit>),
    InvalidVarExpr(String, Expression<Lit>),
    InvalidAnno(Expression<Lit>, Type<Lit>),
    CannotApplySubtype(Expression<Lit>, Type<Lit>),
    CannotSubtype(Type<Lit>, Type<Lit>),
    InvalidVarSubtype(String, String, Type<Lit>, Type<Lit>),
    InvalidExVarSubtype(String, String, Type<Lit>, Type<Lit>),
    CircularTypes(String, Type<Lit>, Type<Lit>),
    InvalidInstantiate(String, Type<Lit>),
    CannotInstantiate(InstantiateError<Lit>),
}

impl<Lit: Literal> Display for Error<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            InvalidCheck(expr, ty) => write!(f, "invalid typecheck: {} : {}", expr, ty),
            CannotSynthesize(expr) => write!(f, "invalid synthesis: {}", expr),
            InvalidVarExpr(var, expr) => write!(f, "invalid var expression: {} : {}", var, expr),
            InvalidAnno(expr, ty) => write!(f, "invalid annotation: {} : {}", expr, ty),
            CannotApplySubtype(expr, ty) => write!(f, "cannot apply subtype: {} : {}", expr, ty),
            CannotSubtype(lhs, rhs) => write!(f, "cannot subtype {} <: {}", lhs, rhs),
            InvalidVarSubtype(x, y, l, r) => {
                write!(f, "invalid var subtype: {} : {} <: {} : {}", x, l, y, r)
            }
            InvalidExVarSubtype(x, y, l, r) => {
                write!(f, "invalid exvar subtype: {} : {} <: {} : {}", x, l, y, r)
            }
            CircularTypes(var, l, r) => write!(f, "circular type: {} : {} <-> {}", var, l, r),
            InvalidInstantiate(var, ty) => write!(f, "cannot instantiate {} : {}", var, ty),
            CannotInstantiate(err) => write!(f, "{}", err),
        }
    }
}

impl<Lit: Literal> std::error::Error for Error<Lit> {}

#[derive(Debug)]
pub enum InstantiateError<Lit: Literal> {
    Existential(String, Type<Lit>, String),
    Quantification(String, Type<Lit>, String, Type<Lit>),
    Function(String, Type<Lit>, Type<Lit>, Type<Lit>),
    Product(String, Type<Lit>, Type<Lit>, Type<Lit>),
}

impl<Lit: Literal> InstantiateError<Lit> {
    pub(super) fn product(s: RcString, ty: &Type<Lit>, fst: &Type<Lit>, snd: &Type<Lit>) -> Self {
        InstantiateError::Product(s.extract(), ty.clone(), fst.clone(), snd.clone())
    }

    pub(super) fn quantification(
        s: RcString,
        ty: &Type<Lit>,
        beta: &RcString,
        b: &Type<Lit>,
    ) -> Self {
        InstantiateError::Quantification(s.extract(), ty.clone(), beta.extract(), b.clone())
    }

    pub(super) fn function(
        alpha: RcString,
        ty: &Type<Lit>,
        a1: &Type<Lit>,
        a2: &Type<Lit>,
    ) -> Self {
        InstantiateError::Function(alpha.extract(), ty.clone(), a1.clone(), a2.clone())
    }

    pub(super) fn existential(alpha: RcString, ty: &Type<Lit>, beta: &RcString) -> Self {
        InstantiateError::Existential(alpha.extract(), ty.clone(), beta.extract())
    }
}

impl<Lit: Literal> Display for InstantiateError<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstantiateError::*;
        match self {
            Existential(alpha, ty, name) => write!(
                f,
                "cannot instantiate esistential: {} : {} -> `{}`",
                alpha, ty, name
            ),
            Quantification(alpha, ty, name, beta) => write!(
                f,
                "cannot instantiate quantification: {} : {} -> `{} -> {}`",
                alpha, ty, name, beta
            ),
            Function(alpha, ty, left, right) => write!(
                f,
                "cannot instantiate function: {} : {} -> `{} : {}`",
                alpha, ty, left, right
            ),
            Product(alpha, ty, fst, snd) => write!(
                f,
                "cannot instantiate product: {} : {} -> `({}, {})`",
                alpha, ty, fst, snd
            ),
        }
    }
}

impl<Lit: Literal> std::error::Error for InstantiateError<Lit> {}

#[derive(Debug)]
pub enum SynthError<Lit: Literal> {
    Decl(String, Expression<Lit>),
    Let(String, Expression<Lit>, Expression<Lit>),
}

impl<Lit: Literal> Display for SynthError<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SynthError::Decl(binding, expr) => {
                write!(f, "cannot syn decl: `{} : {}`", binding, expr)
            }
            SynthError::Let(name, expr, body) => {
                write!(f, "cannot synth `let {} : {} = {}`", name, expr, body)
            }
        }
    }
}

impl<Lit: Literal> std::error::Error for SynthError<Lit> {}

impl<Lit: Literal> From<SynthError<Lit>> for Error<Lit> {
    fn from(err: SynthError<Lit>) -> Self {
        Error::CannotSynthesize(err)
    }
}

impl<Lit: Literal> From<InstantiateError<Lit>> for Error<Lit> {
    fn from(err: InstantiateError<Lit>) -> Self {
        Error::CannotInstantiate(err)
    }
}
