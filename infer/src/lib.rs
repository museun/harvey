pub type Result<T, L> = std::result::Result<T, Error<L>>;

pub mod error;
pub mod string;

mod context;
mod expression;
mod literal;
mod state;
mod tracer;
mod ty;

pub use expression::Expression;
pub use literal::Literal;
pub use state::State;
pub use ty::Type;

use context::{Context, ContextKind};
use error::*;
use string::RcString;
use tracer::Tracer;

#[inline]
pub fn var<Lit: Literal>(name: impl Into<RcString>) -> Expression<Lit> {
    Expression::Var(name.into())
}

#[inline]
pub fn decl<Lit: Literal>(name: impl Into<RcString>, expr: Expression<Lit>) -> Expression<Lit> {
    Expression::Decl(name.into(), Box::new(expr))
}

#[inline]
pub fn apply<Lit: Literal>(left: Expression<Lit>, expr: Expression<Lit>) -> Expression<Lit> {
    Expression::Apply(Box::new(left), Box::new(expr))
}

#[inline]
pub fn anno<Lit: Literal>(expr: Expression<Lit>, ty: Type<Lit>) -> Expression<Lit> {
    Expression::Anno(Box::new(expr), ty)
}

#[inline]
pub fn let_<Lit: Literal>(
    name: impl Into<RcString>,
    expr: Expression<Lit>,
    body: Expression<Lit>,
) -> Expression<Lit> {
    Expression::Let(name.into(), Box::new(expr), Box::new(body))
}

#[inline]
pub fn tuple<Lit: Literal>(fst: Expression<Lit>, snd: Expression<Lit>) -> Expression<Lit> {
    Expression::Tuple(Box::new(fst), Box::new(snd))
}

#[inline]
pub fn literal<Lit: Literal>(literal: impl Into<Lit>) -> Expression<Lit> {
    Expression::Literal(literal.into())
}

pub fn synthesize<Lit: Literal>(expr: Expression<Lit>) -> Result<Type<Lit>, Lit> {
    expr.synthesize(&mut State::new())
}

#[cfg(test)]
mod tests;
