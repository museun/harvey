use super::{Context, ContextKind, Literal, RcString};
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Type<Lit: Literal> {
    Literal(Lit::Type),
    Product(Box<Self>, Box<Self>),
    Var(RcString),
    Existential(RcString),
    Quantification(RcString, Box<Self>),
    Function(Box<Self>, Box<Self>),
}

impl<Lit: Literal> Display for Type<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Literal(lit) => write!(f, "{}", lit),
            Product(a, b) => write!(f, "{} * {}", a, b),
            Var(binding) => write!(f, "{}", binding),
            Existential(binding) => write!(f, "{}'", binding),
            Quantification(binding, ty) => write!(f, "∀{}.{}", binding, ty),
            Function(left, right) => write!(f, "{} -> {}", left, right),
        }
    }
}

impl<Lit: Literal> Type<Lit> {
    pub fn is_well_formed(&self, ctx: &Context<Lit>) -> bool {
        match self {
            Type::Literal(..) => true,
            Type::Var(var) => ctx.has_variable(var),
            Type::Existential(var) => ctx.has_existential(var) || ctx.get_solved(var).is_some(),
            Type::Quantification(alpha, a) => {
                a.is_well_formed(&ctx.cons(ContextKind::Var(alpha.clone())))
            }
            Type::Function(left, right) => left.is_well_formed(ctx) && right.is_well_formed(ctx),
            Type::Product(a, b) => a.is_well_formed(ctx) && b.is_well_formed(ctx),
        }
    }

    pub fn is_monotype(&self) -> bool {
        match self {
            Type::Quantification(..) => false,
            Type::Function(left, right) => left.is_monotype() && right.is_monotype(),
            _ => true,
        }
    }

    pub fn occurs(&self, alpha: &str) -> bool {
        match self {
            Type::Literal(..) => false,
            Type::Var(var) | Type::Existential(var) => **var == alpha,
            Type::Function(t1, t2) => t1.occurs(alpha) || t2.occurs(alpha),
            Type::Quantification(beta, ty) => {
                if alpha == **beta {
                    true
                } else {
                    ty.occurs(alpha)
                }
            }
            Type::Product(a, b) => a.occurs(alpha) || b.occurs(alpha),
        }
    }

    pub fn apply(&self, context: &Context<Lit>) -> Self {
        match self {
            Type::Literal(..) | Type::Var(..) => self.clone(),
            Type::Existential(ref alpha) => {
                if let Some(tau) = context.get_solved(&alpha) {
                    tau.apply(context)
                } else {
                    self.clone()
                }
            }
            Type::Quantification(alpha, a) => {
                Type::Quantification(alpha.clone(), a.apply(context).into())
            }
            Type::Function(l, r) => {
                Type::Function(l.apply(context).into(), r.apply(context).into())
            }
            Type::Product(a, b) => Type::Product(a.apply(context).into(), b.apply(context).into()),
        }
    }

    pub fn subsitute(&self, alpha: &str, beta: &Type<Lit>) -> Self {
        match self {
            Type::Literal(..) => self.clone(),
            Type::Var(var) | Type::Existential(var) => {
                if **var == alpha {
                    beta.clone()
                } else {
                    self.clone()
                }
            }
            Type::Quantification(var, ty) => {
                if **var == alpha {
                    Type::Quantification(var.clone(), beta.clone().into())
                } else {
                    Type::Quantification(var.clone(), ty.subsitute(alpha, beta).into())
                }
            }
            Type::Function(l, r) => Type::Function(
                l.subsitute(alpha, beta).into(),
                r.subsitute(alpha, beta).into(),
            ),
            Type::Product(a, b) => Type::Product(
                a.subsitute(alpha, beta).into(),
                b.subsitute(alpha, beta).into(),
            ),
        }
    }
}
