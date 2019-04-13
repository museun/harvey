use super::{
    Context, ContextKind, Error, Expression, InstantiateError, Literal, RcString, SynthError,
    Tracer, Type,
};

use super::Result;

#[derive(Debug, Copy, Clone)]
pub struct State<Lit>(usize, std::marker::PhantomData<Lit>);

impl<Lit: Literal> Default for State<Lit> {
    fn default() -> Self {
        State(0, std::marker::PhantomData)
    }
}

impl<Lit: Literal> State<Lit> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn existential(&mut self) -> RcString {
        let next = self.0;
        self.0 += 1;
        format!("t{}", next).into()
    }

    pub fn check(
        &mut self,
        context: &Context<Lit>,
        expr: &Expression<Lit>,
        ty: &Type<Lit>,
    ) -> Result<Context<Lit>, Lit> {
        use ContextKind::*;
        let tracer = Tracer::default()
            .enter("check")
            .display(expr)
            .display(ty)
            .print();

        assert!(ty.is_well_formed(context));
        let ok = match (expr, ty) {
            // 1I
            (Expression::Literal(lit), Type::Literal(lit_ty)) => {
                tracer.rule("1I");
                assert!(lit.check(lit_ty));
                context.clone()
            }
            // (x : A) ∈ Γ
            // Γ ⊢ x ⇒ A ⊣ Γ
            (Expression::Decl(x, y), Type::Function(l, r)) => {
                tracer.rule("->I");
                let ty = Typed(x.clone(), *l.clone());
                let gamma = context.cons(ty);
                self.check(&gamma, y, r)?
            }
            // Γ, α ⊢ e ⇐ A ⊣ ∆, α, Θ
            // Γ ⊢ e ⇐ ∀α. A ⊣ ∆
            (_, Type::Quantification(alpha, a)) => {
                tracer.rule("∀I");
                let var = Var(alpha.clone());
                let gamma = context.cons(var.clone());
                self.check(&gamma, expr, a)?
                    .drop(var)
                    .ok_or_else(|| Error::InvalidCheck(expr.clone(), ty.clone()))?
            }
            // xI
            (Expression::Tuple(fst, snd), Type::Product(a, b)) => {
                tracer.rule("xI");
                let gamma = self.check(context, fst, a)?;
                self.check(&gamma, snd, b)?
            }
            // Γ ⊢ e ⇒ A ⊣ Θ Θ ⊢ [Θ]A <: [Θ]B ⊣ ∆
            // Γ ⊢ e ⇐ B ⊣ ∆
            (_, _) => {
                tracer.rule("Sub");
                let (alpha, theta) = self.synthesize(context, expr)?;
                self.subtype(&theta, &alpha.apply(&theta), &ty.apply(&theta))?
            }
        };
        Ok(ok)
    }

    pub fn synthesize(
        &mut self,
        context: &Context<Lit>,
        expr: &Expression<Lit>,
    ) -> Result<(Type<Lit>, Context<Lit>), Lit> {
        use ContextKind::*;
        let tracer = Tracer::default().enter("synthesize").display(expr).print();
        match expr {
            // Γ ⊢ () ⇐ 1 ⊣ Γ
            Expression::Literal(lit) => {
                tracer.rule("1I=>");
                Ok((Type::Literal(lit.synthesize()), context.clone()))
            }
            // (x : A) ∈ Γ
            // Γ ⊢ x ⇒ A ⊣ Γ
            Expression::Var(x) => {
                tracer.rule("Var");

                context
                    .get_typed(x)
                    .ok_or_else(|| Error::InvalidVarExpr(x.extract(), expr.clone()))
                    .map(|anno| (anno, context.clone()))
            }
            // Γ ⊢ A Γ ⊢ e ⇐ A ⊣ ∆
            // Γ ⊢ (e : A) ⇒ A ⊣ ∆
            Expression::Anno(e, anno) => {
                tracer.rule("Anno");
                if anno.is_well_formed(context) {
                    self.check(context, e, anno)
                        .map(|delta| (anno.clone(), delta))
                } else {
                    Err(Error::InvalidAnno(*e.clone(), anno.clone()))
                }
            }
            // Γ, ^α, ^β, x : ^α ⊢ e ⇐ ^β ⊣ ∆, x : ^α, Θ
            // Γ ⊢ λx. e ⇒ ^α → ^β ⊣ ∆
            Expression::Decl(x, e) => {
                tracer.rule("-I=>");
                let ext = |a: &RcString| Existential(a.clone());
                let ext_ty = |a: &RcString| Box::new(Type::Existential(a.clone()));

                let (alpha, beta) = (self.existential(), self.existential());
                let typ = Typed(x.clone(), Type::Existential(alpha.clone()));
                let gamma = context.cons(ext(&alpha)).cons(ext(&beta)).cons(typ.clone());
                let delta = self
                    .check(&gamma, e, &Type::Existential(beta.clone()))?
                    .drop(typ)
                    .ok_or_else::<Error<_>, _>(|| {
                        SynthError::Decl(x.extract(), *e.clone()).into()
                    })?;
                Ok((Type::Function(ext_ty(&alpha), ext_ty(&beta)), delta))
            }
            // Γ ⊢ e1 ⇒ A ⊣ Θ
            // Θ ⊢ [Θ]A • e2 ⇒⇒ C ⊣ ∆
            //
            // Γ ⊢ e1 e2 ⇒ C ⊣ ∆
            Expression::Apply(e1, e2) => {
                tracer.rule("->E");
                self.synthesize(context, e1).and_then(|(alpha, theta)| {
                    self.apply_synthesize(&theta, &alpha.apply(&theta), e2)
                })
            }
            Expression::Tuple(fst, snd) => {
                tracer.rule("Product");
                let (a, gamma) = self.synthesize(context, fst)?;
                let (b, delta) = self.synthesize(&gamma, snd)?;
                Ok((Type::Product(a.into(), b.into()), delta))
            }
            Expression::Let(var, exp, body) => {
                tracer.rule("Let");
                let (t0, gamma) = self.synthesize(context, exp)?;
                let theta = gamma.cons(Typed(var.clone(), t0.clone()));
                let (t1, delta) = self.synthesize(&theta, body)?;
                let delta = delta
                    .insert(Typed(var.clone(), t0), vec![])
                    .ok_or_else::<Error<_>, _>(|| {
                        SynthError::Let(var.extract(), *exp.clone(), *body.clone()).into()
                    })?;
                Ok((t1, delta))
            }
        }
    }

    pub fn apply_synthesize(
        &mut self,
        context: &Context<Lit>,
        ty: &Type<Lit>,
        expr: &Expression<Lit>,
    ) -> Result<(Type<Lit>, Context<Lit>), Lit> {
        use ContextKind::*;
        let tracer = Tracer::default()
            .enter("apply_synthesize")
            .display(expr)
            .display(ty)
            .print();

        let ok = match ty {
            // alpha apply
            // Γ[^α2, ^α1, ^α = ^α1 → ^α2] ⊢ e ⇐ ^α1 ⊣ ∆
            // Γ[^α] ⊢ ^α • e ⇒⇒ ^α2 ⊣ ∆
            Type::Existential(alpha) => {
                tracer.rule("α^App");
                let ext = |a: &RcString| Existential(a.clone());
                let ext_ty = |a: &RcString| Box::new(Type::Existential(a.clone()));

                let (alpha1, alpha2) = (self.existential(), self.existential());
                let solved = Solved(
                    alpha.clone(),
                    Type::Function(ext_ty(&alpha1), ext_ty(&alpha2)),
                );
                let gamma = context
                    .insert(ext(&alpha1), vec![ext(&alpha2), ext(&alpha1), solved])
                    .ok_or_else(|| Error::CannotApplySubtype(expr.clone(), ty.clone()))?;
                let delta = self.check(&gamma, expr, &Type::Existential(alpha1))?;
                (Type::Existential(alpha2), delta)
            }
            // forall apply
            // Γ, ^α ⊢ [^α/α]A • e ⇒⇒ C ⊣ ∆
            // Γ ⊢ ∀α. A • e ⇒⇒ C ⊣ ∆
            Type::Quantification(alpha, a) => {
                tracer.rule("∀App");
                let alpha1 = self.existential();
                let gamma = context.cons(Existential(alpha1.clone()));
                let alpha = a.subsitute(alpha, &Type::Existential(alpha1));
                self.apply_synthesize(&gamma, &alpha, expr)?
            }
            // apply
            // Γ ⊢ e ⇐ A ⊣ ∆
            // Γ ⊢ A → C • e ⇒⇒ C ⊣ ∆
            Type::Function(a, ty) => {
                tracer.rule("->App");
                (*ty.clone(), self.check(context, expr, a)?)
            }
            e => return Err(Error::CannotApplySubtype(expr.clone(), e.clone())),
        };
        Ok(ok)
    }

    pub fn subtype(
        &mut self,
        context: &Context<Lit>,
        left: &Type<Lit>,
        right: &Type<Lit>,
    ) -> Result<Context<Lit>, Lit> {
        use ContextKind::*;
        let tracer = Tracer::default()
            .enter("subtype")
            .display(format!("{} -> {}", left, right))
            .print();

        assert!(left.is_well_formed(context));
        assert!(right.is_well_formed(context));
        match (left, right) {
            // <: unit
            // Γ ⊢ 1 <: 1 ⊣ Γ
            (Type::Literal(a), Type::Literal(b)) => {
                tracer.rule("<:Unit");
                assert_eq!(a, b);
                Ok(context.clone())
            }
            // <: var
            // Γ[α] ⊢ α <: α ⊣ Γ[α]
            (Type::Var(alpha1), Type::Var(alpha2)) => {
                tracer.rule("<:Var");
                if left.is_well_formed(context) && alpha1 == alpha2 {
                    Ok(context.clone())
                } else {
                    let (alpha1, alpha2) = (alpha1.extract(), alpha2.extract());
                    let (left, right) = (left.clone(), right.clone());
                    Err(Error::InvalidVarSubtype(alpha1, alpha2, left, right))
                }
            }
            // <: ex var
            // Γ[^α] ⊢ ^α <: ^α ⊣ Γ[^α]
            (Type::Existential(exist1), Type::Existential(exist2)) if exist1 == exist2 => {
                tracer.rule("<:Exvar");
                if left.is_well_formed(context) {
                    Ok(context.clone())
                } else {
                    let (exist1, exist2) = (exist1.extract(), exist2.extract());
                    let (left, right) = (left.clone(), right.clone());
                    Err(Error::InvalidExVarSubtype(exist1, exist2, left, right))
                }
            }
            // <: ->
            // Γ ⊢ B1 <: A1 ⊣ Θ     Θ ⊢ [Θ]A2 <: [Θ]B2 ⊣ ∆
            // Γ ⊢ A1 → A2 <: B1 → B2 ⊣ ∆
            (Type::Function(a1, b1), Type::Function(a2, b2)) => {
                tracer.rule("<:->");
                self.subtype(context, a1, b1)
                    .and_then(|theta| self.subtype(&theta, &a2.apply(&theta), &b2.apply(&theta)))
            }
            // <: forall L
            // Γ, ◮^α, ^α ⊢ [^α/α]A <: B ⊣ ∆, ◮^α, Θ
            // Γ ⊢ ∀α. A <: B ⊣ ∆
            (Type::Quantification(alpha, a), _) => {
                tracer.rule("<:∀L");
                let beta = self.existential();
                let gamma = context
                    .cons(Marker(beta.clone()))
                    .cons(Existential(beta.clone()));
                let alpha = a.subsitute(alpha, &Type::Existential(beta.clone()));
                self.subtype(&gamma, &alpha, &right)?
                    .drop(Marker(beta))
                    .ok_or_else(|| Error::CannotSubtype(left.clone(), right.clone()))
            }
            // <: forall R
            // Γ, α ⊢ A <: B ⊣ ∆, α, Θ
            // Γ ⊢ A <: ∀α. B ⊣ ∆
            (_, Type::Quantification(alpha, b)) => {
                tracer.rule("<:∀R");
                let theta = context.cons(Var(alpha.clone()));
                self.subtype(&theta, left, b)?
                    .drop(Var(alpha.clone()))
                    .ok_or_else(|| Error::CannotSubtype(left.clone(), right.clone()))
            }
            // <: instantiate L
            // α^ /∈ FV(A)      Γ[^α] ⊢ ^α :=< A ⊣ ∆
            // Γ[^α] ⊢ ^α <: A ⊣ ∆
            (Type::Existential(alpha), _) => {
                tracer.rule("<:InstL");
                if !right.occurs(alpha) {
                    self.instantiate_left(context, alpha.clone(), right)
                } else {
                    Err(Error::CircularTypes(
                        alpha.extract(),
                        left.clone(),
                        right.clone(),
                    ))
                }
            }
            // <: instantiate R
            // α^ /∈ FV(A)      Γ[^α] ⊢ A =<: ^α ⊣ ∆
            // Γ[^α] ⊢ A <: ^α ⊣ ∆
            (_, Type::Existential(alpha)) => {
                tracer.rule("<:InstR");
                if !left.occurs(alpha) {
                    self.instantiate_right(context, left, alpha.clone())
                } else {
                    Err(Error::CircularTypes(
                        alpha.extract(),
                        left.clone(),
                        right.clone(),
                    ))
                }
            }
            (Type::Product(a1, b1), Type::Product(a2, b2)) => self
                .subtype(context, a1, a2)
                .and_then(|gamma| self.subtype(&gamma, b1, b2)),

            _ => Err(Error::CannotSubtype(left.clone(), right.clone())),
        }
    }

    pub fn instantiate_left(
        &mut self,
        context: &Context<Lit>,
        alpha: RcString,
        ty: &Type<Lit>,
    ) -> Result<Context<Lit>, Lit> {
        use ContextKind::*;
        let tracer = Tracer::default()
            .enter("instantiate_left")
            .alpha(alpha.as_str())
            .display(ty)
            .print();

        let ext = |a: &RcString| Existential(a.clone());
        let ext_ty = |a: &RcString| Box::new(Type::Existential(a.clone()));

        let (left, _right) = context
            .split_at(ext(&alpha))
            .ok_or_else(|| Error::InvalidInstantiate(alpha.extract(), ty.clone()))?;

        // Γ ⊢ τ
        // Γ, ^α, Γ ′ ⊢ ^α :=< τ ⊣ Γ, ^α = τ, Γ ′
        if ty.is_monotype() && ty.is_well_formed(&left) {
            tracer.rule("InstLSolve");
            return context
                .insert(ext(&alpha), vec![Solved(alpha.clone(), ty.clone())])
                .ok_or_else(|| Error::InvalidInstantiate(alpha.extract(), ty.clone()));
        }

        match ty {
            // Γ[^α][^β] ⊢ ^α :=< ^β ⊣ Γ[^α][^β = ^α]
            Type::Existential(beta) => {
                tracer.rule("InstLReach");
                let solved = Solved(beta.clone(), Type::Existential(alpha.clone()));
                context
                    .insert(ext(&beta), vec![solved])
                    .ok_or_else::<Error<_>, _>(|| {
                        InstantiateError::existential(alpha, ty, beta).into()
                    })
            }
            // Γ[^α], β ⊢ ^α :=< B ⊣ ∆, β, ∆′
            // Γ[^α] ⊢ ^α :=< ∀β. B ⊣ ∆
            Type::Quantification(beta, b) => {
                tracer.rule("InstLAllR");
                self.instantiate_left(&context.cons(Var(beta.clone())), alpha.clone(), b)?
                    .drop(Var(beta.clone()))
                    .ok_or_else::<Error<_>, _>(|| {
                        InstantiateError::quantification(alpha, ty, beta, b).into()
                    })
            }
            // Γ[^α2, ^α1, ^α = ^α1 → ^α2] ⊢ A1 =<: ^α1 ⊣ Θ
            // Θ ⊢ ^α2 :=< [Θ]A2 ⊣ ∆
            //
            // Γ[^α] ⊢ ^α :=< A1 → A2 ⊣ ∆
            Type::Function(a1, a2) => {
                tracer.rule("InstLArr");
                let (alpha1, alpha2) = (self.existential(), self.existential());
                let solved = Solved(
                    alpha.clone(),
                    Type::Function(ext_ty(&alpha1), ext_ty(&alpha2)),
                );
                let gamma = context
                    .insert(ext(&alpha), vec![ext(&alpha2), ext(&alpha1), solved])
                    .ok_or_else::<Error<_>, _>(|| {
                        InstantiateError::function(alpha, ty, a1, a2).into()
                    })?;
                self.instantiate_right(&gamma, a1, alpha1)
                    .and_then(|theta| self.instantiate_left(&theta, alpha2, &a2.apply(&theta)))
            }
            ty => Err(Error::InvalidInstantiate(alpha.extract(), ty.clone())),
        }
    }

    pub fn instantiate_right(
        &mut self,
        context: &Context<Lit>,
        ty: &Type<Lit>,
        alpha: RcString,
    ) -> Result<Context<Lit>, Lit> {
        use ContextKind::*;
        let tracer = Tracer::default()
            .enter("instantiate_right")
            .display(ty)
            .alpha(alpha.as_str())
            .print();

        let ext = |a: &RcString| Existential(a.clone());
        let ext_ty = |a: &RcString| Box::new(Type::Existential(a.clone()));

        let (left, _right) = context
            .split_at(ext(&alpha))
            .ok_or_else(|| Error::InvalidInstantiate(alpha.extract(), ty.clone()))?;

        // Γ ⊢ τ
        // Γ, ^α, Γ ′ ⊢ τ =<: ^α ⊣ Γ, ^α = τ, Γ ′
        if ty.is_monotype() && ty.is_well_formed(&left) {
            tracer.rule("InstRSolve");
            return context
                .insert(ext(&alpha), vec![Solved(alpha.clone(), ty.clone())])
                .ok_or_else(|| Error::InvalidInstantiate(alpha.extract(), ty.clone()));
        }

        match ty {
            // Γ[^α][^β] ⊢ ^β =<: ^α ⊣ Γ[^α][^β = ^α]
            Type::Existential(beta) => {
                tracer.rule("InstRReach");
                Ok(context.cons(Solved(beta.clone(), Type::Existential(alpha))))
            }
            // Γ[^α], ◮^β, ^β ⊢ [^β/β]B =<: ^α ⊣ ∆, ◮^β, ∆′
            // Γ[^α] ⊢ ∀β. B =<: ^α ⊣ ∆
            Type::Quantification(beta, b) => {
                tracer.rule("InstRAllL");
                let beta1 = self.existential();
                let gamma = context.cons(Marker(beta1.clone())).cons(ext(&beta1));
                self.instantiate_right(&gamma, &b.subsitute(beta, &ext_ty(&beta1)), alpha.clone())?
                    .drop(Marker(beta1))
                    .ok_or_else::<Error<_>, _>(|| {
                        InstantiateError::quantification(alpha, ty, beta, b).into()
                    })
            }
            // Γ[^α2, ^α1, ^α = ^α1 → ^α2] ⊢ ^α1 :=< A1 ⊣ Θ
            // Θ ⊢ [Θ]A2 =<: ^α2 ⊣ ∆
            //
            // Γ[^α] ⊢ A1 → A2 =<: ^α ⊣ ∆
            Type::Function(a1, a2) => {
                tracer.rule("InstRAArr");
                let (alpha1, alpha2) = (self.existential(), self.existential());
                let (a2_prime, a1_prime) = (ext(&alpha2), ext(&alpha1));
                let solved = Solved(alpha, Type::Function(ext_ty(&alpha1), ext_ty(&alpha2)));
                let gamma = context.cons(a2_prime).cons(a1_prime).cons(solved);
                self.instantiate_left(&gamma, alpha1, a1)
                    .and_then(|theta| self.instantiate_right(&theta, &a2.apply(&theta), alpha2))
            }
            Type::Product(a, b) => {
                tracer.rule("InstRProd");
                let (alpha1, beta1) = (self.existential(), self.existential());
                let solved = Solved(
                    alpha.clone(),
                    Type::Product(ext_ty(&alpha1), ext_ty(&beta1)),
                );
                let gamma = context
                    .insert(ext(&alpha), vec![ext(&beta1), ext(&alpha1), solved])
                    .ok_or_else::<Error<_>, _>(|| {
                        InstantiateError::product(alpha, ty, a, b).into()
                    })?;
                self.instantiate_left(&gamma, alpha1, a)
                    .and_then(|theta| self.instantiate_right(&theta, &b.apply(&theta), beta1))
            }
            ty => Err(Error::InvalidInstantiate(alpha.extract(), ty.clone())),
        }
    }
}
