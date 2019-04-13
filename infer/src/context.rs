use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use super::{Literal, RcString, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum ContextKind<Lit: Literal> {
    Var(RcString),
    Existential(RcString),
    Solved(RcString, Type<Lit>),
    Marker(RcString),
    Typed(RcString, Type<Lit>),
}

impl<Lit: Literal> Display for ContextKind<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ContextKind::*;
        match self {
            Var(binding) => write!(f, "{}", binding),
            Existential(binding) => write!(f, "{}'", binding),
            Solved(binding, ty) => write!(f, "{}': {}", binding, ty),
            Marker(marker) => write!(f, "◮{}", marker),
            Typed(binding, ty) => write!(f, "{}: {}", binding, ty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context<Lit: Literal>(Rc<RefCell<Vec<ContextKind<Lit>>>>);

impl<Lit: Literal> Default for Context<Lit> {
    fn default() -> Self {
        Self(Rc::new(RefCell::new(vec![])))
    }
}

impl<Lit: Literal> Context<Lit> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<Lit: Literal> Display for Context<Lit> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;
        self.0.borrow().iter().fold(true, |head, item| {
            if !head {
                write!(f, ", ").unwrap();
            }
            write!(f, "{}", item).unwrap();
            false
        });
        write!(f, " ]")
    }
}

impl<Lit: Literal> Context<Lit> {
    pub fn cons(&self, kind: ContextKind<Lit>) -> Self {
        let list = self.0.clone();
        list.borrow_mut().push(kind);
        Self(list)
    }

    pub fn insert(&self, kind: ContextKind<Lit>, elements: Vec<ContextKind<Lit>>) -> Option<Self> {
        let index = self.index_of(&kind)?;
        let list = self.0.clone();
        list.borrow_mut()
            .splice(index..=index, elements)
            .for_each(|_| ());
        Some(Self(list))
    }

    pub fn split_at(&self, kind: ContextKind<Lit>) -> Option<(Self, Self)> {
        let index = self.index_of(&kind)?;
        let list = self.0.borrow();
        let (l, r) = list.split_at(index);
        let (l, r) = (
            Rc::new(RefCell::new(l.to_vec())),
            Rc::new(RefCell::new(r.to_vec())),
        );
        Some((Self(l), Self(r)))
    }

    pub fn drop(&self, kind: ContextKind<Lit>) -> Option<Self> {
        let index = self.index_of(&kind)?;
        let list = self.0.clone();
        list.borrow_mut().split_off(index);
        Some(Self(list))
    }

    pub fn get_solved(&self, a: &str) -> Option<Type<Lit>> {
        let list = self.0.borrow();
        let mut iter = list.iter().filter_map(|k| match k {
            ContextKind::Solved(alpha, tau) if **alpha == a => Some(tau),
            _ => None,
        });
        iter.next().cloned()
    }

    pub fn get_typed(&self, a: &str) -> Option<Type<Lit>> {
        let list = self.0.borrow();
        let mut iter = list.iter().filter_map(|k| match k {
            ContextKind::Typed(alpha, ty) if **alpha == a => Some(ty),
            _ => None,
        });
        iter.next().cloned()
    }

    pub fn has_existential(&self, a: &str) -> bool {
        self.0.borrow().iter().any(|k| match k {
            ContextKind::Existential(alpha) if **alpha == a => true,
            _ => false,
        })
    }

    pub fn has_variable(&self, a: &str) -> bool {
        self.0.borrow().iter().any(|k| match k {
            ContextKind::Var(alpha) if **alpha == a => true,
            _ => false,
        })
    }

    pub fn index_of(&self, kind: &ContextKind<Lit>) -> Option<usize> {
        self.0.borrow().iter().position(|k| k == kind)
    }
}
