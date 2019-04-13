use std::fmt::{Debug, Display};

pub trait Literal: Display + Debug + Clone + PartialEq {
    type Type: Display + Debug + Clone + PartialEq;
    fn synthesize(&self) -> Self::Type;
    fn check(&self, ty: &Self::Type) -> bool;
}
