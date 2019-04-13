use super::*;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Tokens<T>(pub Vec<T>);

impl<T> Tokens<T>
where
    T: Clone,
{
    pub fn new(tokens: &[T]) -> Self {
        Self(tokens.to_vec())
    }
}

impl<'a, T> Syntax<'a> for Tokens<T>
where
    T: Debug,
{
    type Output = Vec<T>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        unimplemented!()
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        unimplemented!()
    }
}
