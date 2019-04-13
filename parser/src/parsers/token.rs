use super::*;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Token<T>(pub T);

impl<T> Token<T>
where
    T: Clone,
{
    pub fn new(token: T) -> Self {
        Self(token)
    }
}

impl<'a, T> Syntax<'a> for Token<T>
where
    T: Debug,
{
    type Output = T;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        unimplemented!()
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        unimplemented!()
    }
}
