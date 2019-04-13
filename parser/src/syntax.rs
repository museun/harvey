use super::*;
use std::fmt::Debug;

pub trait Syntax<'a>: Debug {
    type Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool;
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output, ErrorReported>;
}

impl<'a, T> Syntax<'a> for &mut T
where
    T: Syntax<'a>,
{
    type Output = T::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        T::test(self, parser)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output, ErrorReported> {
        T::expect(self, parser)
    }
}
