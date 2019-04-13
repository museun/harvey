use super::*;

#[derive(Debug)]
pub struct Guard<T, E>(pub T, pub E);

impl<'a, T, E> Guard<T, E>
where
    T: Syntax<'a>,
    E: Syntax<'a, Output = T::Output>,
{
    pub fn new(t: T, e: E) -> Self {
        Self(t, e)
    }
}

impl<'a, T, E> Syntax<'a> for Guard<T, E>
where
    T: Syntax<'a>,
    E: Syntax<'a, Output = T::Output>,
{
    type Output = T::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        unimplemented!()
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        unimplemented!()
    }
}
