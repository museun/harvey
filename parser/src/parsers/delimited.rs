use super::*;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Delimited<T, D> {
    pub element: T,
    pub delim: D,
    optional: bool,
}

impl<'a, T, D> Delimited<T, D> {
    pub fn new(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: false,
        }
    }
    pub fn optional(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: true,
        }
    }
}

impl<'a, T, D> Syntax<'a> for Delimited<T, D>
where
    T: Syntax<'a>,
    D: Syntax<'a>,
{
    type Output = T::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut self.element)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let Self {
            element,
            delim,
            optional,
        } = self;
        let el = parser.expect(element)?;
        if *optional {
            if !parser.test(delim) {
                return Ok(el);
            } else {
                parser.shift();
            }
        }
        parser.expect(delim).map(|_| el)
    }
}
