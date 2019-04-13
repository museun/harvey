use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct RcString(Rc<String>);

impl From<String> for RcString {
    fn from(s: String) -> Self {
        Self(Rc::new(s))
    }
}

impl From<&str> for RcString {
    fn from(s: &str) -> Self {
        Self(Rc::new(s.to_string()))
    }
}

impl RcString {
    pub fn extract(&self) -> String {
        self.0.as_ref().to_string()
    }
}

impl std::ops::Deref for RcString {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0.as_ref()
    }
}

impl Display for RcString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
