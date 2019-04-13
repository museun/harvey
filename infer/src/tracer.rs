use std::fmt::Display;

#[derive(Default)]
pub struct Tracer {
    buf: String,
    head: Option<()>,
}

impl Tracer {
    pub fn enter(mut self, func: &str) -> Self {
        if self.head.is_some() {
            self.buf.push_str(" ");
        }
        self.buf.push_str(func);
        self.buf.push_str("=>");
        self.print()
    }

    pub fn display<D: Display>(mut self, display: D) -> Self {
        if self.head.is_some() {
            self.buf.push_str(" ");
        }
        self.rule(&display.to_string())
    }

    pub fn alpha(mut self, alpha: &str) -> Self {
        if self.head.is_some() {
            self.buf.push_str(" ");
        }
        self.rule(alpha)
    }

    pub fn rule(mut self, rule: &str) -> Self {
        self.buf.push_str(&format!("{:>40}", rule));
        self.print()
    }

    pub fn print(mut self) -> Self {
        if !self.buf.is_empty() {
            log::trace!("{}", self.buf);
        }
        self.buf.clear();
        self.head.take();
        self
    }
}

impl Drop for Tracer {
    fn drop(&mut self) {
        if !self.buf.is_empty() {
            log::trace!("{}", self.buf)
        }
    }
}
