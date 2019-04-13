#[inline]
pub fn count_digits(d: u64) -> usize {
    let (mut len, mut m) = (1, 1u64);
    while len < 20 {
        m *= 10;
        if m > d {
            return len;
        }
        len += 1;
    }
    len
}

#[macro_export]
macro_rules! print_error {
    ($e:expr) => {{
        use colored::Colorize;
        eprintln!("{}{}", "error: ".red().bold(), $e);
    }};
    ($e:expr, $f:expr, $($args:expr),* $(,)?) => {{
        use colored::Colorize;
        eprint!("{}", "error: ".red().bold());
        eprint!("{}: `", $e);
        eprint!("{}", format_args!($f, $($args),*).to_string().yellow());
        eprintln!("`");
    }};
}
