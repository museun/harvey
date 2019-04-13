use super::{print_error, PrintError};
use std::ops::RangeInclusive;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownCommand,
    MissingInput,
    InvalidInput(String),
}

impl PrintError for Error {
    fn print(&self) {
        match self {
            Error::InvalidInput(input) => print_error!("invalid input", "{}", input),
            Error::MissingInput => print_error!("missing input"),
            Error::UnknownCommand => print_error!("unknown command"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommandRange {
    Range(RangeInclusive<usize>),
    From(usize), // to the end
    Index(usize),
    Nothing,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    History(CommandRange),
    Zap(CommandRange),

    Recall(usize),
    Type(String),
    Info(usize),
    Reset,
    Exit,
    Help, // TODO context
}

impl Command {
    pub fn lookup(data: &str) -> Result<Command, Error> {
        use Error::*;

        let index = data.find(' ').unwrap_or_else(|| data.len());
        let (head, tail) = data.split_at(index);
        let tail = tail.trim();

        match head {
            "t" | "type" | "r" | "recall" | "i" | "info" if tail.is_empty() => Err(MissingInput),

            "r" | "recall" => tail
                .parse()
                .map(Command::Recall)
                .map_err(|_| InvalidInput(tail.to_string())),

            "t" | "type" => Ok(Command::Type(tail.to_string())),

            "i" | "info" => tail
                .parse()
                .map(Command::Info)
                .map_err(|_| InvalidInput(tail.to_string())),

            r @ "show" | r @ "history" | r @ "zap" => {
                use Command::*;
                use CommandRange::*;

                let zap = r == "zap";

                if tail.is_empty() {
                    return if !zap {
                        Ok(History(Nothing))
                    } else {
                        Ok(Zap(Nothing))
                    };
                }

                let (x, xs) = tail
                    .chars()
                    .take_while(char::is_ascii_digit)
                    .filter_map(|c| c.to_digit(10))
                    .map(|c| c as usize)
                    .fold((0, 0), |(a, l), c| (10 * a + c, l + 1));

                if xs == 0 {
                    return Err(InvalidInput(tail.to_string()));
                }

                if tail.len() > xs && tail[xs..].starts_with("..") {
                    if let Ok(d) = tail[xs + 2..].parse::<usize>() {
                        if x >= d {
                            return Err(InvalidInput(tail.to_string()));
                        }
                        return if !zap {
                            Ok(History(Range(x..=d)))
                        } else {
                            Ok(Zap(Range(x..=d)))
                        };
                    }

                    if tail[xs + 2..].chars().nth(0).is_none() {
                        return if !zap {
                            Ok(History(From(x)))
                        } else {
                            Ok(Zap(From(x)))
                        };
                    }
                    return Err(InvalidInput(tail.to_string()));
                }

                if !zap {
                    Ok(History(Index(x)))
                } else {
                    Ok(Zap(Index(x)))
                }
            }

            "h" | "help" => Ok(Command::Help),

            // these won't have abbreviations because they aren't idempotent
            "reset" => Ok(Command::Reset),
            "quit" | "exit" => Ok(Command::Exit),

            _ => Err(Error::UnknownCommand),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO more tests

    #[test]
    fn parse_history() {
        let input = "history";
        let cmd = Command::lookup(&input).unwrap();
        assert_eq!(cmd, Command::History(CommandRange::Nothing));

        let input = "history 1";
        let cmd = Command::lookup(&input).unwrap();
        assert_eq!(cmd, Command::History(CommandRange::Index(1)));

        let input = "history 100..";
        let cmd = Command::lookup(&input).unwrap();
        assert_eq!(cmd, Command::History(CommandRange::From(100)));

        let input = "history 1..3";
        let cmd = Command::lookup(&input).unwrap();
        assert_eq!(cmd, Command::History(CommandRange::Range(1..=3)));

        let input = "history 100..10";
        let err = Command::lookup(&input).unwrap_err();
        assert_eq!(err, Error::InvalidInput("100..10".to_string()));

        let input = "history 10..10";
        let err = Command::lookup(&input).unwrap_err();
        assert_eq!(err, Error::InvalidInput("10..10".to_string()));

        let input = "history 1..-3";
        let err = Command::lookup(&input).unwrap_err();
        assert_eq!(err, Error::InvalidInput("1..-3".to_string()));

        let input = "history -1..3";
        let err = Command::lookup(&input).unwrap_err();
        assert_eq!(err, Error::InvalidInput("-1..3".to_string()));

        let input = "history -1";
        let err = Command::lookup(&input).unwrap_err();
        assert_eq!(err, Error::InvalidInput("-1".to_string()));
    }
}
