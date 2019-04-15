#![allow(unused_variables, dead_code)]
use linefeed::{Interface, ReadResult};

#[macro_use]
mod util;
use util::*;

use transaction::Transaction;

mod history;
use history::{Error as HistoryError, History};

mod state;
use state::{Error as StateError, State};

mod command;
use command::{Command, CommandRange, Error as CommandError};

pub trait PrintError {
    fn print(&self);
}

#[derive(Debug)]
pub enum Error {
    CommandError(CommandError),
    HistoryError(HistoryError),
    StateError(StateError),
}

impl PrintError for Error {
    fn print(&self) {
        match self {
            Error::CommandError(err) => err.print(),
            Error::HistoryError(err) => err.print(),
            Error::StateError(err) => err.print(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Next {
    Nothing,
    Reset,
    Exit,
}

type ReplFile = diag::CurrentFile;

struct Repl;
impl Repl {
    fn handle(cmd: Command, state: &mut State) -> Result<Next, Error> {
        match &cmd {
            Command::Exit => state.exit(),
            Command::Reset => {
                state.reset();
                return Ok(Next::Reset);
            }
            Command::Help => eprintln!("can't help yet"),

            Command::History(CommandRange::From(index)) => {
                let history = state.history();
                history
                    .show_range(*index..=history.len().saturating_sub(1))
                    .map_err(Error::HistoryError)?;
            }

            Command::History(CommandRange::Index(index)) => {
                state
                    .history()
                    .show_index(*index)
                    .map_err(Error::HistoryError)?;
            }

            Command::History(CommandRange::Range(range)) => {
                state
                    .history()
                    .show_range(range.clone())
                    .map_err(Error::HistoryError)?;
            }

            Command::History(..) => {
                state.history().show_all().map_err(Error::HistoryError)?;
            }

            Command::Zap(CommandRange::From(index)) => {
                let history = state.history_mut();
                history
                    .zap_range(*index..=history.len().saturating_sub(1))
                    .map_err(Error::HistoryError)?;
            }

            Command::Zap(CommandRange::Index(index)) => {
                state
                    .history_mut()
                    .zap_index(*index)
                    .map_err(Error::HistoryError)?;
            }

            Command::Zap(CommandRange::Range(range)) => {
                state
                    .history_mut()
                    .zap_range(range.clone())
                    .map_err(Error::HistoryError)?;
            }

            Command::Zap(..) => {
                print_error!("to clear the full history, consider :reset or :zap 0..");
            }

            Command::Recall(index) => {
                state.recall(*index).map_err(Error::HistoryError)?;
            }

            Command::Info(index) => state.info(*index),
            Command::Type(input) => state.lookup(input),
        }

        Ok(Next::Nothing)
    }

    fn read(
        entry: u64,
        state: &mut State,
        interface: &mut Interface<linefeed::DefaultTerminal>,
    ) -> Result<Next, Error> {
        let left = count_digits(entry);
        interface.set_prompt(&format!("{}> ", entry)).unwrap();

        if let Some(recall) = state.buffer() {
            interface.set_buffer(&recall).unwrap();
        }

        let mut buffer = String::new();

        loop {
            let data = match interface.read_line().unwrap() {
                ReadResult::Input(data) => data,
                ReadResult::Eof => return Ok(Next::Exit),
                ReadResult::Signal(..) => return Ok(Next::Reset),
            };

            let temp = data.trim();
            if temp.is_empty() {
                let data = buffer.trim_end();
                if !data.is_empty() {
                    state.try_input(&data).map_err(Error::StateError)?;
                    return Ok(Next::Nothing);
                }
                return Ok(Next::Reset);
            }

            if temp.starts_with(':') {
                let cmd = Command::lookup(&temp[1..]).map_err(Error::CommandError)?;
                return match Repl::handle(cmd, state)? {
                    ok @ Next::Reset => Ok(ok),
                    ok => {
                        state.save_input(&temp);
                        Ok(ok)
                    }
                };
            }
            buffer.push_str(&data);
            buffer.push_str(" \n");

            interface
                .set_prompt(&format!("{}~ ", " ".repeat(left)))
                .unwrap();
        }
    }
}

fn main() {
    let mut interface = Interface::new("harvey-repl").unwrap();

    let mut state = State::default();
    'outer: loop {
        let entry = state.index();
        loop {
            match Repl::read(entry as u64, &mut state, &mut interface) {
                Err(err) => {
                    err.print();
                    continue;
                }
                Ok(Next::Exit) => state.exit(),
                Ok(Next::Reset) => continue 'outer,
                Ok(Next::Nothing) => break,
            }
        }
        state.next_index();
    }
}
