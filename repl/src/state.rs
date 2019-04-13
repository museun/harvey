use super::{print_error, History, HistoryError, PrintError, Transaction};

#[derive(Debug)]
pub enum Error {}

impl PrintError for Error {
    fn print(&self) {
        print_error!("something");
    }
}

#[derive(Clone, Default)]
pub struct State {
    history: History,
    pos: usize,
    buf: Option<String>,
}

impl State {
    pub fn history(&self) -> &History {
        &self.history
    }

    pub fn history_mut(&mut self) -> &mut History {
        &mut self.history
    }

    pub fn index(&self) -> usize {
        self.pos
    }

    pub fn next_index(&mut self) {
        self.pos += 1;
    }

    pub fn buffer(&mut self) -> Option<String> {
        self.buf.take()
    }

    pub(crate) fn checkpoint(&mut self) -> Transaction<Self> {
        Transaction::new(self)
    }

    pub(crate) fn try_input(&mut self, input: &str) -> Result<(), Error> {
        let file = super::ReplFile {};
        for tok in lexer::Lexer::new(input, file) {
            eprintln!("{:?}", tok.value)
        }
        self.history.add_item(self.pos, input.to_string());
        Ok(())
    }

    pub(crate) fn save_input(&mut self, input: &str) {
        self.history.add_item(self.pos, input.to_string())
    }

    pub(crate) fn exit(&self) -> ! {
        // TODO clean up state
        std::process::exit(0);
    }

    pub(crate) fn reset(&mut self) {
        self.history.clear();
        self.buf.take();
        self.pos = 0;
    }

    pub(crate) fn recall(&mut self, index: usize) -> Result<(), HistoryError> {
        let t = self.history().get(index)?.clone();
        self.buf.replace(t);
        Ok(())
    }

    pub(crate) fn info(&mut self, _index: usize) {}

    pub(crate) fn lookup(&mut self, _data: &str) {}
}
