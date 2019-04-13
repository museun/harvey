use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

thread_local!(
    pub static FILE_MAP: RefCell<HashMap<usize, Rc<String>>> = RefCell::new(HashMap::new());
    static BLANK_NAME: Rc<String> = Rc::new(String::from("<input>"));
);

pub static ID_SOURCE: AtomicUsize = AtomicUsize::new(0);

pub trait SpanFile: Copy + Debug + Eq + Ord {
    fn name(&self) -> Rc<String>; // TODO not this
}

impl FileName {
    pub fn new(name: impl ToString) -> Self {
        let id = ID_SOURCE.fetch_add(1, Ordering::Relaxed);
        FILE_MAP.with(|map| map.borrow_mut().insert(id, Rc::new(name.to_string())));
        Self { id }
    }
}

impl SpanFile for FileName {
    fn name(&self) -> Rc<String> {
        FILE_MAP.with(|map| map.borrow().get(&self.id).unwrap().clone())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CurrentFile;

impl SpanFile for CurrentFile {
    fn name(&self) -> Rc<String> {
        BLANK_NAME.with(Rc::clone)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileName {
    pub id: usize,
}
