use std::cell::RefCell;
use std::fmt;
use std::hash::{Hash, Hasher};

use fxhash::FxHashSet;

thread_local! {
    static INTERNER: RefCell<Interner> = RefCell::new(Interner::new());
}

#[derive(Copy, Clone, Debug)]
pub struct Str(&'static str);

impl Str {
    pub fn new(s: String) -> Self {
        INTERNER.with(|interner| interner.borrow_mut().intern(s))
    }

    fn ptr(&self) -> *const u8 {
        self.0.as_ptr()
    }
}

impl From<&'static str> for Str {
    fn from(s: &'static str) -> Self {
        Str(s)
    }
}

impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr(), other.ptr())
    }
}

impl Eq for Str {}

impl Hash for Str {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr().hash(state);
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Default)]
struct Interner {
    set: FxHashSet<&'static str>,
}

impl Interner {
    fn new() -> Self {
        Self::default()
    }

    fn intern(&mut self, s: String) -> Str {
        if let Some(ptr) = self.set.get(s.as_str()) {
            return Str(ptr);
        }

        let value = Box::leak(s.into_boxed_str());
        self.set.insert(value);
        Str(value)
    }
}
