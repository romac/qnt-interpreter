use std::fmt;

use fxhash::FxHashMap;

pub use crate::str::Str;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sym {
    pub id: u32,
    pub name: Str,
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}${}", self.name, self.id)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Var {
    pub sym: Sym,
}

impl Var {
    pub fn new(sym: Sym) -> Self {
        Self { sym }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.sym.fmt(f)
    }
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    pub defs: FxHashMap<Sym, Def>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Def {
    pub sym: Sym,
    pub args: Vec<Sym>,
    pub body: Expr,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Var(Var),
    Lit(Lit),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Call(Sym, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Bool(bool),
}

impl Value {
    pub fn as_int(&self) -> i64 {
        match self {
            Value::Int(n) => *n,
            _ => panic!("Expected integer"),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Expected boolean"),
        }
    }
}
