use std::fmt;

use color_eyre::eyre::bail;
use color_eyre::Result;
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

impl BinOp {
    pub fn to_fn(self) -> fn(Value, Value) -> Result<Value> {
        match self {
            BinOp::Add => |a, b| Ok(Value::Int(a.as_int()? + b.as_int()?)),
            BinOp::Sub => |a, b| Ok(Value::Int(a.as_int()? - b.as_int()?)),
            BinOp::Mul => |a, b| Ok(Value::Int(a.as_int()? * b.as_int()?)),
            BinOp::Lt => |a, b| Ok(Value::Bool(a.as_int()? < b.as_int()?)),
            BinOp::Eq => |a, b| Ok(Value::Bool(a == b)),
        }
    }

    pub fn eval(&self, a: Value, b: Value) -> Result<Value> {
        self.to_fn()(a, b)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Var(Var),
    Lit(Lit),
    Let(Sym, Box<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Call(Sym, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

impl Lit {
    pub fn to_value(self) -> Value {
        match self {
            Lit::Int(n) => Value::Int(n),
            Lit::Bool(b) => Value::Bool(b),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Undefined,
    Int(i64),
    Bool(bool),
}

impl Value {
    pub fn as_int(&self) -> Result<i64> {
        match self {
            Value::Int(n) => Ok(*n),
            _ => bail!("Expected integer"),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => bail!("Expected boolean"),
        }
    }
}
