use std::fmt;
use std::hash::{Hash, Hasher};

use color_eyre::eyre::bail;
use color_eyre::Result;
use fxhash::{FxHashMap, FxHashSet};

pub use crate::str::Str;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sym {
    pub id: u32,
    pub name: Str,
}

impl Sym {
    pub fn new(id: u32, name: Str) -> Self {
        Self { id, name }
    }

    pub fn typed(self, ty: Type) -> TypedSym {
        TypedSym { sym: self, ty }
    }
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}${}", self.name, self.id)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedSym {
    pub sym: Sym,
    pub ty: Type,
}

impl TypedSym {
    pub fn new(sym: Sym, ty: Type) -> Self {
        Self { sym, ty }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    pub defs: FxHashMap<Sym, Def>,
    pub syms: Vec<Sym>,
}

impl SymbolTable {
    pub fn define(&mut self, def: Def) {
        self.syms.push(def.sym);
        self.defs.insert(def.sym, def);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Def {
    pub sym: Sym,
    pub args: Vec<TypedSym>,
    pub body: Expr,
    pub return_type: Type,
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
    Let(TypedSym, Box<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Call(Sym, Vec<Expr>),

    // Control flow
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),

    // Set ops
    SetAdd(Box<Expr>, Box<Expr>),
    SetContains(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    Set(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Undefined,
    Int(i64),
    Bool(bool),
    Set(FxHashSet<Value>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let discr = core::mem::discriminant(self);
        discr.hash(state);

        match self {
            Value::Undefined => (),
            Value::Int(n) => n.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Set(set) => {
                for elem in set {
                    elem.hash(state);
                }
            }
        }
    }
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

    pub fn as_set(&self) -> Result<&FxHashSet<Value>> {
        match self {
            Value::Set(set) => Ok(set),
            _ => bail!("Expected set"),
        }
    }

    pub fn to_set(self) -> Result<FxHashSet<Value>> {
        match self {
            Value::Set(set) => Ok(set),
            _ => bail!("Expected set"),
        }
    }
}
