use std::rc::Rc;

use color_eyre::eyre::eyre;
use color_eyre::Result;
use fxhash::FxHashMap;

use crate::ast::*;

type Closure<'a> = Box<dyn Fn(&mut Env) -> Result<Value> + 'a>;

#[derive(Default)]
pub struct Env<'a> {
    values: FxHashMap<Sym, Value>,
    cache: FxHashMap<Sym, Rc<Closure<'a>>>,
    parent: Option<&'a Env<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self {
            values: FxHashMap::default(),
            cache: FxHashMap::default(),
            parent: None,
        }
    }

    pub fn with_parent(parent: &'a Env, values: FxHashMap<Sym, Value>) -> Self {
        Env {
            values,
            cache: FxHashMap::default(),
            parent: Some(parent),
        }
    }

    pub fn value(&self, name: &Sym) -> Option<&Value> {
        match self.values.get(name) {
            Some(value) => Some(value),
            None => self.parent.as_ref().and_then(|p| p.value(name)),
        }
    }

    pub fn cached(&self, name: &Sym) -> Option<&Closure> {
        match self.cache.get(name) {
            Some(closure) => Some(closure),
            None => self.parent.as_ref().and_then(|p| p.cached(name)),
        }
    }
}

pub struct Interpreter<'a> {
    symbols: &'a SymbolTable,
}

impl<'a> Interpreter<'a> {
    pub fn new(symbols: &'a SymbolTable) -> Self {
        Self { symbols }
    }

    pub fn compile<'e>(&self, expr: &'e Expr) -> Result<Closure<'e>>
    where
        'a: 'e,
    {
        match expr {
            Expr::Lit(lit) => {
                let val = match lit {
                    Lit::Int(n) => Value::Int(*n),
                    Lit::Bool(b) => Value::Bool(*b),
                };
                Ok(Box::new(move |_| Ok(val.clone())))
            }

            Expr::Var(var) => {
                let sym = var.sym;
                Ok(Box::new(move |env| {
                    env.value(&sym)
                        .cloned()
                        .ok_or_else(|| eyre!("Undefined variable: {sym}"))
                }))
            }

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile(lhs)?;
                let rhs = self.compile(rhs)?;

                let apply = match op {
                    BinOp::Add => |a: Value, b: Value| Value::Int(a.as_int() + b.as_int()),
                    BinOp::Sub => |a: Value, b: Value| Value::Int(a.as_int() - b.as_int()),
                    BinOp::Mul => |a: Value, b: Value| Value::Int(a.as_int() * b.as_int()),
                    BinOp::Lt => |a: Value, b: Value| Value::Bool(a.as_int() < b.as_int()),
                    BinOp::Eq => |a: Value, b: Value| Value::Bool(a == b),
                };

                Ok(Box::new(move |env| {
                    let l = lhs(env)?;
                    let r = rhs(env)?;
                    Ok(apply(l, r))
                }))
            }

            Expr::If(cnd, thn, els) => {
                let cnd = self.compile(cnd)?;
                let thn = self.compile(thn)?;
                let els = self.compile(els)?;

                Ok(Box::new(move |env| match cnd(env)? {
                    Value::Bool(true) => thn(env),
                    Value::Bool(false) => els(env),
                    _ => Err(eyre!("Condition must be boolean")),
                }))
            }

            Expr::While(cond, body) => {
                let cond = self.compile(cond)?;
                let body = self.compile(body)?;

                Ok(Box::new(move |env| {
                    let mut result = Value::Int(0);
                    while cond(env)?.as_bool() {
                        result = body(env)?;
                    }
                    Ok(result)
                }))
            }

            Expr::Call(sym, args) => {
                let def = self.symbols.defs.get(sym).unwrap();
                let args = args
                    .iter()
                    .map(|arg| self.compile(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(Box::new(move |env| {
                    let mut values = FxHashMap::default();
                    for (param, arg) in def.args.iter().zip(&args) {
                        values.insert(*param, arg(env)?);
                    }

                    let mut call_env = Env::with_parent(env, values);
                    let body = env.cached(sym).unwrap();
                    body(&mut call_env)
                }))
            }
        }
    }

    pub fn eval(&self, expr: &Expr, env: &mut Env) -> Result<Value> {
        let closure = self.compile(expr)?;
        closure(env)
    }
}

pub fn prepare(syms: &SymbolTable) -> Result<(Interpreter<'_>, Env<'_>)> {
    let interpreter = Interpreter::new(syms);
    let mut env = Env::new();

    for def in interpreter.symbols.defs.values() {
        let sym = def.sym;
        let closure = interpreter.compile(&def.body)?;
        env.cache.insert(sym, Rc::new(closure));
    }

    Ok((interpreter, env))
}
