use color_eyre::eyre::eyre;
use color_eyre::Result;
use fxhash::FxHashMap;

use crate::ast::*;

type Closure<'a> = Box<dyn Fn(&mut Env) -> Result<Value> + 'a>;

#[derive(Default)]
pub struct Env<'a> {
    values: FxHashMap<Sym, Value>,
    cache: FxHashMap<Sym, Closure<'a>>,
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

    pub fn nested(&self, values: FxHashMap<Sym, Value>) -> Env<'_> {
        Env {
            values,
            cache: FxHashMap::default(),
            parent: Some(self),
        }
    }

    pub fn with(&self, name: Sym, value: Value) -> Env<'_> {
        let mut env = Env {
            values: FxHashMap::default(),
            cache: FxHashMap::default(),
            parent: Some(self),
        };
        env.values.insert(name, value);
        env
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

            Expr::Let(sym, value, body) => {
                let value = self.compile(value)?;
                let body = self.compile(body)?;

                Ok(Box::new(move |env| {
                    let value = value(env)?;
                    body(&mut env.with(*sym, value))
                }))
            }

            Expr::Block(exprs) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.compile(expr))
                    .collect::<Result<Vec<_>>>()?;

                Ok(Box::new(move |env| {
                    let mut result = Value::Int(0);
                    for expr in &exprs {
                        result = expr(env)?;
                    }
                    Ok(result)
                }))
            }

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile(lhs)?;
                let rhs = self.compile(rhs)?;
                let apply = op.to_fn();

                Ok(Box::new(move |env| {
                    let l = lhs(env)?;
                    let r = rhs(env)?;
                    apply(l, r)
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
                    while cond(env)?.as_bool()? {
                        result = body(env)?;
                    }
                    Ok(result)
                }))
            }

            Expr::Call(sym, args) => {
                let def = self
                    .symbols
                    .defs
                    .get(sym)
                    .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

                let args = args
                    .iter()
                    .map(|arg| self.compile(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(Box::new(move |env| {
                    let mut values = FxHashMap::default();
                    for (param, arg) in def.args.iter().zip(&args) {
                        values.insert(*param, arg(env)?);
                    }

                    let body = env
                        .cached(sym)
                        .ok_or_else(|| eyre!("Cached function not found: {sym}"))?;

                    body(&mut env.nested(values))
                }))
            }
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value> {
        let closure = self.compile(expr)?;

        let mut env = Env::new();

        for def in self.symbols.defs.values() {
            let sym = def.sym;
            let closure = self.compile(&def.body)?;
            env.cache.insert(sym, closure);
        }

        closure(&mut env)
    }
}
