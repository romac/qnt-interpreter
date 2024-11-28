use color_eyre::eyre::eyre;
use color_eyre::Result;
use fxhash::{FxHashMap, FxHashSet};

use crate::ast::*;

#[derive(Default, Debug)]
pub struct Env<'a> {
    values: FxHashMap<Sym, Value>,
    parent: Option<&'a Env<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn nested(&self, values: FxHashMap<Sym, Value>) -> Env<'_> {
        Env {
            values,
            parent: Some(self),
        }
    }

    pub fn with(&mut self, name: Sym, value: Value) -> Env<'_> {
        let mut env = Env {
            values: FxHashMap::default(),
            parent: Some(self),
        };
        env.define(name, value);
        env
    }

    pub fn define(&mut self, name: Sym, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Sym) -> Option<&Value> {
        match self.values.get(name) {
            Some(value) => Some(value),
            None => self.parent.as_ref().and_then(|p| p.get(name)),
        }
    }
}

pub struct Interpreter<'a> {
    table: &'a SymbolTable,
}

impl<'a> Interpreter<'a> {
    pub fn new(table: &'a SymbolTable) -> Self {
        Self { table }
    }

    pub fn eval_in(&self, expr: &Expr, env: &mut Env) -> Result<Value> {
        match expr {
            Expr::Lit(lit) => match lit {
                Lit::Int(n) => Ok(Value::Int(*n)),
                Lit::Bool(b) => Ok(Value::Bool(*b)),
                Lit::Set(elems) => {
                    let elems = elems
                        .iter()
                        .map(|elem| self.eval_in(elem, env))
                        .collect::<Result<FxHashSet<_>>>()?;

                    Ok(Value::Set(elems))
                }
            },

            Expr::Var(var) => env
                .get(&var.sym)
                .cloned()
                .ok_or_else(|| eyre!("Undefined variable: {}", var.sym)),

            Expr::Let(sym, value, body) => {
                let value = self.eval_in(value, env)?;
                self.eval_in(body, &mut env.with(sym.sym, value))
            }

            Expr::Block(exprs) => {
                let mut result = Value::Undefined;
                for expr in exprs {
                    result = self.eval_in(expr, env)?;
                }
                Ok(result)
            }

            Expr::BinOp(op, left, right) => {
                let lhs = self.eval_in(left, env)?;
                let rhs = self.eval_in(right, env)?;
                let apply = op.to_fn();

                apply(lhs, rhs)
            }

            Expr::Call(sym, args) => {
                let def = self
                    .table
                    .defs
                    .get(sym)
                    .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

                if args.len() != def.args.len() {
                    return Err(eyre!(
                        "Wrong number of arguments for {sym}: found {}, expected {}",
                        args.len(),
                        def.args.len()
                    ));
                }

                // Evaluate arguments and bind them to parameters
                let mut values = FxHashMap::default();
                for (param, arg) in def.args.iter().zip(args) {
                    let value = self.eval_in(arg, env)?;
                    values.insert(param.sym, value);
                }

                // Evaluate function body in new environment
                self.eval_in(&def.body, &mut env.nested(values))
            }

            Expr::If(cnd, thn, els) => {
                let cnd = self.eval_in(cnd, env)?;
                match cnd {
                    Value::Bool(true) => self.eval_in(thn, env),
                    Value::Bool(false) => self.eval_in(els, env),
                    _ => Err(eyre!("Condition must evaluate to a boolean")),
                }
            }

            Expr::While(cond, body) => {
                let mut result = Value::Undefined;
                while self.eval_in(cond, env)?.as_bool()? {
                    result = self.eval_in(body, env)?;
                }
                Ok(result)
            }

            Expr::SetAdd(set, elem) => {
                let mut set = self.eval_in(set, env)?.to_set()?;
                set.insert(self.eval_in(elem, env)?);
                Ok(Value::Set(set))
            }

            Expr::SetContains(set, elem) => {
                let set = self.eval_in(set, env)?.to_set()?;
                let contains = set.contains(&self.eval_in(elem, env)?);
                Ok(Value::Bool(contains))
            }
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value> {
        self.eval_in(expr, &mut Env::new())
    }
}

pub fn run(syms: &SymbolTable, main_sym: Sym) -> Result<i64> {
    let interpreter = Interpreter::new(syms);
    let expr = Expr::Call(main_sym, vec![]);

    match interpreter.eval(&expr)? {
        Value::Int(result) => Ok(result),
        _ => Err(eyre!("Expected integer result")),
    }
}
