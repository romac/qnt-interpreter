use color_eyre::eyre::eyre;
use color_eyre::Result;
use fxhash::FxHashMap;

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
            Expr::Lit(lit) => Ok(lit.to_value()),

            Expr::Var(var) => env
                .get(&var.sym)
                .cloned()
                .ok_or_else(|| eyre!("Undefined variable: {}", var.sym)),

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
                    values.insert(*param, value);
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
                let mut result = Value::Int(0);
                while self.eval_in(cond, env)?.as_bool()? {
                    result = self.eval_in(body, env)?;
                }
                Ok(result)
            }
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value> {
        self.eval_in(expr, &mut Env::new())
    }
}
