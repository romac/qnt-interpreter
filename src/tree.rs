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

    pub fn with_parent(parent: &'a Env, values: FxHashMap<Sym, Value>) -> Self {
        Env {
            values,
            parent: Some(parent),
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

    pub fn _eval(&self, expr: &Expr, env: &mut Env) -> Result<Value> {
        match expr {
            Expr::Lit(lit) => Ok(match lit {
                Lit::Int(n) => Value::Int(*n),
                Lit::Bool(b) => Value::Bool(*b),
            }),

            Expr::Var(var) => env
                .get(&var.sym)
                .cloned()
                .ok_or_else(|| eyre!("Undefined variable: {}", var.sym)),

            Expr::BinOp(op, left, right) => {
                let lhs = self._eval(left, env)?;
                let rhs = self._eval(right, env)?;

                match (op, lhs, rhs) {
                    (BinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (BinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (BinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (BinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (BinOp::Eq, a, b) => Ok(Value::Bool(a == b)),
                    _ => Err(eyre!("Invalid operation {op:?}")),
                }
            }

            Expr::Call(sym, args) => {
                let def = self
                    .table
                    .defs
                    .get(sym)
                    .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

                if args.len() != def.args.len() {
                    return Err(eyre!("Wrong number of arguments for {sym}"));
                }

                let mut values = FxHashMap::default();

                // Evaluate arguments and bind them to parameters
                for (param, arg) in def.args.iter().zip(args) {
                    let value = self._eval(arg, env)?;
                    values.insert(*param, value);
                }

                let mut new_env = Env::with_parent(env, values);

                // Evaluate function body in new environment
                self._eval(&def.body, &mut new_env)
            }

            Expr::If(cond, then_expr, else_expr) => {
                let cond_val = self._eval(cond, env)?;
                match cond_val {
                    Value::Bool(true) => self._eval(then_expr, env),
                    Value::Bool(false) => self._eval(else_expr, env),
                    _ => Err(eyre!("Condition must evaluate to a boolean")),
                }
            }

            Expr::While(cond, body) => {
                let mut result = Value::Int(0);
                while self._eval(cond, env)?.as_bool() {
                    result = self._eval(body, env)?;
                }
                Ok(result)
            }
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value> {
        self._eval(expr, &mut Env::new())
    }
}
