use fxhash::FxHashMap;

use crate::{ast::*, fib};

#[derive(Debug)]
pub struct Env<'a> {
    values: FxHashMap<Sym, Value>,
    parent: Option<&'a Env<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self {
            values: FxHashMap::default(),
            parent: None,
        }
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

pub struct Interpreter {
    table: SymbolTable,
}

impl Interpreter {
    pub fn new(table: SymbolTable) -> Self {
        Interpreter { table }
    }

    pub fn eval(&self, expr: &Expr, env: &mut Env) -> Result<Value, String> {
        match expr {
            Expr::Lit(lit) => Ok(match lit {
                Lit::Int(n) => Value::Int(*n),
                Lit::Bool(b) => Value::Bool(*b),
            }),

            Expr::Var(var) => env
                .get(&var.sym)
                .cloned()
                .ok_or_else(|| format!("Undefined variable: {}", var.sym)),

            Expr::BinOp(op, left, right) => {
                let lhs = self.eval(left, env)?;
                let rhs = self.eval(right, env)?;

                match (op, lhs, rhs) {
                    (BinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (BinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (BinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (BinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (BinOp::Eq, a, b) => Ok(Value::Bool(a == b)),
                    _ => Err(format!("Invalid operation {op:?}")),
                }
            }

            Expr::Call(sym, args) => {
                let def = self
                    .table
                    .defs
                    .get(sym)
                    .ok_or_else(|| format!("Undefined function: {sym}"))?;

                if args.len() != def.args.len() {
                    return Err(format!("Wrong number of arguments for {sym}"));
                }

                let mut values = FxHashMap::default();

                // Evaluate arguments and bind them to parameters
                for (param, arg) in def.args.iter().zip(args) {
                    let value = self.eval(arg, env)?;
                    values.insert(*param, value);
                }

                let mut new_env = Env::with_parent(env, values);

                // Evaluate function body in new environment
                self.eval(&def.body, &mut new_env)
            }

            Expr::If(cond, then_expr, else_expr) => {
                let cond_val = self.eval(cond, env)?;
                match cond_val {
                    Value::Bool(true) => self.eval(then_expr, env),
                    Value::Bool(false) => self.eval(else_expr, env),
                    _ => Err("Condition must evaluate to a boolean".to_string()),
                }
            }
        }
    }
}

pub fn eval() -> Result<(), String> {
    let fib = fib();
    let sym = fib.sym;

    let mut symbol_table = SymbolTable::default();
    symbol_table.defs.insert(sym, fib);

    let interpreter = Interpreter::new(symbol_table);
    let mut env = Env::new();

    // Test fibonacci numbers 1 through 20
    for n in 1..=27 {
        println!("Calculating fib({n})...");
        let expr = Expr::Call(sym, vec![Expr::Lit(Lit::Int(n))]);

        match interpreter.eval(&expr, &mut env) {
            Ok(Value::Int(result)) => println!("fib({n}) = {result}"),
            Ok(other) => println!("Unexpected result type: {other:?}"),
            Err(e) => println!("Error: {e}"),
        }
    }

    Ok(())
}
