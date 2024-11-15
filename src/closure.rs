use std::rc::Rc;

use fxhash::FxHashMap;

use crate::ast::*;

type Closure = Box<dyn Fn(&mut Env) -> Value>;

#[derive(Default)]
pub struct Env<'a> {
    values: FxHashMap<Sym, Value>,
    cache: FxHashMap<Sym, Rc<Closure>>,
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

#[derive(Default)]
pub struct Interpreter {
    symbols: SymbolTable,
}

impl Interpreter {
    pub fn new(symbols: SymbolTable) -> Self {
        Self { symbols }
    }

    pub fn compile(&self, expr: Expr) -> Closure {
        match expr {
            Expr::Lit(lit) => {
                let val = match lit {
                    Lit::Int(n) => Value::Int(n),
                    Lit::Bool(b) => Value::Bool(b),
                };
                Box::new(move |_| val.clone())
            }

            Expr::Var(var) => {
                let sym = var.sym;
                Box::new(move |env| env.value(&sym).unwrap().clone())
            }

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile(*lhs);
                let rhs = self.compile(*rhs);

                let apply = match op {
                    BinOp::Add => |a: Value, b: Value| Value::Int(a.as_int() + b.as_int()),
                    BinOp::Sub => |a: Value, b: Value| Value::Int(a.as_int() - b.as_int()),
                    BinOp::Mul => |a: Value, b: Value| Value::Int(a.as_int() * b.as_int()),
                    BinOp::Lt => |a: Value, b: Value| Value::Bool(a.as_int() < b.as_int()),
                    BinOp::Eq => |a: Value, b: Value| Value::Bool(a == b),
                };

                Box::new(move |env| {
                    let l = lhs(env);
                    let r = rhs(env);
                    apply(l, r)
                })
            }

            Expr::If(cond, then_expr, else_expr) => {
                let cond = self.compile(*cond);
                let then_expr = self.compile(*then_expr);
                let else_expr = self.compile(*else_expr);
                Box::new(move |env| match cond(env) {
                    Value::Bool(true) => then_expr(env),
                    Value::Bool(false) => else_expr(env),
                    _ => panic!("Condition must be boolean"),
                })
            }

            Expr::Call(sym, args) => {
                let def = self.symbols.defs.get(&sym).unwrap().clone();
                let compiled_args: Vec<_> = args.into_iter().map(|arg| self.compile(arg)).collect();

                let syms = self.symbols.clone();
                let body = def.body.clone();

                Box::new(move |env| {
                    if env.cached(&sym).is_none() {
                        let interpreter = Interpreter::new(syms.clone());
                        let closure = interpreter.compile(body.clone());
                        env.cache.insert(sym, Rc::new(closure));
                    }

                    let mut values = FxHashMap::default();
                    for (param, arg) in def.args.iter().zip(&compiled_args) {
                        values.insert(*param, arg(env));
                    }

                    let mut call_env = Env::with_parent(env, values);
                    let body = env.cached(&sym).unwrap();
                    body(&mut call_env)
                })
            }
        }
    }

    pub fn eval(&self, expr: Expr) -> Value {
        let closure = self.compile(expr);
        let mut env = Env::default();
        closure(&mut env)
    }
}

pub fn eval() -> Result<(), String> {
    let fib = crate::fib();
    let sym = fib.sym;

    let mut symbol_table = SymbolTable::default();
    symbol_table.defs.insert(sym, fib);

    let interpreter = Interpreter::new(symbol_table);

    // Test fibonacci numbers 1 through 20
    for n in 1..=27 {
        println!("Calculating fib({n})...");
        let expr = Expr::Call(sym, vec![Expr::Lit(Lit::Int(n))]);

        match interpreter.eval(expr) {
            Value::Int(result) => println!("fib({n}) = {result}"),
            other => println!("Unexpected result type: {other:?}"),
        }
    }

    Ok(())
}
