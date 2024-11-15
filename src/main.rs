#![allow(dead_code)]

mod ast;
mod closure;
mod str;
mod tree;

use color_eyre::eyre::eyre;
use color_eyre::Result;

use crate::ast::*;

fn fib() -> Def {
    let fib_sym = Sym {
        id: 1,
        name: "fib".into(),
    };

    let n_sym = Sym {
        id: 2,
        name: "n".into(),
    };

    // Create the fibonacci function definition
    Def {
        sym: fib_sym,
        args: vec![n_sym],
        body: Expr::If(
            // if n < 2
            Box::new(Expr::BinOp(
                BinOp::Lt,
                Box::new(Expr::Var(Var::new(n_sym))),
                Box::new(Expr::Lit(Lit::Int(2))),
            )),
            // then return n
            Box::new(Expr::Var(Var::new(n_sym))),
            // else return fib(n-1) + fib(n-2)
            Box::new(Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::Call(
                    fib_sym,
                    vec![Expr::BinOp(
                        BinOp::Sub,
                        Box::new(Expr::Var(Var::new(n_sym))),
                        Box::new(Expr::Lit(Lit::Int(1))),
                    )],
                )),
                Box::new(Expr::Call(
                    fib_sym,
                    vec![Expr::BinOp(
                        BinOp::Sub,
                        Box::new(Expr::Var(Var::new(n_sym))),
                        Box::new(Expr::Lit(Lit::Int(2))),
                    )],
                )),
            )),
        ),
    }
}

fn main() -> Result<()> {
    if std::env::args().len() != 2 {
        return Err(eyre!("Usage: cargo run <tree|closure|bytecode>"));
    }

    let fib = fib();
    let sym = fib.sym;

    let mut syms = SymbolTable::default();
    syms.defs.insert(sym, fib);

    match std::env::args().nth(1).unwrap().as_str() {
        "tree" => {
            let (interpreter, mut env) = tree::prepare(&syms);
            run(sym, |expr| interpreter.eval(&expr, &mut env))
        }

        "closure" => {
            let (interpreter, mut env) = closure::prepare(&syms)?;
            run(sym, |expr| interpreter.eval(&expr, &mut env))
        }

        _ => Err(eyre!("Invalid mode")),
    }
}

fn run(sym: Sym, mut eval: impl FnMut(Expr) -> Result<Value>) -> Result<()> {
    // Test fibonacci numbers 1 through 27
    for n in 1..=27 {
        let expr = Expr::Call(sym, vec![Expr::Lit(Lit::Int(n))]);

        match eval(expr)? {
            Value::Int(result) => println!("fib({n}) = {result}"),
            _ => return Err(eyre!("Expected integer result")),
        }
    }

    Ok(())
}
