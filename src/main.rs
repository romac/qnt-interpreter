#![allow(dead_code)]

mod ast;
mod closure;
mod str;
mod tree;

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

fn main() -> Result<(), String> {
    if std::env::args().len() != 2 {
        return Err("Usage: cargo run <tree|closure|bytecode>".into());
    }

    match std::env::args().nth(1).unwrap().as_str() {
        "tree" => tree::eval(),

        "closure" => closure::eval(),

        "bytecode" => {
            Ok(())
            // let def = fib();
            // let closure = def.to_closure();
            // let bytecode = closure.to_bytecode();
            // println!("{:?}", bytecode);
        }
        _ => Err("Invalid mode".into()),
    }
}
