#![allow(dead_code)]

pub mod ast;
pub mod str;

#[cfg(feature = "closure")]
pub mod closure;

#[cfg(feature = "jit")]
pub mod jit;

#[cfg(feature = "tree")]
pub mod tree;

#[cfg(feature = "vm")]
pub mod vm;

#[cfg(feature = "wasm")]
pub mod wasm;

use crate::ast::*;

pub fn fib_def() -> Def {
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
        args: vec![n_sym.typed(Type::Int)],
        return_type: Type::Int,
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

pub fn main_def(n: i64) -> Def {
    let main_sym = Sym {
        id: 3,
        name: "main".into(),
    };

    Def {
        sym: main_sym,
        args: vec![],
        return_type: Type::Int,
        body: Expr::Call(
            Sym {
                id: 1,
                name: "fib".into(),
            },
            vec![Expr::Lit(Lit::Int(n))],
        ),
    }
}
