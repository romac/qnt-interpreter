#![allow(dead_code)]

pub mod ast;
pub mod closure;
pub mod jit;
pub mod str;
pub mod tree;
pub mod vm;
pub mod wasm;

use crate::ast::*;

pub fn fib_def(arena: &ExprArena) -> Def {
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
        body: arena.alloc(Expr::If(
            // if n < 2
            arena.alloc(Expr::BinOp(
                BinOp::Lt,
                arena.alloc(Expr::Var(Var::new(n_sym))),
                arena.alloc(Expr::Lit(Lit::Int(2))),
            )),
            // then return n
            arena.alloc(Expr::Var(Var::new(n_sym))),
            // else return fib(n-1) + fib(n-2)
            arena.alloc(Expr::BinOp(
                BinOp::Add,
                arena.alloc(Expr::Call(
                    fib_sym,
                    vec![arena.alloc(Expr::BinOp(
                        BinOp::Sub,
                        arena.alloc(Expr::Var(Var::new(n_sym))),
                        arena.alloc(Expr::Lit(Lit::Int(1))),
                    ))],
                )),
                arena.alloc(Expr::Call(
                    fib_sym,
                    vec![arena.alloc(Expr::BinOp(
                        BinOp::Sub,
                        arena.alloc(Expr::Var(Var::new(n_sym))),
                        arena.alloc(Expr::Lit(Lit::Int(2))),
                    ))],
                )),
            )),
        )),
    }
}

pub fn main_def(n: i64, arena: &ExprArena) -> Def {
    let main_sym = Sym {
        id: 3,
        name: "main".into(),
    };

    Def {
        sym: main_sym,
        args: vec![],
        body: arena.alloc(Expr::Call(
            Sym {
                id: 1,
                name: "fib".into(),
            },
            vec![arena.alloc(Expr::Lit(Lit::Int(n)))],
        )),
    }
}
