#![allow(dead_code)]

mod ast;
mod closure;
mod jit;
mod str;
mod tree;
mod vm;

use color_eyre::eyre::eyre;
use color_eyre::Result;

use crate::ast::*;

fn fib_def() -> Def {
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

fn main_def(n: i64) -> Def {
    let main_sym = Sym {
        id: 3,
        name: "main".into(),
    };

    Def {
        sym: main_sym,
        args: vec![],
        body: Expr::Call(
            Sym {
                id: 1,
                name: "fib".into(),
            },
            vec![Expr::Lit(Lit::Int(n))],
        ),
    }
}

fn main() -> Result<()> {
    if std::env::args().len() != 2 {
        return Err(eyre!("Usage: cargo run <tree|closure|vm>"));
    }

    let fib = fib_def();
    let main = main_def(29);
    let main_sym = main.sym;

    let mut syms = SymbolTable::default();
    syms.define(fib);
    syms.define(main);

    match std::env::args().nth(1).unwrap().as_str() {
        "tree" => {
            let interpreter = tree::Interpreter::new(&syms);
            run(main_sym, |expr| interpreter.eval(&expr))
        }

        "closure" => {
            let interpreter = closure::Interpreter::new(&syms);
            run(main_sym, |expr| interpreter.eval(&expr))
        }

        "vm" => {
            let compiler = vm::Compiler::new(&syms);
            let (code, ctx) = compiler.compile()?;

            let mut vm = vm::VM::new(code, ctx.defs, &syms);
            // vm.print_code();

            let result = vm.call(main_sym, vec![])?;
            println!("main() = {result:?}");

            Ok(())
        }

        "jit" => {
            let mut compiler = jit::Compiler::new()?;
            compiler.compile(&syms)?;

            let result = compiler.eval(main_sym)?;
            println!("main() = {result}");

            Ok(())
        }

        _ => Err(eyre!("Invalid mode")),
    }
}

fn run(sym: Sym, mut eval: impl FnMut(Expr) -> Result<Value>) -> Result<()> {
    let expr = Expr::Call(sym, vec![]);

    match eval(expr)? {
        Value::Int(result) => println!("main() = {result}"),
        _ => return Err(eyre!("Expected integer result")),
    }

    Ok(())
}
