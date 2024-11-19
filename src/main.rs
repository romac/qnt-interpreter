use color_eyre::eyre::eyre;
use color_eyre::Result;

use qnt_interpreter::{ast::*, closure, fib_def, jit, main_def, tree, vm, wasm};

fn main() -> Result<()> {
    if std::env::args().len() != 3 {
        return Err(eyre!("Usage: cargo run <n> <tree|closure|vm|jit>"));
    }

    let n: i64 = std::env::args().nth(1).unwrap().parse()?;

    let arena = ExprArena::new();

    let fib = fib_def(&arena);
    let main = main_def(n, &arena);
    let main_sym = main.sym;

    let mut syms = SymbolTable::new(arena);
    syms.define(fib);
    syms.define(main);

    let run = match std::env::args().nth(2).unwrap().as_str() {
        "tree" => tree::run,
        "closure" => closure::run,
        "vm" => vm::run,
        "jit" => jit::run,
        "wasm" => wasm::run,
        _ => return Err(eyre!("Invalid mode")),
    };

    let result = run(&syms, main_sym)?;
    println!("fib(n) = {result}");

    Ok(())
}
