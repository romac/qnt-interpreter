use criterion::{black_box, criterion_group, criterion_main, Criterion};
use qnt_interpreter::ast::{ExprArena, SymbolTable};
use qnt_interpreter::{closure, fib_def, jit, main_def, tree, vm, wasm};

pub fn criterion_benchmark(c: &mut Criterion) {
    let n = 27;
    let arena = ExprArena::new();

    let fib = fib_def(&arena);
    let main = main_def(n, &arena);
    let main_sym = main.sym;

    let mut syms = SymbolTable::new(arena);
    syms.define(fib);
    syms.define(main);
    let syms = black_box(syms);

    c.bench_function(&format!("tree: fib({n})"), |b| {
        b.iter(|| tree::run(&syms, main_sym))
    });

    c.bench_function(&format!("closure: fib({n})"), |b| {
        let closure = |_| closure::prepare(&syms, black_box(main_sym)).unwrap();
        b.iter(|| closure(black_box(n)))
    });

    c.bench_function(&format!("vm: fib({n})"), |b| {
        let mut vm = vm::prepare(&syms).unwrap();
        b.iter(|| vm.call(black_box(main_sym), vec![]))
    });

    c.bench_function(&format!("jit: fib({n})"), |b| {
        let run = |_| jit::prepare(&syms, main_sym).unwrap();
        b.iter(|| run(black_box(n)))
    });

    c.bench_function(&format!("wasm: fib({n})"), |b| {
        let path = wasm::prepare(&syms, main_sym).unwrap();
        b.iter(|| wasm::execute(black_box(&path)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);