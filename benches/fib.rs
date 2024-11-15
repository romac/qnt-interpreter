use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use qnt_interpreter::ast::SymbolTable;
use qnt_interpreter::{closure, fib_def, jit, main_def, tree, vm, wasm};

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fib");

    for n in [20, 25].iter().copied() {
        let fib = fib_def();
        let main = main_def(n);
        let main_sym = main.sym;

        let mut syms = SymbolTable::default();
        syms.define(fib);
        syms.define(main);
        let syms = black_box(syms);

        group.bench_with_input(BenchmarkId::new("tree", n), &syms, |b, syms| {
            b.iter(|| tree::run(syms, main_sym))
        });

        group.bench_with_input(BenchmarkId::new("closure", n), &syms, |b, syms| {
            let closure = |_| closure::prepare(syms, black_box(main_sym)).unwrap();
            b.iter(|| closure(black_box(n)))
        });

        group.bench_with_input(BenchmarkId::new("vm", n), &syms, |b, syms| {
            let mut vm = vm::prepare(syms).unwrap();
            b.iter(|| vm.call(black_box(main_sym), vec![]))
        });

        group.bench_with_input(BenchmarkId::new("jit", n), &syms, |b, syms| {
            let run = |_| jit::prepare(syms, main_sym).unwrap();
            b.iter(|| run(black_box(n)))
        });

        group.bench_with_input(BenchmarkId::new("wasm", n), &syms, |b, syms| {
            let engine = wasmtime::Engine::default();
            let module = wasm::prepare(&engine, syms, main_sym).unwrap();
            b.iter(|| wasm::execute(&engine, black_box(&module)))
        });
    }

    group.finish()
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
