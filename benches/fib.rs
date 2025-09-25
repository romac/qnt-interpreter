use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use qnt_interpreter::ast::SymbolTable;
use qnt_interpreter::{closure, fib_def, jit, main_def, pure, tree, vm, vm_optimal, wasm};

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fib");

    for n in [15, 20, 25, 30, 35].iter().copied() {
        let fib = fib_def();
        let main = main_def(n);
        let main_sym = main.sym;

        let mut syms = SymbolTable::default();
        syms.define(fib);
        syms.define(main);

        group.bench_function(BenchmarkId::new("pure", n), |b| b.iter(|| pure(n)));

        group.bench_function(BenchmarkId::new("tree", n), |b| {
            b.iter(|| tree::run(&syms, main_sym))
        });

        group.bench_function(BenchmarkId::new("closure", n), |b| {
            b.iter(|| closure::run(&syms, main_sym))
        });

        group.bench_function(BenchmarkId::new("vm", n), |b| {
            b.iter(|| vm::run(&syms, main_sym))
        });

        group.bench_function(BenchmarkId::new("vm_optimal", n), |b| {
            b.iter(|| vm_optimal::run(syms.clone(), main_sym))
        });

        group.bench_function(BenchmarkId::new("jit", n), |b| {
            b.iter(|| jit::run(&syms, main_sym))
        });

        group.bench_function(BenchmarkId::new("wasm", n), |b| {
            b.iter(|| wasm::run(&syms, main_sym))
        });
    }

    group.finish()
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
