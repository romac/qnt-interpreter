# QNT Interpreters

Interpreters for a toy language using various techniques.

The abstract syntax tree can be found in [`src/ast.rs`](./src/ast.rs).
It is currently extremely limited, in order to ease the implementation
of the multiple interpreters, and should eventually be extended with
more interesting data types and operations, eg. sets, records, etc.

## Usage

Compute the nth fibonacci number with a given interpreter:

```
$ cargo run --release -- <n> <tree|closure|vm|jit|wasm>
```

eg.

```
$ cargo run --release -- 29 closure
```

Run the [benchmarks](./benches/fib.rs) with

```
$ cargo bench
```

## Techniques implemented

- [Tree-walking](./src/tree.rs)
- [Closure](./src/closure.rs)
- [VM](./src/vm.rs)
- [JIT](./src/jit.rs)
- [WASM](./src/wasm.rs)

## License

Copyright 2024 Romain Ruetschi

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
