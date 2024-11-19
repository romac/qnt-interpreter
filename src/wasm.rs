use std::fmt::Write;
use std::path::{Path, PathBuf};

use fxhash::FxHashMap;

use color_eyre::eyre::eyre;
use color_eyre::Result;

use crate::ast::*;

#[derive(Default)]
pub struct WasmCompiler {
    locals: FxHashMap<Sym, u32>,
    next_local: u32,
    output: String,
    indent: usize,
}

impl WasmCompiler {
    pub fn new() -> Self {
        Self::default()
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            write!(&mut self.output, "  ").unwrap();
        }
    }

    fn write_line(&mut self, s: &str) {
        self.write_indent();
        writeln!(&mut self.output, "{}", s).unwrap();
    }

    pub fn compile(mut self, symbols: &SymbolTable, main_sym: Sym) -> String {
        self.write_line("(module");
        self.indent += 1;

        self.write_line("(memory 1)");
        self.write_line("(export \"memory\" (memory 0))");

        for def in symbols.defs.values() {
            self.compile_def(def, &symbols.arena);
        }

        self.write_line(&format!("(export \"_start\" (func ${}))", main_sym.name));

        self.indent -= 1;
        self.write_line(")");

        self.output
    }

    fn compile_def(&mut self, def: &Def, arena: &ExprArena) {
        self.locals.clear();
        self.next_local = 0;

        self.write_indent();
        write!(&mut self.output, "(func ${}", def.sym.name).unwrap();

        for arg in &def.args {
            write!(&mut self.output, " (param $p{} i64)", self.next_local).unwrap();
            self.locals.insert(*arg, self.next_local);
            self.next_local += 1;
        }
        writeln!(&mut self.output, " (result i64)").unwrap();

        self.indent += 1;
        self.compile_expr(&def.body, arena);
        self.indent -= 1;

        self.write_line(")");
    }

    fn compile_expr(&mut self, expr: &ExprRef, arena: &ExprArena) {
        match arena.get(*expr) {
            Expr::Var(var) => {
                let local = self.locals[&var.sym];
                self.write_line(&format!("(local.get $p{})", local));
            }
            Expr::Lit(lit) => match lit {
                Lit::Int(n) => self.write_line(&format!("(i64.const {})", n)),
                Lit::Bool(b) => self.write_line(&format!("(i64.const {})", if *b { 1 } else { 0 })),
            },
            Expr::Let(sym, val, body) => {
                self.write_indent();
                write!(&mut self.output, "(local $l{} i64)", self.next_local).unwrap();
                self.locals.insert(*sym, self.next_local);
                self.next_local += 1;
                writeln!(&mut self.output).unwrap();

                self.compile_expr(val, arena);
                self.compile_expr(body, arena);
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr, arena);
                }
            }
            Expr::BinOp(op, lhs, rhs) => {
                self.write_indent();
                write!(&mut self.output, "(").unwrap();
                match op {
                    BinOp::Add => write!(&mut self.output, "i64.add"),
                    BinOp::Sub => write!(&mut self.output, "i64.sub"),
                    BinOp::Mul => write!(&mut self.output, "i64.mul"),
                    BinOp::Lt => write!(&mut self.output, "i64.lt_s"),
                    BinOp::Eq => write!(&mut self.output, "i64.eq"),
                }
                .unwrap();
                writeln!(&mut self.output).unwrap();

                self.indent += 1;
                self.compile_expr(lhs, arena);
                self.compile_expr(rhs, arena);
                self.indent -= 1;

                self.write_line(")");
            }
            Expr::Call(sym, args) => {
                self.write_indent();
                write!(&mut self.output, "(call ${}", sym.name).unwrap();
                if !args.is_empty() {
                    writeln!(&mut self.output).unwrap();
                    self.indent += 1;
                    for arg in args {
                        self.compile_expr(arg, arena);
                    }
                    self.indent -= 1;
                    self.write_line(")");
                } else {
                    writeln!(&mut self.output, ")").unwrap();
                }
            }
            Expr::If(cond, then, else_) => {
                self.write_line("(if (result i64)");
                self.indent += 1;
                self.compile_expr(cond, arena);
                self.write_line("(then");
                self.indent += 1;
                self.compile_expr(then, arena);
                self.indent -= 1;
                self.write_line(")");
                self.write_line("(else");
                self.indent += 1;
                self.compile_expr(else_, arena);
                self.indent -= 1;
                self.write_line(")");
                self.indent -= 1;
                self.write_line(")");
            }
            Expr::While(cond, body) => {
                self.write_line("(loop");
                self.indent += 1;
                self.write_line("(if");
                self.indent += 1;
                self.compile_expr(cond, arena);
                self.write_line("(then");
                self.indent += 1;
                self.compile_expr(body, arena);
                self.write_line("(br 1)");
                self.indent -= 1;
                self.write_line(")");
                self.indent -= 1;
                self.write_line(")");
                self.indent -= 1;
                self.write_line(")");
                self.write_line("(i64.const 0)");
            }
        }
    }
}

pub fn compile(code: &str) -> Result<PathBuf> {
    use std::process::Command;

    // Call `wat2wasm` to compile the WAT code to a binary Wasm module`
    // and write it to a temporary file.
    let wat_path = std::env::temp_dir().join("code.wat");
    let wasm_path = std::env::temp_dir().join("code.wasm");
    std::fs::write(&wat_path, code)?;

    let output = Command::new("wat2wasm")
        .arg(&wat_path)
        .arg("-o")
        .arg(&wasm_path)
        .output()?;

    if !output.status.success() {
        return Err(eyre!(
            "wat2wasm failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(wasm_path)
}

pub fn execute(wasm_path: &Path) -> Result<i64> {
    use std::process::Command;

    // Call `wasmtime` to run the Wasm module and capture its output.
    let output = Command::new("wasmtime")
        .arg("--invoke")
        .arg("_start")
        .arg(wasm_path)
        .output()?;

    if !output.status.success() {
        return Err(eyre!(
            "wasmtime failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    // Parse the output as an integer and return it.
    let text = String::from_utf8_lossy(&output.stdout);
    text.trim().parse().map_err(Into::into)
}

pub fn prepare(syms: &SymbolTable, main_sym: Sym) -> Result<PathBuf> {
    let compiler = WasmCompiler::new();
    let code = compiler.compile(syms, main_sym);
    compile(&code)
}

pub fn run(syms: &SymbolTable, main_sym: Sym) -> Result<i64> {
    prepare(syms, main_sym).and_then(|path| execute(&path))
}
