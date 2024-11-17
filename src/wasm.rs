use std::fmt::Write;

use fxhash::FxHashMap;

use crate::ast::*;

pub struct WasmCompiler {
    locals: FxHashMap<Sym, u32>,
    next_local: u32,
    output: String,
    indent: usize,
}

impl WasmCompiler {
    pub fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
            next_local: 0,
            output: String::new(),
            indent: 0,
        }
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

    pub fn compile(mut self, symtab: &SymbolTable) -> String {
        self.write_line("(module");
        self.indent += 1;

        self.write_line("(memory 1)");
        self.write_line("(export \"memory\" (memory 0))");

        for def in symtab.defs.values() {
            self.compile_def(def);
        }

        self.write_line("(export \"_start\" (func $main))");

        self.indent -= 1;
        self.write_line(")");

        self.output
    }

    fn compile_def(&mut self, def: &Def) {
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
        self.compile_expr(&def.body);
        self.indent -= 1;

        self.write_line(")");
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
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

                self.compile_expr(val);
                self.compile_expr(body);
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr);
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
                writeln!(&mut self.output, "").unwrap();

                self.indent += 1;
                self.compile_expr(lhs);
                self.compile_expr(rhs);
                self.indent -= 1;

                self.write_line(")");
            }
            Expr::Call(sym, args) => {
                self.write_indent();
                write!(&mut self.output, "(call ${}", sym.name).unwrap();
                if !args.is_empty() {
                    writeln!(&mut self.output, "").unwrap();
                    self.indent += 1;
                    for arg in args {
                        self.compile_expr(arg);
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
                self.compile_expr(cond);
                self.write_line("(then");
                self.indent += 1;
                self.compile_expr(then);
                self.indent -= 1;
                self.write_line(")");
                self.write_line("(else");
                self.indent += 1;
                self.compile_expr(else_);
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
                self.compile_expr(cond);
                self.write_line("(then");
                self.indent += 1;
                self.compile_expr(body);
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
