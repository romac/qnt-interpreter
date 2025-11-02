use crate::ast::*;
use color_eyre::eyre::eyre;
use color_eyre::Result;
use fxhash::FxHashMap;

#[derive(Debug)]
enum Instr {
    // Stack operations
    Push(Value), // Push constant value
    Load(usize), // Load variable from call-frame

    // Arithmetic & comparison
    Add, // Add top two values
    Sub, // Subtract top two values
    Lt,  // Less than comparison

    // Control flow
    Jump(usize),        // Unconditional jump
    JumpIfFalse(usize), // Conditional jump
    Call(usize, usize), // Function call (symbol, arg count)
    Return,             // Return from function
}

#[derive(Default, Debug)]
struct Frame {
    local_offset: usize,
    return_addr: usize,
}

#[derive(Debug)]
struct VM {
    code: Vec<Instr>,
    locals: Vec<Value>,
    stack: Vec<Value>,
    frames: Vec<Frame>,
    pc: usize,
}

impl VM {
    fn new(code: Vec<Instr>, main: usize) -> Self {
        VM {
            code,
            locals: Vec::with_capacity(8),
            stack: Vec::with_capacity(64),
            frames: Vec::with_capacity(128),
            pc: main,
        }
    }

    fn print_code(&self) {
        for (i, instr) in self.code.iter().enumerate() {
            println!("{:04}: {:?}", i, instr);
        }
    }

    fn execute(&mut self) -> Result<Value> {
        loop {
            // println!("Executing ({}): {:?}", self.pc, &self.code[self.pc]);

            match &self.code[self.pc] {
                // Stack operations
                Instr::Push(value) => {
                    self.stack.push(value.clone());
                    self.pc += 1;
                }

                Instr::Load(i) => {
                    let frame = self.frames.last().unwrap();
                    let value = &self.locals[frame.local_offset + i];
                    self.stack.push(value.clone());
                    self.pc += 1;
                }

                // Arithmetic & comparison
                Instr::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(BinOp::Add.eval_inline(&a, &b)?);
                    self.pc += 1;
                }

                Instr::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(BinOp::Sub.eval_inline(&a, &b)?);
                    self.pc += 1;
                }

                Instr::Lt => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(BinOp::Lt.eval_inline(&a, &b)?);
                    self.pc += 1;
                }

                // Control flow
                Instr::Jump(i) => {
                    self.pc = *i;
                }

                Instr::JumpIfFalse(i) => {
                    let cond = self.stack.pop().unwrap();
                    if !cond.as_bool()? {
                        self.pc = *i;
                    } else {
                        self.pc += 1;
                    }
                }

                Instr::Call(ptr, args_count) => {
                    let local_offset = self.locals.len();
                    for _ in 0..*args_count {
                        let value = self.stack.pop().unwrap();
                        self.locals.push(value);
                    }
                    let frame = Frame {
                        local_offset,
                        return_addr: self.pc + 1,
                    };
                    self.frames.push(frame);
                    self.pc = *ptr;
                }

                Instr::Return => {
                    if let Some(frame) = self.frames.pop() {
                        self.locals.truncate(frame.local_offset);
                        self.pc = frame.return_addr;
                    } else {
                        break;
                    }
                }
            }
        }

        self.stack.pop().ok_or_else(|| eyre!("Stack empty"))
    }
}

#[derive(Default)]
struct Compiler {
    code: Vec<Instr>,
    defs: FxHashMap<Sym, usize>,
    locals: FxHashMap<Sym, usize>,
}

impl Compiler {
    fn compile(mut syms: SymbolTable, main_sym: Sym) -> Result<(Vec<Instr>, usize)> {
        let mut compiler = Self::default();
        let main = syms.defs.remove(&main_sym).unwrap();
        compiler.compile_syms(syms)?;

        let main_ptr = compiler.code.len();
        compiler.compile_fn(main)?;
        Ok((compiler.code, main_ptr))
    }

    fn compile_syms(&mut self, syms: SymbolTable) -> Result<()> {
        for def in syms.defs.into_values() {
            self.compile_fn(def)?;
        }
        Ok(())
    }

    fn compile_fn(&mut self, def: Def) -> Result<()> {
        let index = self.code.len();
        self.defs.insert(def.sym, index);

        self.locals.clear();
        for arg in &def.args {
            let index = self.locals.len();
            self.locals.insert(arg.sym, index);
        }

        self.compile_expr(def.body)?;
        self.code.push(Instr::Return);
        Ok(())
    }

    fn compile_expr(&mut self, expr: Expr) -> Result<()> {
        match expr {
            Expr::Lit(Lit::Bool(b)) => {
                let value = Value::Bool(b);
                self.code.push(Instr::Push(value));
            }

            Expr::Lit(Lit::Int(n)) => {
                let value = Value::Int(n);
                self.code.push(Instr::Push(value));
            }

            Expr::Var(var) => {
                let sym = var.sym;
                let index = self.locals.get(&sym).unwrap();
                self.code.push(Instr::Load(*index));
            }

            Expr::BinOp(op, left, right) => {
                self.compile_expr(*left)?;
                self.compile_expr(*right)?;

                match op {
                    BinOp::Add => self.code.push(Instr::Add),
                    BinOp::Sub => self.code.push(Instr::Sub),
                    BinOp::Lt => self.code.push(Instr::Lt),
                    _ => unimplemented!(),
                }
            }

            Expr::If(cnd, thn, els) => {
                self.compile_expr(*cnd)?;
                let jump_if_false = self.code.len();
                self.code.push(Instr::JumpIfFalse(0));

                self.compile_expr(*thn)?;
                let jump_end = self.code.len();
                self.code.push(Instr::Jump(0));

                let else_start = self.code.len();
                self.compile_expr(*els)?;
                let after = self.code.len();

                // Patch jumps
                self.code[jump_if_false] = Instr::JumpIfFalse(else_start);
                self.code[jump_end] = Instr::Jump(after);
            }

            Expr::Call(sym, args) => {
                let arity = args.len();
                for arg in args {
                    self.compile_expr(arg)?;
                }
                let ptr = self.defs.get(&sym).unwrap();
                self.code.push(Instr::Call(*ptr, arity));
            }

            Expr::While(_cond, _body) => unimplemented!(),
            Expr::Block(_exprs) => unimplemented!(),
            Expr::Let(_sym, _value, _body) => unimplemented!(),
            Expr::Lit(Lit::Set(_)) => unimplemented!(),
            Expr::SetAdd(_, _) => unimplemented!(),
            Expr::SetContains(_, _) => unimplemented!(),
        }

        Ok(())
    }
}

pub fn run(syms: SymbolTable, main_sym: Sym) -> Result<i64> {
    let (code, main) = Compiler::compile(syms, main_sym)?;
    let mut vm = VM::new(code, main);
    // vm.print_code();

    match vm.execute()? {
        Value::Int(result) => Ok(result),
        _ => Err(eyre!("Expected integer result")),
    }
}
