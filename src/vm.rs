use color_eyre::eyre::eyre;
use color_eyre::Result;
use fxhash::FxHashMap;

use crate::ast::*;

#[derive(Clone, Debug)]
pub enum Instr {
    // Stack operations
    Push(Value),  // Push constant value
    Pop,          // Pop top value
    Load(usize),  // Load variable from frame
    Store(usize), // Store value to frame

    // Arithmetic & comparison
    Add, // Add top two values
    Sub, // Subtract top two values
    Mul, // Multiply top two values
    Lt,  // Less than comparison
    Eq,  // Equality comparison

    // Control flow
    Jump(usize),        // Unconditional jump
    JumpIfFalse(usize), // Conditional jump
    Call(Sym, usize),   // Function call (symbol, arg count)
    Return,             // Return from function
}

#[derive(Debug)]
pub struct Frame {
    locals: Vec<Value>,
    return_addr: usize,
}

#[derive(Debug)]
pub struct VM<'a> {
    code: Vec<Instr>,
    defs: FxHashMap<Sym, usize>,
    pub stack: Vec<Value>,
    frames: Vec<Frame>,
    pc: usize,
    symbol_table: &'a SymbolTable,
}

impl<'a> VM<'a> {
    pub fn new(
        code: Vec<Instr>,
        defs: FxHashMap<Sym, usize>,
        symbol_table: &'a SymbolTable,
    ) -> Self {
        VM {
            code,
            defs,
            stack: Vec::new(),
            frames: Vec::new(),
            pc: 0,
            symbol_table,
        }
    }

    pub fn print_code(&self) {
        for (i, instr) in self.code.iter().enumerate() {
            println!("{:04}: {:?}", i, instr);
        }
    }

    pub fn call(&mut self, sym: Sym, args: Vec<Value>) -> Result<Value> {
        let def = self
            .symbol_table
            .defs
            .get(&sym)
            .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

        if args.len() != def.args.len() {
            return Err(eyre!(
                "Wrong number of arguments for {sym}: found {}, expected {}",
                args.len(),
                def.args.len()
            ));
        }

        let index = self
            .defs
            .get(&sym)
            .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

        let frame = Frame {
            locals: args,
            return_addr: usize::MAX,
        };

        self.frames.push(frame);

        self.pc = *index;

        self.execute()
    }

    pub fn execute(&mut self) -> Result<Value> {
        while self.pc < self.code.len() {
            // println!("Executing ({}): {:?}", self.pc, &self.code[self.pc]);

            match &self.code[self.pc] {
                // Stack operations
                Instr::Push(value) => {
                    self.stack.push(value.clone());
                    self.pc += 1;
                }

                Instr::Pop => {
                    self.stack.pop();
                    self.pc += 1;
                }

                Instr::Load(i) => {
                    let frame = self.frames.last().unwrap();
                    self.stack.push(frame.locals[*i].clone());
                    self.pc += 1;
                }

                Instr::Store(v) => {
                    let value = self.stack.pop().unwrap();
                    let frame = self.frames.last_mut().unwrap();
                    frame.locals[*v] = value;
                    self.pc += 1;
                }

                // Arithmetic & comparison
                Instr::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = BinOp::Add.eval(a, b)?;
                    self.stack.push(res);
                    self.pc += 1;
                }

                Instr::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = BinOp::Sub.eval(a, b)?;
                    self.stack.push(res);
                    self.pc += 1;
                }

                Instr::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = BinOp::Mul.eval(a, b)?;
                    self.stack.push(res);
                    self.pc += 1;
                }

                Instr::Lt => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = BinOp::Lt.eval(a, b)?;
                    self.stack.push(res);
                    self.pc += 1;
                }

                Instr::Eq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = BinOp::Eq.eval(a, b)?;
                    self.stack.push(res);
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

                Instr::Call(sym, args_count) => {
                    let def = self
                        .symbol_table
                        .defs
                        .get(sym)
                        .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

                    let code_index = self
                        .defs
                        .get(sym)
                        .ok_or_else(|| eyre!("Undefined function: {sym}"))?;

                    if args_count != &def.args.len() {
                        return Err(eyre!(
                            "Wrong number of arguments for {sym}: found {}, expected {}",
                            args_count,
                            def.args.len()
                        ));
                    }

                    // Pop arguments from stack and bind them to parameters
                    let mut values = Vec::new();
                    for _ in 0..*args_count {
                        values.push(self.stack.pop().unwrap());
                    }

                    // Create new frame
                    let frame = Frame {
                        locals: values,
                        return_addr: self.pc + 1,
                    };

                    self.frames.push(frame);

                    // Jump to function body
                    self.pc = *code_index;
                }

                Instr::Return => {
                    let return_value = self.stack.pop().unwrap();
                    let frame = self.frames.pop().unwrap();
                    self.pc = frame.return_addr;
                    self.stack.push(return_value);
                }
            }
        }

        self.stack.pop().ok_or_else(|| eyre!("Stack empty"))
    }
}

pub struct Compiler<'a> {
    code: Vec<Instr>,
    symbol_table: &'a SymbolTable,
}

#[derive(Debug)]
pub struct Context {
    pub locals: FxHashMap<Sym, usize>,
    pub defs: FxHashMap<Sym, usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            code: Vec::new(),
            symbol_table,
        }
    }

    pub fn compile(mut self) -> Result<(Vec<Instr>, Context)> {
        let mut ctx = Context {
            locals: FxHashMap::default(),
            defs: FxHashMap::default(),
        };

        for sym in &self.symbol_table.syms {
            let def = self.symbol_table.defs.get(sym).unwrap();
            self.compile_fn(def, &mut ctx)?;
        }

        Ok((self.code, ctx))
    }

    pub fn compile_fn(&mut self, def: &Def, ctx: &mut Context) -> Result<()> {
        let index = self.code.len();
        ctx.defs.insert(def.sym, index);

        ctx.locals.clear();
        for arg in &def.args {
            let index = ctx.locals.len();
            ctx.locals.insert(arg.sym, index);
        }

        self.compile_expr(&def.body, ctx)?;

        self.code.push(Instr::Return);

        Ok(())
    }

    pub fn compile_expr(&mut self, expr: &Expr, ctx: &mut Context) -> Result<()> {
        match expr {
            Expr::Lit(lit) => {
                self.code.push(Instr::Push(lit.to_value()));
            }

            Expr::Var(var) => {
                let sym = var.sym;
                let index = ctx.locals.get(&sym).unwrap();

                self.code.push(Instr::Load(*index));
            }

            Expr::Let(sym, value, body) => {
                self.compile_expr(value, ctx)?;

                let index = ctx.locals.len();
                ctx.locals.insert(sym.sym, index);

                self.code.push(Instr::Store(index));

                self.compile_expr(body, ctx)?;
            }

            Expr::BinOp(op, left, right) => {
                self.compile_expr(left, ctx)?;
                self.compile_expr(right, ctx)?;

                match op {
                    BinOp::Add => self.code.push(Instr::Add),
                    BinOp::Sub => self.code.push(Instr::Sub),
                    BinOp::Mul => self.code.push(Instr::Mul),
                    BinOp::Lt => self.code.push(Instr::Lt),
                    BinOp::Eq => self.code.push(Instr::Eq),
                }
            }

            Expr::Call(sym, args) => {
                for arg in args {
                    self.compile_expr(arg, ctx)?;
                }

                self.code.push(Instr::Call(*sym, args.len()));
            }

            Expr::If(cnd, thn, els) => {
                self.compile_expr(cnd, ctx)?;

                let jump_if_false = self.code.len();
                self.code.push(Instr::JumpIfFalse(0));

                self.compile_expr(thn, ctx)?;

                let jump_end = self.code.len();
                self.code.push(Instr::Jump(0));

                let else_start = self.code.len();
                self.compile_expr(els, ctx)?;

                let after = self.code.len();

                // Patch jumps
                self.code[jump_if_false] = Instr::JumpIfFalse(else_start);
                self.code[jump_end] = Instr::Jump(after);
            }

            Expr::While(cond, body) => {
                let loop_start = self.code.len();

                self.compile_expr(cond, ctx)?;

                let jump_if_false = self.code.len();
                self.code.push(Instr::JumpIfFalse(0));

                self.compile_expr(body, ctx)?;

                self.code.push(Instr::Jump(loop_start));

                let after = self.code.len();

                self.code[jump_if_false] = Instr::JumpIfFalse(after);
            }

            Expr::Block(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr, ctx)?;
                }
            }
        }

        Ok(())
    }
}

pub fn prepare(syms: &SymbolTable) -> Result<VM> {
    let compiler = Compiler::new(syms);
    let (code, ctx) = compiler.compile()?;

    let vm = VM::new(code, ctx.defs, syms);
    // vm.print_code();
    Ok(vm)
}

pub fn run(syms: &SymbolTable, main_sym: Sym) -> Result<i64> {
    let compiler = Compiler::new(syms);
    let (code, ctx) = compiler.compile()?;

    let mut vm = VM::new(code, ctx.defs, syms);
    // vm.print_code();

    match vm.call(main_sym, vec![])? {
        Value::Int(result) => Ok(result),
        _ => Err(eyre!("Expected integer result")),
    }
}
