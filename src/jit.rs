use color_eyre::eyre::eyre;
use color_eyre::Result;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{FuncId, Linkage, Module};
use cranelift::prelude::Value;
use cranelift::prelude::*;
use fxhash::FxHashMap;

use crate::ast::*;

pub struct Compiler {
    module: JITModule,
    function_ids: FxHashMap<Sym, FuncId>,
    variables: FxHashMap<Sym, Variable>,
}

impl Compiler {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        let isa_builder = cranelift::native::builder()
            .map_err(|msg| eyre!("host machine is not supported: {msg}"))?;

        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let builder = JITBuilder::with_isa(isa, cranelift::module::default_libcall_names());
        let module = JITModule::new(builder);

        Ok(Self {
            module,
            function_ids: FxHashMap::default(),
            variables: FxHashMap::default(),
        })
    }

    pub fn compile(&mut self, symbol_table: &SymbolTable) -> Result<()> {
        let mut ctx = self.module.make_context();
        let mut builder_ctx = FunctionBuilderContext::new();

        for sym in &symbol_table.syms {
            let def = symbol_table.defs.get(sym).unwrap();
            self.compile_function(def, &mut ctx, &mut builder_ctx)?;
        }

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions().unwrap();

        Ok(())
    }

    pub fn eval(&self, sym: Sym) -> Result<i64> {
        let func_id = self
            .function_ids
            .get(&sym)
            .copied()
            .ok_or_else(|| eyre!("Undefined function"))?;

        let code = self.module.get_finalized_function(func_id);

        let jit_fn = unsafe { std::mem::transmute::<*const u8, fn() -> i64>(code) };
        Ok(jit_fn())
    }

    pub fn compile_function(
        &mut self,
        def: &Def,
        ctx: &mut codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
    ) -> Result<FuncId> {
        let mut signature = self.module.make_signature();

        // Add parameters to signature
        for _ in &def.args {
            signature.params.push(AbiParam::new(types::I64));
        }
        signature.returns.push(AbiParam::new(types::I64));

        // Create function
        let func_id =
            self.module
                .declare_function(&def.sym.name.to_string(), Linkage::Export, &signature)?;

        self.function_ids.insert(def.sym, func_id);

        ctx.func.signature = signature;

        // Create function builder
        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        // Create variables for parameters
        for (i, arg) in def.args.iter().enumerate() {
            let var = Variable::new(i);
            self.variables.insert(*arg, var);
            builder.declare_var(var, types::I64);
            builder.def_var(var, builder.block_params(entry_block)[i]);
        }

        // Compile body
        let result = self.compile_expr(&mut builder, &def.body)?;
        builder.ins().return_(&[result]);
        builder.seal_all_blocks();
        builder.finalize();

        // Finish function
        self.module.define_function(func_id, ctx)?;
        self.module.clear_context(ctx);

        Ok(func_id)
    }

    fn compile_expr(&mut self, builder: &mut FunctionBuilder, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Var(var) => {
                let variable = self
                    .variables
                    .get(&var.sym)
                    .ok_or_else(|| eyre!("Undefined variable"))?;

                Ok(builder.use_var(*variable))
            }

            Expr::Lit(lit) => match lit {
                Lit::Int(n) => Ok(builder.ins().iconst(types::I64, *n)),
                Lit::Bool(b) => Ok(builder.ins().iconst(types::I64, *b as i64)),
            },

            Expr::Let(sym, value, body) => {
                let value = self.compile_expr(builder, value)?;
                let var = Variable::new(0);
                self.variables.insert(*sym, var);
                builder.declare_var(var, types::I64);
                builder.def_var(var, value);
                self.compile_expr(builder, body)
            }

            Expr::Block(exprs) => {
                let mut result = builder.ins().iconst(types::I64, 0);
                for expr in exprs {
                    result = self.compile_expr(builder, expr)?;
                }
                Ok(result)
            }

            Expr::BinOp(op, left, right) => {
                let lhs = self.compile_expr(builder, left)?;
                let rhs = self.compile_expr(builder, right)?;

                match op {
                    BinOp::Add => Ok(builder.ins().iadd(lhs, rhs)),
                    BinOp::Sub => Ok(builder.ins().isub(lhs, rhs)),
                    BinOp::Mul => Ok(builder.ins().imul(lhs, rhs)),
                    BinOp::Lt => Ok(builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)),
                    BinOp::Eq => Ok(builder.ins().icmp(IntCC::Equal, lhs, rhs)),
                }
            }
            Expr::If(cond, then_expr, else_expr) => {
                let cond_val = self.compile_expr(builder, cond)?;

                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();

                builder.append_block_param(merge_block, types::I64);

                builder
                    .ins()
                    .brif(cond_val, then_block, &[], else_block, &[]);

                builder.switch_to_block(then_block);
                let then_val = self.compile_expr(builder, then_expr)?;
                builder.ins().jump(merge_block, &[then_val]);

                builder.switch_to_block(else_block);
                let else_val = self.compile_expr(builder, else_expr)?;
                builder.ins().jump(merge_block, &[else_val]);

                builder.switch_to_block(merge_block);
                Ok(builder.block_params(merge_block)[0])
            }

            Expr::While(cond, body) => {
                let header_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                builder.ins().jump(header_block, &[]);
                builder.switch_to_block(header_block);

                let cond_val = self.compile_expr(builder, cond)?;
                builder
                    .ins()
                    .brif(cond_val, body_block, &[], exit_block, &[]);

                builder.switch_to_block(body_block);
                self.compile_expr(builder, body)?;
                builder.ins().jump(header_block, &[]);

                builder.switch_to_block(exit_block);
                Ok(builder.ins().iconst(types::I64, 0))
            }

            Expr::Call(sym, args) => {
                let func_id = self
                    .function_ids
                    .get(sym)
                    .copied()
                    .ok_or_else(|| eyre!("Undefined function"))?;

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.compile_expr(builder, arg)?);
                }

                let call = self.module.declare_func_in_func(func_id, builder.func);
                let inst = builder.ins().call(call, &arg_values);
                Ok(builder.inst_results(inst)[0])
            }
        }
    }
}

pub fn prepare(syms: &SymbolTable, main_sym: Sym) -> Result<fn() -> i64> {
    let mut compiler = Compiler::new()?;
    compiler.compile(syms)?;

    let func_id = compiler
        .function_ids
        .get(&main_sym)
        .copied()
        .ok_or_else(|| eyre!("Undefined function"))?;

    let code = compiler.module.get_finalized_function(func_id);

    let jit_fn = unsafe { std::mem::transmute::<*const u8, fn() -> i64>(code) };

    Ok(jit_fn)
}

pub fn run(syms: &SymbolTable, main_sym: Sym) -> Result<i64> {
    let mut compiler = Compiler::new()?;
    compiler.compile(syms)?;
    compiler.eval(main_sym)
}
