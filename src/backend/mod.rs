use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_module::{DataContext, Module, Linkage};
use cranelift_object::{ObjectModule, ObjectBuilder};
use target_lexicon::triple;

use crate::frontend::ast_lowering::{SExpr, Type as SExprType};

pub struct Generator {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: ObjectModule,
}

impl Default for Generator {
    fn default() -> Self {
        use std::str::FromStr;

        let mut b = settings::builder();
        b.set("opt_level", "speed_and_size").unwrap();

        let f = settings::Flags::new(b);
        let isa_data = isa::lookup(triple!("x86_64-elf")).unwrap().finish(f);
        let builder = ObjectBuilder::new(isa_data, "x86_64", cranelift_module::default_libcall_names()).unwrap();
        let module = ObjectModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

impl Generator {
    pub fn compile(&mut self, sexprs: Vec<SExpr<'_>>) {
        self.translate(&sexprs);
    }

    fn translate(&mut self, sexprs: &[SExpr<'_>]) {
        for sexpr in sexprs {
            if let SExpr::FuncDef { name, ret_type, args, expr, .. } = sexpr {
                for (_, typ) in args {
                    let typ = Self::convert_type_to_type(typ);
                    self.ctx.func.signature.params.push(AbiParam::new(typ));
                }

                if *ret_type != SExprType::Tuple(vec![]) {
                    let typ = Self::convert_type_to_type(ret_type);
                    self.ctx.func.signature.returns.push(AbiParam::new(typ));
                }

                let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
                let entry_block = builder.create_block();
                builder.append_block_params_for_function_params(entry_block);
                builder.switch_to_block(entry_block);
                builder.seal_block(entry_block);
                let mut var_map = HashMap::new();
                let mut var_index = 0;
                for (i, (name, typ)) in args.iter().enumerate() {
                    let var = Variable::new(var_index);
                    var_map.insert(*name, var);
                    var_index += 1;
                    builder.declare_var(var, Self::convert_type_to_type(typ));
                    builder.def_var(var, builder.block_params(entry_block)[i]);
                }

                let ret_value = Self::translate_expr(&**expr, &mut builder, &mut var_map, &mut var_index);
                builder.ins().return_(&[ret_value]);
                builder.finalize();

                let id = self.module.declare_function(name, Linkage::Export, &self.ctx.func.signature).unwrap();
                self.module.define_function(id, &mut self.ctx).unwrap();
                self.module.clear_context(&mut self.ctx);
            }
        }
    }

    fn translate_expr(sexpr: &SExpr, builder: &mut FunctionBuilder, var_map: &mut HashMap<&'_ str, Variable>, var_index: &mut usize) -> Value {
        #[allow(unused)]
        match sexpr {
            SExpr::Int { meta, value } => {
                if matches!(meta.type_, SExprType::Int(_, 1)) {
                    builder.ins().bconst(Self::convert_type_to_type(&meta.type_), *value != 0)
                } else {
                    builder.ins().iconst(Self::convert_type_to_type(&meta.type_), *value as i64)
                }
            }

            SExpr::Symbol { value, .. } => {
                builder.use_var(*var_map.get(value).unwrap())
            }

            SExpr::Float { meta, value } => {
                if meta.type_ == SExprType::F32 {
                    builder.ins().f32const(*value as f32)
                } else {
                    builder.ins().f64const(*value)
                }
            }

            SExpr::Str { meta, value } => todo!(),

            SExpr::Seq { meta, values } => {
                for value in values[..values.len() - 1].iter() {
                    Self::translate_expr(value, builder, var_map, var_index);
                }

                Self::translate_expr(values.last().unwrap(), builder, var_map, var_index)
            }

            SExpr::Cond { meta, values } => todo!(),
            SExpr::Loop { meta, value } => todo!(),
            SExpr::Break { meta, value } => todo!(),
            SExpr::Nil { meta } => todo!(),

            SExpr::Type { value, .. } => Self::translate_expr(value, builder, var_map, var_index),

            SExpr::FuncCall { meta, func, values } => {
                let values: Vec<_> = values.iter().map(|v| Self::translate_expr(v, builder, var_map, var_index)).collect();
                match **func {
                    SExpr::Symbol { value: "+", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            builder.ins().fadd(values[0], values[1])
                        } else {
                            builder.ins().iadd(values[0], values[1])
                        }
                    }

                    SExpr::Symbol { value: "+", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            builder.ins().fsub(values[0], values[1])
                        } else {
                            builder.ins().isub(values[0], values[1])
                        }
                    }

                    SExpr::Symbol { value: "*", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            builder.ins().fmul(values[0], values[1])
                        } else {
                            builder.ins().imul(values[0], values[1])
                        }
                    }

                    SExpr::Symbol { value: "/", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            builder.ins().fdiv(values[0], values[1])
                        } else if matches!(meta.type_, SExprType::Int(true, _)) {
                            builder.ins().sdiv(values[0], values[1])
                        } else {
                            builder.ins().udiv(values[0], values[1])
                        }
                    }

                    SExpr::Symbol { value: "%", .. } => {
                        if matches!(meta.type_, SExprType::Int(true, _)) {
                            builder.ins().srem(values[0], values[1])
                        } else {
                            builder.ins().urem(values[0], values[1])
                        }
                    }

                    _ => todo!(),
                }
            }

            SExpr::StructSet { meta, name, values } => todo!(),
            SExpr::Declare { meta, mutable, variable, value } => todo!(),
            SExpr::Assign { meta, variable, value } => todo!(),
            SExpr::Attribute { meta, top, attrs } => todo!(),

            _ => todo!(),
        }
    }

    fn convert_type_to_type(t: &SExprType) -> Type {
        match t {
            SExprType::Int(_, width) if *width == 1 => types::B1,
            SExprType::Int(_, width) if *width == 8 => types::I8,
            SExprType::Int(_, width) if *width == 16 => types::I16,
            SExprType::Int(_, width) if *width == 32 => types::I32,
            SExprType::Int(_, width) if *width == 64 => types::I64,
            SExprType::F32 => types::F32,
            SExprType::F64 => types::F64,

            SExprType::Tuple(v) if v.is_empty() => todo!(),

            SExprType::Pointer(_, _) => todo!(),
            SExprType::Slice(_, _) => todo!(),

            SExprType::Struct(_, _) => todo!(),

            SExprType::Function(_, _) => todo!(),

            _ => unreachable!(),
        }
    }

    pub fn emit_object(self) -> Vec<u8> {
        self.module.finish().emit().unwrap()
    }
}
