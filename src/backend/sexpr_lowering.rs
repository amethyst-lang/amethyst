use std::collections::HashMap;

use inkwell::{context::Context, builder::Builder, passes::PassManager, values::FunctionValue, module::{Module, Linkage}, types::{FunctionType, BasicMetadataTypeEnum}};

use crate::frontend::ast_lowering::{SExpr, Type as SExprType};

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    func_map: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    fn convert_type(&self, type_: &SExprType) -> BasicMetadataTypeEnum<'ctx> {
        match type_ {
            SExprType::Int(_, 8) => self.context.i8_type().into(),
            SExprType::Int(_, 16) => self.context.i16_type().into(),
            SExprType::Int(_, 32) => self.context.i32_type().into(),
            SExprType::Int(_, 64) => self.context.i64_type().into(),
            SExprType::Int(_, 128) => self.context.i128_type().into(),
            SExprType::F32 => self.context.f32_type().into(),
            SExprType::F64 => self.context.f64_type().into(),
            SExprType::Tuple(_) => todo!(),
            SExprType::Pointer(_, _) => todo!(),
            SExprType::Slice(_, _) => todo!(),
            SExprType::Struct(_, _) => todo!(),
            SExprType::Union(_) => todo!(),
            SExprType::Function(_, _) => todo!(),

            _ => unreachable!(),
        }
    }

    fn mangle_func_name(&self, name: &str) -> String {
        format!("_amy_{}", name)
    }

    pub fn lower(sexprs: Vec<SExpr>) {
        let context = Context::create();
        let mut compiler = Compiler {
            context: &context,
            builder: context.create_builder(),
            module: context.create_module("uwu"),
            func_map: HashMap::new(),
        };

        for sexpr in sexprs.iter() {
            match sexpr {
                SExpr::FuncDef { name, ret_type, args, .. } => {
                    let type_ = compiler.convert_type(ret_type);
                    let arg_types: Vec<_> = args.iter().map(|(_, t)| compiler.convert_type(t)).collect();
                    let type_ = match type_ {
                        BasicMetadataTypeEnum::ArrayType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::FloatType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::IntType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::PointerType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::StructType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::VectorType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::MetadataType(v) => v.fn_type(&arg_types, false),
                    };
                    let func = compiler.module.add_function(&compiler.mangle_func_name(name), type_, Some(Linkage::Common));
                    compiler.func_map.insert(name.clone(), func);
                }

                SExpr::FuncExtern { name, ret_type, args, linked_to, .. } => {
                    let type_ = compiler.convert_type(ret_type);
                    let arg_types: Vec<_> = args.iter().map(|(_, t)| compiler.convert_type(t)).collect();
                    let type_ = match type_ {
                        BasicMetadataTypeEnum::ArrayType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::FloatType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::IntType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::PointerType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::StructType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::VectorType(v) => v.fn_type(&arg_types, false),
                        BasicMetadataTypeEnum::MetadataType(v) => v.fn_type(&arg_types, false),
                    };
                    let func = compiler.module.add_function(linked_to, type_, Some(Linkage::External));
                    compiler.func_map.insert(name.clone(), func);
                }

                _ => (),
            }
        }

        for sexpr in sexprs {
            match sexpr {
                SExpr::FuncDef { meta, ann, name, ret_type, args, expr } => {
                }

                _ => (),
            }
        }
    }
}