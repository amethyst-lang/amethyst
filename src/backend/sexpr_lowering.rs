use std::collections::HashMap;

use inkwell::{context::Context, builder::Builder, passes::PassManager, values::{FunctionValue, BasicMetadataValueEnum, BasicValueEnum}, module::{Module, Linkage}, types::{FunctionType, BasicMetadataTypeEnum}};

use crate::frontend::ast_lowering::{SExpr, Type as SExprType};

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    func_map: HashMap<String, FunctionValue<'ctx>>,
    current_func: Option<FunctionValue<'ctx>>,
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
        if name == "main" {
            "main".to_string()
        } else {
            format!("_amy_{}", name)
        }
    }

    pub fn lower(context: &'ctx Context, sexprs: Vec<SExpr>) -> Self {
        let mut compiler = Compiler {
            context,
            builder: context.create_builder(),
            module: context.create_module("uwu"),
            func_map: HashMap::new(),
            current_func: None,
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
                    let func = compiler.module.add_function(&compiler.mangle_func_name(name), type_, None);
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
                SExpr::FuncDef { name, ret_type, expr, .. } => {
                    let func = *compiler.func_map.get(&name).unwrap();
                    let block = compiler.context.append_basic_block(func, "entry");
                    compiler.builder.position_at_end(block);
                    compiler.current_func = Some(func);
                    let v = compiler.lower_helper(*expr);
                    match ret_type {
                        SExprType::Tuple(v) if v.is_empty() => (),
                        _ => {
                            compiler.builder.build_return(Some(&v));
                        }
                    }
                    compiler.current_func = None;
                }

                _ => (),
            }
        }

        compiler.module.verify().expect("must not fail");
        compiler
    }

    fn lower_helper(&self, expr: SExpr) -> BasicValueEnum {
        match expr {
            SExpr::Int { meta, value } => match (self.convert_type(&meta.type_), meta.type_) {
                (BasicMetadataTypeEnum::IntType(t), SExprType::Int(signed, _)) => t.const_int(value, signed).into(),
                _ => unreachable!(),
            },

            SExpr::Float { meta, value } => match self.convert_type(&meta.type_) {
                BasicMetadataTypeEnum::FloatType(t) => t.const_float(value).into(),
                _ => unreachable!(),
            },

            SExpr::Str { meta, value } => todo!(),
            SExpr::Symbol { meta, value } => todo!(),
            SExpr::ModuleAccess { meta, value } => todo!(),
            SExpr::Tuple { meta, tuple } => todo!(),
            SExpr::Seq { meta, values } => todo!(),
            SExpr::Cond { meta, values, elsy } => todo!(),
            SExpr::Loop { meta, value } => todo!(),
            SExpr::Break { meta, value } => todo!(),
            SExpr::Nil { meta } => todo!(),
            SExpr::Type { meta, value } => todo!(),
            SExpr::FuncDef { meta, ann, name, ret_type, args, expr } => todo!(),
            SExpr::FuncCall { meta, func, values } => todo!(),
            SExpr::FuncExtern { meta, name, ret_type, args, linked_to } => todo!(),
            SExpr::StructDef { meta, ann, name, fields } => todo!(),
            SExpr::StructSet { meta, name, values } => todo!(),
            SExpr::Declare { meta, conditional, settings } => todo!(),
            SExpr::Assign { meta, var, value } => todo!(),
            SExpr::Attribute { meta, top, attr } => todo!(),
            SExpr::SliceGet { meta, top, index } => todo!(),
            SExpr::SizeOf { meta, type_ } => todo!(),
            SExpr::Ref { meta, value } => todo!(),
            SExpr::Deref { meta, value } => todo!(),
            SExpr::Import { meta, imports } => todo!(),
        }
    }
}