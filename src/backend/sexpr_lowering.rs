#![allow(unused)]

use std::collections::HashMap;

use crate::frontend::ast_lowering::{SExpr, Type as SExprType, LValue};

use super::ir::{Module, ModuleBuilder, Value, ToIntegerOperation, Operation, Type as IrType, Terminator, BasicBlockId, VariableId, FunctionId};

struct LowerHelperArgs {
    breaks: Vec<Vec<(BasicBlockId, Option<Value>)>>,
    var_map: Vec<HashMap<String, VariableId>>,
    func_map: HashMap<String, FunctionId>,
}

fn lower_helper(builder: &mut ModuleBuilder, sexpr: SExpr, args: &mut LowerHelperArgs) -> Option<Value> {
    let type_ = convert_type(&sexpr.meta().type_);
    match sexpr {
        SExpr::Int { meta, value } => builder.push_instruction(&type_, value.to_integer_operation()),

        SExpr::Float { meta, value } => todo!(),
        SExpr::Str { meta, value } => todo!(),

        SExpr::Symbol { meta, value } => {
            for scope in args.var_map.iter().rev() {
                if let Some(var) = scope.get(value) {
                    return builder.push_instruction(&type_, Operation::GetVar(*var));
                }
            }
            None
        }

        SExpr::List { meta, values } => todo!(),
        SExpr::Quote { meta, value } => todo!(),
        SExpr::Comma { meta, value } => todo!(),
        SExpr::Backtick { meta, value } => todo!(),
        SExpr::Splice { meta, value } => todo!(),

        SExpr::Seq { meta, values } => {
            let mut last = None;
            args.var_map.push(HashMap::new());
            for value in values {
                last = lower_helper(builder, value, args);
            }
            args.var_map.pop();
            last
        }

        SExpr::Cond { meta, values, elsy } => {
            let mut mapping = Vec::new();

            for (cond, then) in values {
                args.var_map.push(HashMap::new());
                let cond = lower_helper(builder, cond, args).unwrap();
                let current_block = builder.get_block().unwrap();
                let then_block = builder.push_block().unwrap();
                builder.switch_to_block(then_block);
                let then = lower_helper(builder, then, args);
                let last_block = builder.get_block().unwrap();
                mapping.push((last_block, then));
                let cont_block = builder.push_block().unwrap();
                builder.switch_to_block(current_block);
                builder.set_terminator(Terminator::Branch(cond, then_block, cont_block));
                builder.switch_to_block(cont_block);
                args.var_map.pop();
            }

            if let Some(elsy) = elsy {
                args.var_map.push(HashMap::new());
                let value = lower_helper(builder, *elsy, args);
                args.var_map.pop();
                mapping.push((builder.get_block().unwrap(), value))
            }

            let post_block = builder.push_block().unwrap();
            let mapping: Vec<_> = mapping.into_iter().filter_map(|(block, value)| {
                builder.switch_to_block(block);
                builder.set_terminator(Terminator::Jump(post_block));
                value.map(|v| (block, v))
            }).collect();

            builder.switch_to_block(post_block);
            if mapping.is_empty() {
                None
            } else {
                builder.push_instruction(&type_, Operation::Phi(mapping))
            }
        }

        SExpr::Loop { meta, value } => {
            args.breaks.push(Vec::new());
            args.var_map.push(HashMap::new());
            let block = builder.push_block().unwrap();
            builder.set_terminator(Terminator::Jump(block));
            builder.switch_to_block(block);
            lower_helper(builder, *value, args);
            builder.set_terminator(Terminator::Jump(block));

            args.var_map.pop();
            if let Some(mappings) = args.breaks.pop() {
                let final_block = builder.push_block().unwrap();
                let mappings: Vec<_> = mappings.into_iter().filter_map(|(block, value)| {
                    builder.switch_to_block(block);
                    builder.set_terminator(Terminator::Jump(final_block));
                    value.map(|v| (block, v))
                }).collect();
                builder.switch_to_block(final_block);
                if mappings.is_empty() {
                    None
                } else {
                    builder.push_instruction(&type_, Operation::Phi(mappings))
                }
            } else {
                unreachable!();
            }
        }

        SExpr::Break { meta, value } => {
            let value = value.and_then(|v| lower_helper(builder, *v, args));
            let block = builder.get_block().unwrap();

            if let Some(break_) = args.breaks.last_mut() {
                break_.push((block, value));
            }

            let new = builder.push_block().unwrap();
            builder.switch_to_block(new);
            None
        }

        SExpr::Nil { meta } => None,

        SExpr::Type { meta, value } => lower_helper(builder, *value, args),

        SExpr::FuncDef { meta, name, ret_type, args, expr } => unreachable!(),

        SExpr::FuncCall { meta, func, values } => {
            match *func {
                SExpr::Symbol { value: "+", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Add(values[0], values[1]))
                }

                SExpr::Symbol { value: "-", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Sub(values[0], values[1]))
                }

                SExpr::Symbol { value: "*", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Mul(values[0], values[1]))
                }

                SExpr::Symbol { value: "/", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Div(values[0], values[1]))
                }

                SExpr::Symbol { value: "%", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Mod(values[0], values[1]))
                }

                SExpr::Symbol { value: "<<", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Bsl(values[0], values[1]))
                }

                SExpr::Symbol { value: "%", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Mod(values[0], values[1]))
                }

                SExpr::Symbol { value: ">>", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Bsr(values[0], values[1]))
                }

                SExpr::Symbol { value: "==", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Eq(values[0], values[1]))
                }

                SExpr::Symbol { value: "!=", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Ne(values[0], values[1]))
                }

                SExpr::Symbol { value: "<", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Lt(values[0], values[1]))
                }

                SExpr::Symbol { value: "<=", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Le(values[0], values[1]))
                }

                SExpr::Symbol { value: ">", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Gt(values[0], values[1]))
                }

                SExpr::Symbol { value: ">=", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::Ge(values[0], values[1]))
                }

                SExpr::Symbol { value: "&", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::BitAnd(values[0], values[1]))
                }

                SExpr::Symbol { value: "|", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::BitOr(values[0], values[1]))
                }

                SExpr::Symbol { value: "^", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::BitXor(values[0], values[1]))
                }

                SExpr::Symbol { meta, value } => {
                    for scope in args.var_map.iter().rev() {
                        if let Some(var) = scope.get(value) {
                            let v = builder.push_instruction(&convert_type(&meta.type_), Operation::GetVar(*var)).unwrap();
                            let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                            return builder.push_instruction(&type_, Operation::CallIndirect(v, values))
                        }
                    }

                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    if let Some(func) = args.func_map.get(value) {
                        builder.push_instruction(&type_, Operation::Call(*func, values))
                    } else {
                        None
                    }
                }

                f => {
                    let f = lower_helper(builder, f, args).unwrap();
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(&type_, Operation::CallIndirect(f, values))
                }
            }
        }

        SExpr::FuncExtern { meta, name, ret_type, args, linked_to } => todo!(),
        SExpr::StructDef { meta, name, fields } => todo!(),

        SExpr::StructSet { meta, name, values } => todo!(),

        SExpr::Declare { meta, mutable, variable, value } => {
            if let Some(v) = lower_helper(builder, *value, args) {
                let var = builder.push_variable(variable, &convert_type(&meta.type_)).unwrap();
                args.var_map.last_mut().unwrap().insert(variable.to_owned(), var);
                builder.push_instruction(&IrType::Void, Operation::SetVar(var, v));
                builder.push_instruction(&type_, Operation::GetVar(var))
            } else {
                None
            }
        }

        SExpr::Assign { meta, lvalue: LValue::Symbol(variable), value } => {
            if let Some(v) = lower_helper(builder, *value, args) {
                for scope in args.var_map.iter().rev() {
                    if let Some(var) = scope.get(variable) {
                        builder.push_instruction(&IrType::Void, Operation::SetVar(*var, v));
                        return builder.push_instruction(&type_, Operation::GetVar(*var));
                    }
                }
            }
            None
        }

        SExpr::Assign { meta, lvalue, value } => todo!(),

        SExpr::Attribute { meta, top, attr } => todo!(),
        SExpr::SizeOf { meta, type_ } => todo!(),
        SExpr::Ref { meta, value } => todo!(),
        SExpr::Deref { meta, value } => todo!(),
    }
}

fn convert_type(type_: &SExprType) -> IrType {
    match type_ {
        SExprType::Int(signed, width) => IrType::Integer(*signed, *width),
        SExprType::F32 => todo!(),
        SExprType::F64 => todo!(),
        SExprType::Tuple(v) if v.is_empty() => IrType::Void,
        SExprType::Tuple(_) => todo!(),
        SExprType::Pointer(_, _) => todo!(),
        SExprType::Slice(_, _) => todo!(),
        SExprType::Struct(_, _) => todo!(),
        SExprType::Function(_, _) => todo!(),

        _ => unreachable!(),
    }
}

pub fn lower(sexprs: Vec<SExpr>) -> Module {
    let mut builder = ModuleBuilder::default()
        .with_name("uwu");
    let mut helper_args = LowerHelperArgs {
        breaks: Vec::new(),
        var_map: Vec::new(),
        func_map: HashMap::new(),
    };

    for sexpr in sexprs.iter() {
        if let SExpr::FuncDef { name, ret_type, args, .. } = sexpr {
            let args: Vec<_> = args.iter().map(|(n, t)| (*n, convert_type(t))).collect();
            let func = builder.new_function(name, &args, &convert_type(ret_type));
            helper_args.func_map.insert((*name).to_owned(), func);
        }
    }

    for sexpr in sexprs {
        match sexpr {
            SExpr::FuncDef { meta, name, ret_type, args, expr } => {
                let func = *helper_args.func_map.get(name).unwrap();
                builder.switch_to_function(func);
                let block = builder.push_block().unwrap();
                builder.switch_to_block(block);
                helper_args.var_map.push(args.into_iter().zip(builder.get_function_args(func).unwrap().into_iter()).map(|((n, _), v)| (n.to_owned(), v)).collect());
                let expr = lower_helper(&mut builder, *expr, &mut helper_args);
                helper_args.var_map.pop();
                if let Some(ret) = expr {
                    builder.set_terminator(Terminator::Return(ret));
                } else {
                    builder.set_terminator(Terminator::ReturnVoid);
                }
            }

            SExpr::FuncExtern { meta, name, ret_type, args, linked_to } => (),
            SExpr::StructDef { meta, name, fields } => (),
            _ => (),
        }
    }

    builder.build()
}

