#![allow(unused)]

use crate::frontend::ast_lowering::SExpr;

use super::ir::{Module, ModuleBuilder, Value, ToIntegerOperation, Operation, Type, Terminator, BasicBlockId};

struct LowerHelperArgs {
    breaks: Vec<Vec<(BasicBlockId, Option<Value>)>>,
}

fn lower_helper(builder: &mut ModuleBuilder, sexpr: SExpr, args: &mut LowerHelperArgs) -> Option<Value> {
    match sexpr {
        SExpr::Int { meta, value } => builder.push_instruction(value.to_integer_operation()),
        SExpr::Float { meta, value } => todo!(),
        SExpr::Str { meta, value } => todo!(),
        SExpr::Symbol { meta, value } => todo!(),

        SExpr::List { meta, values } => todo!(),
        SExpr::Quote { meta, value } => todo!(),
        SExpr::Comma { meta, value } => todo!(),
        SExpr::Backtick { meta, value } => todo!(),
        SExpr::Splice { meta, value } => todo!(),

        SExpr::Seq { meta, values } => {
            let mut last = None;
            for value in values {
                last = lower_helper(builder, value, args);
            }
            last
        }

        SExpr::Cond { meta, values, elsy } => {
            let mut mapping = Vec::new();

            for (cond, then) in values {
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
            }

            if let Some(elsy) = elsy {
                let value = lower_helper(builder, *elsy, args);
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
                builder.push_instruction(Operation::Phi(mapping))
            }
        }

        SExpr::Loop { meta, value } => {
            args.breaks.push(Vec::new());
            let block = builder.push_block().unwrap();
            builder.set_terminator(Terminator::Jump(block));
            builder.switch_to_block(block);
            lower_helper(builder, *value, args);
            builder.set_terminator(Terminator::Jump(block));

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
                    builder.push_instruction(Operation::Phi(mappings))
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

        SExpr::FuncDef { meta, name, ret_type, args, expr } => todo!(),

        SExpr::FuncCall { meta, func, values } => {
            match *func {
                SExpr::Symbol { value: "+", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Add(values[0], values[1]))
                }

                SExpr::Symbol { value: "-", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Sub(values[0], values[1]))
                }

                SExpr::Symbol { value: "*", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Mul(values[0], values[1]))
                }

                SExpr::Symbol { value: "/", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Div(values[0], values[1]))
                }

                SExpr::Symbol { value: "%", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Mod(values[0], values[1]))
                }

                SExpr::Symbol { value: "<<", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Bsl(values[0], values[1]))
                }

                SExpr::Symbol { value: "%", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Mod(values[0], values[1]))
                }

                SExpr::Symbol { value: ">>", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Bsr(values[0], values[1]))
                }

                SExpr::Symbol { value: "==", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Eq(values[0], values[1]))
                }

                SExpr::Symbol { value: "!=", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Ne(values[0], values[1]))
                }

                SExpr::Symbol { value: "<", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Lt(values[0], values[1]))
                }

                SExpr::Symbol { value: "<=", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Le(values[0], values[1]))
                }

                SExpr::Symbol { value: ">", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Gt(values[0], values[1]))
                }

                SExpr::Symbol { value: ">=", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::Ge(values[0], values[1]))
                }

                SExpr::Symbol { value: "&", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::BitAnd(values[0], values[1]))
                }

                SExpr::Symbol { value: "|", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::BitOr(values[0], values[1]))
                }

                SExpr::Symbol { value: "^", .. } => {
                    let values: Vec<_> = values.into_iter().flat_map(|v| lower_helper(builder, v, args)).collect();
                    builder.push_instruction(Operation::BitXor(values[0], values[1]))
                }

                _ => todo!(),
            }
        }

        SExpr::FuncExtern { meta, name, ret_type, args, linked_to } => todo!(),
        SExpr::StructDef { meta, name, fields } => todo!(),

        SExpr::StructSet { meta, name, values } => todo!(),
        SExpr::Declare { meta, mutable, variable, value } => todo!(),
        SExpr::Assign { meta, lvalue, value } => todo!(),
        SExpr::Attribute { meta, top, attr } => todo!(),
        SExpr::SizeOf { meta, type_ } => todo!(),
        SExpr::Ref { meta, value } => todo!(),
        SExpr::Deref { meta, value } => todo!(),
    }
}

pub fn lower(sexprs: Vec<SExpr>) -> Module {
    let mut builder = ModuleBuilder::default()
        .with_name("uwu");
    let mut helper_args = LowerHelperArgs {
        breaks: Vec::new(),
    };

    for sexpr in sexprs {
        match sexpr {
            SExpr::FuncDef { meta, name, ret_type, args, expr } => {
                let func = builder.new_function(name, &[], &Type::Integer(true, 32));
                builder.switch_to_function(func);
                let block = builder.push_block().unwrap();
                builder.switch_to_block(block);
                let expr = lower_helper(&mut builder, *expr, &mut helper_args);
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
