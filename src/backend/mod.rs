use std::collections::HashMap;
use std::fmt::Write;

use cranelift::prelude::{*, isa::CallConv, codegen::Context};
use cranelift_module::{DataContext, Module, Linkage};
use cranelift_object::{ObjectModule, ObjectBuilder};
use target_lexicon::triple;

use crate::frontend::{ast_lowering::{SExpr, Type as SExprType, LValue}, correctness::Struct};

pub struct Generator {
    builder_context: FunctionBuilderContext,
    ctx: Context,
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
    pub fn compile(&mut self, sexprs: Vec<SExpr<'_>>, structs: &HashMap<&str, Struct>) {
        self.translate(&sexprs, structs);
    }

    fn translate(&mut self, sexprs: &[SExpr<'_>], structs: &HashMap<&str, Struct>) {
        for sexpr in sexprs {
            if let SExpr::FuncDef { name, ret_type, args, expr, .. } = sexpr {
                if args.iter().any(|(_, v)| v.has_generic()) || ret_type.has_generic() {
                    continue;
                }

                for (_, typ) in args {
                    for t in Self::convert_type_to_type(typ, structs) {
                        self.ctx.func.signature.params.push(AbiParam::new(t));
                    }
                }

                for t in Self::convert_type_to_type(ret_type, structs) {
                    self.ctx.func.signature.returns.push(AbiParam::new(t));
                }

                let mut func = self.ctx.func.clone();
                let mut builder = FunctionBuilder::new(&mut func, &mut self.builder_context);
                let entry_block = builder.create_block();
                builder.append_block_params_for_function_params(entry_block);
                builder.switch_to_block(entry_block);
                let mut var_map = vec![HashMap::new()];
                let mut var_index = 0;
                for (i, (name, typ)) in args.iter().enumerate() {
                    let mut vars = vec![];
                    for t in Self::convert_type_to_type(typ, structs) {
                        let var = Variable::new(var_index);
                        var_index += 1;
                        builder.declare_var(var, t);
                        builder.def_var(var, builder.block_params(entry_block)[i]);
                        vars.push(var);
                    }
                    var_map[0].insert(*name, vars);
                }

                // TODO: returning strings
                let ret_value = Self::translate_expr(&**expr, &mut builder, &mut var_map, &mut var_index, None, &mut self.module, &mut self.ctx, &mut self.data_ctx, structs);
                builder.ins().return_(&ret_value);
                builder.seal_all_blocks();
                println!("{}", builder.func);
                builder.finalize();
                self.ctx.func = func;

                let id = self.module.declare_function(&Self::mangle_func(name, args.iter().map(|(_, v)| v), ret_type), Linkage::Export, &self.ctx.func.signature).unwrap();
                self.module.define_function(id, &mut self.ctx).unwrap();
                self.module.clear_context(&mut self.ctx);
                var_map.clear();
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn translate_expr<'a>(sexpr: &SExpr<'a>, builder: &mut FunctionBuilder, var_map: &mut Vec<HashMap<&'a str, Vec<Variable>>>, var_index: &mut usize, break_block: Option<Block>, module: &mut ObjectModule, ctx: &mut Context, data_ctx: &mut DataContext, structs: &HashMap<&str, Struct>) -> Vec<Value> {
        #[allow(unused)]
        match sexpr {
            SExpr::Int { meta, value } => {
                if matches!(meta.type_, SExprType::Int(_, 1)) {
                    vec![builder.ins().bconst(Self::convert_type_to_type_ref(&meta.type_, structs), *value != 0)]
                } else {
                    vec![builder.ins().iconst(Self::convert_type_to_type_ref(&meta.type_, structs), *value as i64)]
                }
            }

            SExpr::Symbol { meta, value } => {
                for scope in var_map.iter().rev() {
                    if let Some(var) = scope.get(value) {
                        return var.iter().map(|&v| builder.use_var(v)).collect();
                    }
                }

                let mut sig = Signature::new(CallConv::SystemV);
                if let SExprType::Function(a, r) = &meta.type_ {
                    sig.params.extend(a.iter().map(|v| Self::convert_type_to_type_ref(v, structs)).map(AbiParam::new));
                    sig.returns.push(AbiParam::new(Self::convert_type_to_type_ref(&**r, structs)));
                    let func = module.declare_function(&Self::mangle_func(value, a.iter(), &**r), Linkage::Import, &sig).unwrap();
                    let func = module.declare_func_in_func(func, builder.func);

                    vec![builder.ins().func_addr(Self::convert_type_to_type_ref(&meta.type_, structs), func)]
                } else {
                    unreachable!("func must have type func");
                }
            }

            SExpr::Float { meta, value } => {
                if meta.type_ == SExprType::F32 {
                    vec![builder.ins().f32const(*value as f32)]
                } else {
                    vec![builder.ins().f64const(*value)]
                }
            }

            SExpr::Str { meta, value } => {
                let name = format!("{}", var_index);
                *var_index += 1;
                data_ctx.define(Box::from(value.as_bytes()));
                let sym = module.declare_data(&name, Linkage::Hidden, false, false).unwrap();
                module.define_data(sym, data_ctx).unwrap();
                data_ctx.clear();
                let val = module.declare_data_in_func(sym, builder.func);
                let size = builder.ins().iconst(Self::convert_type_to_type_ref(&SExprType::Int(false, 64), structs), value.len() as i64);
                let reference = builder.ins().global_value(Self::convert_type_to_type_ref(&meta.type_, structs), val);
                vec![size, reference]
            }

            SExpr::Seq { values, .. } => {
                var_map.push(HashMap::new());
                for value in values[..values.len() - 1].iter() {
                    Self::translate_expr(value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                }

                let v = Self::translate_expr(values.last().unwrap(), builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                var_map.pop();
                v
            }

            SExpr::Cond { meta, values, elsy } => {
                let mut conds = vec![builder.current_block().unwrap()];
                let mut thens = vec![];
                for _ in values {
                    conds.push(builder.create_block());
                    thens.push(builder.create_block());
                }

                let last = builder.create_block();

                for t in Self::convert_type_to_type(&meta.type_, structs) {
                    builder.append_block_param(last, t);
                }

                for (i, (cond, body)) in values.iter().enumerate() {
                    var_map.push(HashMap::new());
                    let cond = Self::translate_expr(cond, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs)[0];
                    builder.ins().brz(cond, conds[i + 1], &[]);
                    builder.ins().jump(thens[i], &[]);
                    builder.switch_to_block(thens[i]);
                    let then = Self::translate_expr(body, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                    var_map.pop();
                    builder.ins().jump(last, &then);
                    builder.switch_to_block(conds[i + 1]);
                }

                let elsy = if let Some(elsy) = elsy {
                    var_map.push(HashMap::new());
                    let v = Self::translate_expr(&**elsy, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                    var_map.pop();
                    v
                } else {
                    vec![]
                };
                builder.ins().jump(last, &elsy);

                builder.switch_to_block(last);
                builder.block_params(last).to_vec()
            }

            SExpr::Loop { meta, value } => {
                let loop_block = builder.create_block();
                let break_block = builder.create_block();

                for t in Self::convert_type_to_type(&meta.type_, structs) {
                    builder.append_block_param(break_block, t);
                }

                builder.ins().jump(loop_block, &[]);
                builder.switch_to_block(loop_block);
                var_map.push(HashMap::new());
                Self::translate_expr(&**value, builder, var_map, var_index, Some(break_block), module, ctx, data_ctx, structs);
                var_map.pop();
                builder.ins().jump(loop_block, &[]);
                builder.switch_to_block(break_block);
                builder.block_params(break_block).to_vec()
            }

            SExpr::Break { meta, value } => {
                let value = if let Some(value) = value {
                    Self::translate_expr(&**value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs)
                } else {
                    vec![]
                };

                builder.ins().jump(break_block.unwrap(), &value);
                let new = builder.create_block();
                builder.switch_to_block(new);
                vec![]
            }

            SExpr::Nil { meta } => vec![],

            SExpr::Type { value, .. } => Self::translate_expr(value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs),

            SExpr::FuncCall { meta, func, values } => {
                let args = values;
                let values: Vec<_> = args.iter().map(|v| Self::translate_expr(v, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs)).collect();
                match &**func {
                    SExpr::Symbol { value: "+", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            vec![builder.ins().fadd(values[0][0], values[1][0])]
                        } else {
                            vec![builder.ins().iadd(values[0][0], values[1][0])]
                        }
                    }

                    SExpr::Symbol { value: "-", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            vec![builder.ins().fsub(values[0][0], values[1][0])]
                        } else {
                            vec![builder.ins().isub(values[0][0], values[1][0])]
                        }
                    }

                    SExpr::Symbol { value: "*", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            vec![builder.ins().fmul(values[0][0], values[1][0])]
                        } else {
                            vec![builder.ins().imul(values[0][0], values[1][0])]
                        }
                    }

                    SExpr::Symbol { value: "/", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            vec![builder.ins().fdiv(values[0][0], values[1][0])]
                        } else if matches!(meta.type_, SExprType::Int(true, _)) {
                            vec![builder.ins().sdiv(values[0][0], values[1][0])]
                        } else {
                            vec![builder.ins().udiv(values[0][0], values[1][0])]
                        }
                    }

                    SExpr::Symbol { value: "%", .. } => {
                        if matches!(meta.type_, SExprType::Int(true, _)) {
                            vec![builder.ins().srem(values[0][0], values[1][0])]
                        } else {
                            vec![builder.ins().urem(values[0][0], values[1][0])]
                        }
                    }

                    SExpr::Symbol { value: "syscall", .. } => {
                        let mut syscall_sig = Signature::new(CallConv::SystemV);
                        syscall_sig.params.extend([AbiParam::new(Self::convert_type_to_type_ref(&SExprType::Int(false, 64), structs)); 7]);
                        syscall_sig.returns.push(AbiParam::new(Self::convert_type_to_type_ref(&SExprType::Int(false, 64), structs)));
                        let syscall = module.declare_function("syscall_", Linkage::Import, &syscall_sig).unwrap();
                        let syscall = module.declare_func_in_func(syscall, builder.func);

                        let call = builder.ins().call(syscall, &values.into_iter().flatten().collect::<Vec<_>>());
                        builder.inst_results(call).to_vec()
                    }

                    SExpr::Symbol { value: "cast", .. } => {
                        match (&meta.type_, &args[0].meta().type_) {
                            (SExprType::Int(_, width1), SExprType::Int(_, width2)) => todo!(),
                            (SExprType::Pointer(_, _), SExprType::Int(_, _)) => vec![values[0][0]],
                            (SExprType::Int(_, _), SExprType::Pointer(_, _)) => vec![values[0][0]],
                            _ => todo!("casting into {:?} from {:?}", meta.type_, args[0].meta().type_),
                        }
                    }

                    SExpr::Symbol { value: "alloca", .. } => {
                        match &meta.type_ {
                            SExprType::Pointer(_, v) => {
                                let slot = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, Self::size_of(&**v, structs)));
                                vec![builder.ins().stack_addr(Self::convert_type_to_type_ref(&meta.type_, structs), slot, 0)]
                            }

                            SExprType::Slice(_, v) => {
                                const SIZE: u32 = 512;
                                let len = values[0][0];
                                let size = builder.ins().imul_imm(len, Self::size_of(&**v, structs) as i64);
                                let flags = builder.ins().icmp_imm(IntCC::UnsignedLessThanOrEqual, size, SIZE as i64);
                                builder.ins().trapz(flags, TrapCode::StackOverflow);
                                let slot = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, SIZE));
                                let reference = builder.ins().stack_addr(Self::convert_type_to_type_ref(&meta.type_, structs), slot, 0);
                                let slot = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 16));
                                vec![len, reference]
                            }

                            _ => unreachable!(),
                        }
                    }

                    SExpr::Symbol { value: "get", .. } => {
                        let v = values[0][1];
                        let i = values[1][0];
                        let v = builder.ins().load(Self::convert_type_to_type_ref(&SExprType::Pointer(true, Box::new(SExprType::F32)), structs), MemFlags::new(), v, 8);
                        let i = builder.ins().imul_imm(i, Self::size_of(&meta.type_, structs) as i64);
                        let ptr = builder.ins().iadd(v, i);
                        vec![builder.ins().load(Self::convert_type_to_type_ref(&meta.type_, structs), MemFlags::new(), ptr, 0)]
                    }

                    SExpr::Symbol { value: "&", .. } => vec![builder.ins().band(values[0][0], values[1][0])],
                    SExpr::Symbol { value: "|", .. } => vec![builder.ins().bor(values[0][0], values[1][0])],
                    SExpr::Symbol { value: "^", .. } => vec![builder.ins().bxor(values[0][0], values[1][0])],
                    SExpr::Symbol { value: "<<", .. } => vec![builder.ins().ishl(values[0][0], values[1][0])],
                    SExpr::Symbol { value: ">>", .. } => vec![builder.ins().ushr(values[0][0], values[1][0])],

                    SExpr::Symbol { value: "<", .. } => {
                        if meta.type_ == SExprType::F32 || meta.type_ == SExprType::F64 {
                            vec![builder.ins().fcmp(FloatCC::LessThan, values[0][0], values[1][0])]
                        } else if matches!(meta.type_, SExprType::Int(true, _)) {
                            vec![builder.ins().icmp(IntCC::SignedLessThan, values[0][0], values[1][0])]
                        } else {
                            vec![builder.ins().icmp(IntCC::UnsignedLessThan, values[0][0], values[1][0])]
                        }
                    }

                    SExpr::Symbol { meta, value } => {
                        let mut sig = Signature::new(CallConv::SystemV);
                        if let SExprType::Function(a, r) = &meta.type_ {
                            sig.params.extend(a.iter().map(|v| Self::convert_type_to_type_ref(v, structs)).map(AbiParam::new));
                            sig.returns.push(AbiParam::new(Self::convert_type_to_type_ref(&**r, structs)));
                        }

                        for scope in var_map.iter().rev() {
                            if let Some(func) = scope.get(value) {
                                let sig = builder.import_signature(sig);
                                let func = builder.use_var(func[0]);
                                let call = builder.ins().call_indirect(sig, func, &values.into_iter().flatten().collect::<Vec<_>>());
                                return builder.inst_results(call).to_vec();
                            }
                        }

                        if let SExprType::Function(a, r) = &meta.type_ {
                            let func = module.declare_function(&Self::mangle_func(value, a.iter(), &**r), Linkage::Import, &sig).unwrap();
                            let func = module.declare_func_in_func(func, builder.func);

                            let call = builder.ins().call(func, &values.into_iter().flatten().collect::<Vec<_>>());
                            builder.inst_results(call).to_vec()
                        } else {
                            unreachable!("must be func");
                        }
                    }

                    _ => todo!("{:?} can't be used as a function yet", func),
                }
            }

            SExpr::StructSet { meta, name, values } => {
                if let Some(struct_) = structs.get(name) {
                    let mut result = vec![vec![]; struct_.fields.len()];
                    let fields: HashMap<_, _> = struct_.fields.iter().enumerate().map(|(i, (name, _))| (*name, i)).collect();

                    for (field, value) in values {
                        let value = Self::translate_expr(value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                        result[*fields.get(field).unwrap()] = value;
                    }

                    result.into_iter().flatten().collect()
                } else {
                    unreachable!();
                }
            }

            SExpr::Declare { meta, mutable, variable, value } => {
                let val = Self::translate_expr(&**value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                let mut vars = vec![];
                let mut result = vec![];
                for (val, typ) in val.into_iter().zip(Self::convert_type_to_type(&meta.type_, structs)) {
                    let var = Variable::new(*var_index);
                    *var_index += 1;
                    builder.declare_var(var, typ);
                    builder.def_var(var, val);
                    result.push(builder.use_var(var));
                    vars.push(var);
                }
                var_map.last_mut().unwrap().insert(*variable, vars);
                result
            }

            SExpr::Assign { meta, lvalue: LValue::Symbol(variable), value } => {
                for scope in var_map.iter().rev() {
                    if let Some(v) = scope.get(variable) {
                        let mut result = vec![];
                        let vars = v.clone();
                        let val = Self::translate_expr(&**value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                        for (&var, val) in vars.iter().zip(val) {
                            builder.def_var(var, val);
                            result.push(builder.use_var(var));
                        }
                        return result;
                    }
                }

                unreachable!();
            }

            SExpr::Assign { meta, lvalue, value } => {
                let value = Self::translate_expr(&**value, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                let lvalue = Self::get_pointer(&meta.type_, lvalue, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                //builder.ins().store(MemFlags::new(), value, lvalue, 0);
                value
            }

            SExpr::Attribute { meta, top, attrs } => {
                let val = Self::translate_expr(top, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                match &top.meta().type_ {
                    /*
                    Slices are of the following form:
                    struct Slice<T> {
                        len: u64,
                        ptr: *const T,
                    }
                    ie:
                    {
                        len: 00..063
                        ptr: 64..127
                    }
                    */
                    SExprType::Slice(_, _) => {
                        match attrs[0] {
                            "len" | "cap" => {
                                vec![val[0]]
                            }

                            "ptr" => {
                                vec![val[1]]
                            }

                            _ => unreachable!(),
                        }
                    }

                    SExprType::Struct(name, generics) => {
                        let struct_ = structs.get(name).unwrap();
                        let (mut i, (_, mut t)) = struct_.fields.iter().cloned().enumerate().find(|(_, (v, _))| *v == attrs[0]).unwrap();
                        let mut v = &val[i..Self::convert_type_to_type(&t, structs).len()];
                        for attr in attrs.iter().skip(1) {
                            if let SExprType::Struct(name, _) = t {
                                let (j, (_, u)) = struct_.fields.iter().enumerate().find(|(_, (v, _))| v == attr).unwrap();
                                i += j;
                                t = u.clone();
                                v = &val[i..Self::convert_type_to_type(&t, structs).len()];
                            } else if let SExprType::Slice(_, _) = t {
                                match attrs[0] {
                                    "len" | "cap" => {
                                        return vec![val[i]];
                                    }

                                    "ptr" => {
                                        return vec![val[i + 1]];
                                    }

                                    _ => unreachable!(),
                                }
                            } else {
                                unreachable!();
                            }
                        }

                        v.to_vec()
                    }

                    _ => unreachable!(),
                }
            }

            _ => todo!(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn get_pointer<'a>(type_: &SExprType, lvalue: &LValue<'a>, builder: &mut FunctionBuilder, var_map: &mut Vec<HashMap<&'a str, Vec<Variable>>>, var_index: &mut usize, break_block: Option<Block>, module: &mut ObjectModule, ctx: &mut Context, data_ctx: &mut DataContext, structs: &HashMap<&str, Struct>) -> Vec<Value> {
        /*
        match lvalue {
            LValue::Symbol(v) => {
                for scope in var_map.iter().rev() {
                    if let Some(var) = scope.get(v) {
                        return var.iter().map(|&v| builder.use_var(v)).collect();
                    }
                }

                unreachable!("variable was typechecked");
            }

            LValue::Attribute(_v, _attrs) => todo!(),

            LValue::Deref(v) => {
                let parent_type = SExprType::Pointer(true, Box::new(type_.clone()));
                let ret = Self::get_pointer(&parent_type, v, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);

                if let LValue::Symbol(_) = **v {
                    ret
                } else {
                    let t = Self::convert_type_to_type(type_, structs);
                    let offsets = Self::offsets_and_sizes_of(type_, structs);
                    ret.into_iter()
                        .zip(t)
                        .zip(offsets)
                        .map(|((v, t), (offset, _))| builder.ins().load(t, MemFlags::new(), v, offset))
                        .collect()
                }
            }

            LValue::Get(v, i) => {
                let parent_type = SExprType::Slice(true, Box::new(type_.clone()));
                let i = Self::translate_expr(&**i, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs)[0];

                let offset = builder.ins().imul_imm(i, Self::size_of(type_, structs) as i64);
                let ptr = Self::get_pointer(child_type, v, builder, var_map, var_index, break_block, module, ctx, data_ctx, structs);
                let ptr = if let LValue::Symbol(_) = **v {
                    ptr
                } else {
                    builder.ins().load(Self::convert_type_to_type_ref(&SExprType::Pointer(true, Box::new(SExprType::F32)), structs), MemFlags::new(), ptr, 0)
                };
                let ptr = builder.ins().load(Self::convert_type_to_type_ref(&SExprType::Pointer(true, Box::new(SExprType::F32)), structs), MemFlags::new(), ptr, 8);
                builder.ins().iadd(ptr, offset)
            }
        }
        */
        vec![]
    }

    fn convert_type_to_type_helper(t: &SExprType, structs: &HashMap<&str, Struct>, map: &HashMap<&str, Vec<Type>>) -> Vec<Type> {
        match t {
            SExprType::Int(_, width) if *width == 1 => vec![types::B1],
            SExprType::Int(_, width) if *width == 8 => vec![types::I8],
            SExprType::Int(_, width) if *width == 16 => vec![types::I16],
            SExprType::Int(_, width) if *width == 32 => vec![types::I32],
            SExprType::Int(_, width) if *width == 64 => vec![types::I64],
            SExprType::F32 => vec![types::F32],
            SExprType::F64 => vec![types::F64],

            SExprType::Tuple(v) if v.is_empty() => vec![types::B1],

            SExprType::Pointer(_, _) => vec![types::I64],
            SExprType::Slice(_, _) => vec![types::I64, types::I64],

            SExprType::Generic(g) => {
                map.get(g).unwrap().clone()
            }

            SExprType::Struct(name, v) => {
                let struct_ = structs.get(name).unwrap();
                let map = struct_.generics.iter().zip(v.iter()).map(|(a, b)| {
                    if let SExprType::Generic(v) = a {
                        (*v, Self::convert_type_to_type_helper(b, structs, map))
                    } else {
                        unreachable!();
                    }
                }).collect();
                let mut fields = vec![];
                for (_, field) in struct_.fields.iter() {
                    fields.extend(Self::convert_type_to_type_helper(field, structs, &map));
                }

                fields
            }

            SExprType::Function(_, _) => vec![types::I64],

            _ => unreachable!(),
        }
    }

    fn convert_type_to_type(t: &SExprType, structs: &HashMap<&str, Struct>) -> Vec<Type> {
        let generics_map = HashMap::new();
        Self::convert_type_to_type_helper(t, structs, &generics_map)
    }

    fn convert_type_to_type_ref(t: &SExprType, structs: &HashMap<&str, Struct>) -> Type {
        let mut v = Self::convert_type_to_type(t, structs);

        if v.len() == 1 {
            v.remove(0)
        } else {
            types::I64
        }
    }

    fn size_of(t: &SExprType, structs: &HashMap<&str, Struct>) -> u32 {
        let v = Self::convert_type_to_type(t, structs);
        let mut size = 0isize;

        for v in v {
            if v == types::B1 || v == types::I8 {
                size += 1;
            } else if v == types::I16 {
                size += (2 - size).rem_euclid(2) + 2;
            } else if v == types::I32 {
                size += (4 - size).rem_euclid(4) + 4;
            } else if v == types::I64 {
                size += (8 - size).rem_euclid(8) + 8;
            } else {
                unreachable!();
            }
        }

        size as u32
    }

    fn offsets_and_sizes_of(t: &SExprType, structs: &HashMap<&str, Struct>) -> Vec<(i32, usize)> {
        let v = Self::convert_type_to_type(t, structs);
        let mut result = vec![];
        let mut offset = 0i32;

        for v in v {
            if v == types::B1 || v == types::I8 {
                result.push((offset, 1));
                offset += 1;
            } else if v == types::I16 {
                offset += (2 - offset).rem_euclid(2);
                result.push((offset, 2));
                offset += 2;
            } else if v == types::I32 {
                offset += (4 - offset).rem_euclid(4);
                result.push((offset, 4));
                offset += 4;
            } else if v == types::I64 {
                offset += (8 - offset).rem_euclid(8);
                result.push((offset, 8));
                offset += 8;
            } else {
                unreachable!();
            }
        }

        result
    }

    fn mangle_type(mangled: &mut String, t: &SExprType) {
        match t {
            SExprType::Int(false, width) => write!(mangled, "u{}", width).unwrap(),
            SExprType::Int(true, width) => write!(mangled, "i{}", width).unwrap(),
            SExprType::F32 => mangled.push('f'),
            SExprType::F64 => mangled.push('F'),
            SExprType::Tuple(v) if v.is_empty() => mangled.push('N'),
            SExprType::Pointer(false, v) => {
                mangled.push('p');
                Self::mangle_type(mangled, &**v);
            }
            SExprType::Pointer(true, v) => {
                mangled.push('P');
                Self::mangle_type(mangled, &**v);
            }
            SExprType::Slice(false, v) => {
                mangled.push('s');
                Self::mangle_type(mangled, &**v);
            }
            SExprType::Slice(true, v) => {
                mangled.push('S');
                Self::mangle_type(mangled, &**v);
            }
            SExprType::Struct(_, _) => todo!(),
            SExprType::Function(a, r) => {
                mangled.push('U');
                for (i, a) in a.iter().enumerate() {
                    if i != 0 {
                        mangled.push(',');
                    }
                    Self::mangle_type(mangled, a);
                }
                mangled.push(':');
                Self::mangle_type(mangled, &**r);
            }

            _ => unreachable!(),
        }
    }

    fn mangle_func<'a, 'b>(name: &str, args: impl Iterator<Item=&'a SExprType<'b>>, ret: &SExprType) -> String
        where 'b: 'a
    {
        if name == "main" {
            return String::from("main");
        }

        let mut mangled = String::new();
        write!(mangled, "amy_{}@", name).unwrap();
        for (i, arg) in args.enumerate() {
            if i != 0 {
                mangled.push(',');
            }
            Self::mangle_type(&mut mangled, arg);
        }
        mangled.push(':');
        Self::mangle_type(&mut mangled, ret);
        mangled
    }

    pub fn emit_object(self) -> Vec<u8> {
        self.module.finish().emit().unwrap()
    }
}
