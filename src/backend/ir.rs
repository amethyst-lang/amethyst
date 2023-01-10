use std::{fmt::Display, collections::{HashSet, HashMap}};

use super::arch::{Instr, InstructionSelector, VCode, VCodeGenerator};

#[derive(Default)]
pub struct Module {
    name: String,
    functions: Vec<Function>,
}

impl Module {
    pub fn lower_to_vcode<I, S>(self) -> VCode<I>
    where
        S: InstructionSelector<Instruction = I>,
        I: Instr,
    {
        let mut gen = VCodeGenerator::<I, S>::new_module(&self.name);
        let mut selector = S::default();

        for (i, function) in self.functions.iter().enumerate() {
            gen.add_function(&function.name, FunctionId(i));
        }

        for (f, func) in self.functions.into_iter().enumerate() {
            gen.switch_to_function(FunctionId(f));
            for (i, block) in func.blocks.iter().enumerate() {
                if block.deleted {
                    continue;
                }

                gen.push_label(BasicBlockId(FunctionId(f), i));
            }

            for (i, block) in func.blocks.into_iter().enumerate() {
                if block.deleted {
                    continue;
                }

                gen.switch_to_label(BasicBlockId(FunctionId(f), i));
                for instr in block.instructions {
                    selector.select_instr(&mut gen, instr.yielded, instr.type_, instr.operation);
                }
                selector.select_term(&mut gen, block.terminator);
            }
        }

        gen.build(selector)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "/* module {} */", self.name)?;
        for (i, func) in self.functions.iter().enumerate() {
            writeln!(f, "\n\n@{}: {}", i, func)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub enum Type {
    Void,
    Integer(bool, u8),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Integer(signed, width) => {
                write!(f, "{}{}", if *signed { "i" } else { "u" }, width)
            }
        }
    }
}

struct Function {
    name: String,
    arg_types: Vec<Type>,
    ret_type: Type,
    variables: Vec<Variable>,
    blocks: Vec<BasicBlock>,
    value_index: usize,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {} @{}(", self.ret_type, self.name)?;
        let mut first = true;
        for arg_type in self.arg_types.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg_type)?;
        }
        writeln!(f, ") {{")?;

        for (i, var) in self.variables.iter().enumerate() {
            writeln!(f, "    #{} : {} // {}", i, var.type_, var.name)?;
        }

        for (i, block) in self.blocks.iter().enumerate() {
            if !block.deleted {
                write!(f, "{}: // preds =", i)?;
                for pred in block.predecessors.iter() {
                    write!(f, " {}", pred)?;
                }
                write!(f, "\n{}", block)?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct FunctionId(usize);

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

struct Variable {
    name: String,
    type_: Type,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct VariableId(usize);

impl Display for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

struct BasicBlock {
    deleted: bool,
    predecessors: HashSet<BasicBlockId>,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in self.instructions.iter() {
            writeln!(f, "    {}", instruction)?;
        }
        writeln!(f, "    {}", self.terminator)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct BasicBlockId(FunctionId, usize);

impl Display for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.1)
    }
}

struct Instruction {
    yielded: Option<Value>,
    type_: Type,
    operation: Operation,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(yielded) = &self.yielded {
            write!(f, "{} = ", yielded)?;
        }

        write!(f, "{} {}", self.type_, self.operation)
    }
}

pub trait ToIntegerOperation {
    fn to_integer_operation(self) -> Operation;
}

impl ToIntegerOperation for i8 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for u8 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for i16 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for u16 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for i32 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for u32 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for i64 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for u64 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for i128 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

impl ToIntegerOperation for u128 {
    fn to_integer_operation(self) -> Operation {
        Operation::Integer(true, self.to_le_bytes().to_vec())
    }
}

pub enum Operation {
    Identity(Value),

    Integer(bool, Vec<u8>),

    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Mod(Value, Value),
    Bsl(Value, Value),
    Bsr(Value, Value),
    Eq(Value, Value),
    Ne(Value, Value),
    Lt(Value, Value),
    Le(Value, Value),
    Gt(Value, Value),
    Ge(Value, Value),
    BitAnd(Value, Value),
    BitOr(Value, Value),
    BitXor(Value, Value),

    Phi(Vec<(BasicBlockId, Value)>),

    GetVar(VariableId),
    SetVar(VariableId, Value),

    Call(FunctionId, Vec<Value>),
    CallIndirect(Value, Vec<Value>),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Identity(val) => write!(f, "id {}", val),

            Operation::Integer(signed, val) => {
                if *signed {
                    write!(f, "iconst ")?;
                } else {
                    write!(f, "uconst ")?;
                }

                if val.is_empty() {
                    write!(f, "0")
                } else {
                    for byte in val.iter().rev() {
                        write!(f, "{:02x}", byte)?;
                    }
                    Ok(())
                }
            }

            Operation::Add(a, b) => write!(f, "addi {}, {}", a, b),
            Operation::Sub(a, b) => write!(f, "subi {}, {}", a, b),
            Operation::Mul(a, b) => write!(f, "muli {}, {}", a, b),
            Operation::Div(a, b) => write!(f, "divi {}, {}", a, b),
            Operation::Mod(a, b) => write!(f, "mod {}, {}", a, b),
            Operation::Bsl(a, b) => write!(f, "shiftl {}, {}", a, b),
            Operation::Bsr(a, b) => write!(f, "shiftr {}, {}", a, b),
            Operation::Eq(a, b) => write!(f, "eqi {}, {}", a, b),
            Operation::Ne(a, b) => write!(f, "neqi {}, {}", a, b),
            Operation::Lt(a, b) => write!(f, "lti {}, {}", a, b),
            Operation::Le(a, b) => write!(f, "leqi {}, {}", a, b),
            Operation::Gt(a, b) => write!(f, "gti {}, {}", a, b),
            Operation::Ge(a, b) => write!(f, "geqi {}, {}", a, b),
            Operation::BitAnd(a, b) => write!(f, "andi {}, {}", a, b),
            Operation::BitOr(a, b) => write!(f, "ori {}, {}", a, b),
            Operation::BitXor(a, b) => write!(f, "xori {}, {}", a, b),

            Operation::Phi(maps) => {
                write!(f, "phi ")?;
                let mut first = true;
                for (block, value) in maps {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{} => {}", block, value)?;
                }
                Ok(())
            }

            Operation::GetVar(var) => write!(f, "get {}", var),
            Operation::SetVar(var, val) => write!(f, "set {}, {}", var, val),

            Operation::Call(func, args) => {
                write!(f, "call {}(", func)?;
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }

            Operation::CallIndirect(func, args) => {
                write!(f, "icall {}(", func)?;
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub enum Terminator {
    NoTerminator,
    ReturnVoid,
    Return(Value),
    Jump(BasicBlockId),
    Branch(Value, BasicBlockId, BasicBlockId),
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::NoTerminator => write!(f, "noterm"),
            Terminator::ReturnVoid => write!(f, "ret void"),
            Terminator::Return(v) => write!(f, "ret {}", v),
            Terminator::Jump(b) => write!(f, "jump {}", b),
            Terminator::Branch(c, t, e) => write!(f, "branch {}, {}, {}", c, t, e),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Value(usize);

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Default)]
pub struct ModuleBuilder {
    internal: Module,
    current_function: Option<usize>,
    current_block: Option<usize>,
}

impl ModuleBuilder {
    pub fn with_name(mut self, name: &str) -> Self {
        self.internal.name = name.to_owned();
        self
    }

    // Automatically does some dead code elimination, lowers variables to phi operations, and
    // checks for malformed IR.
    // TODO: return Result<Module, MalformedIrError> on malformed IR instead of panicking.
    pub fn build(mut self) -> Module {
        for (f, func) in self.internal.functions.iter_mut().enumerate() {
            let mut blocks_prec = vec![HashSet::new(); func.blocks.len()];
            for (i, block) in func.blocks.iter().enumerate() {
                match &block.terminator {
                    Terminator::NoTerminator => (),
                    Terminator::ReturnVoid => (),
                    Terminator::Return(_) => (),

                    Terminator::Jump(next) => {
                        let this = BasicBlockId(FunctionId(f), i);
                        blocks_prec[next.1].insert(this);
                    }

                    Terminator::Branch(_, on_true, on_false) => {
                        let this = BasicBlockId(FunctionId(f), i);
                        blocks_prec[on_true.1].insert(this);
                        blocks_prec[on_false.1].insert(this);
                    }
                }
            }

            for (block, prec) in func.blocks.iter_mut().zip(blocks_prec.into_iter()) {
                block.predecessors = prec;
            }

            let mut removed = Vec::new();
            while {
                removed.clear();

                for (i, block) in func.blocks.iter_mut().enumerate() {
                    if block.deleted || i == 0 {
                        continue;
                    }

                    if block.predecessors.is_empty() {
                        block.deleted = true;
                        removed.push(BasicBlockId(FunctionId(f), i));
                    }
                }

                for block in func.blocks.iter_mut() {
                    for remove in removed.iter() {
                        block.predecessors.remove(remove);
                    }
                }

                !removed.is_empty()
            } {}

            for block in func.blocks.iter() {
                if block.deleted {
                    continue;
                }

                if let Terminator::Branch(_, a, b) = block.terminator {
                    if func.blocks[a.1].predecessors.len() > 1 || func.blocks[b.1].predecessors.len() > 1 {
                        panic!("malformed ir");
                    }
                }
            }

            let mut var_map = HashMap::new();
            let mut phi_to_var_map = HashMap::new();
            for (i, block) in func.blocks.iter_mut().enumerate() {
                if block.deleted {
                    continue;
                }

                match block.predecessors.len() {
                    0 => (),

                    1 => {
                        let prev = block.predecessors.iter().next().unwrap().1;
                        for var in func.arg_types.len()..func.variables.len() {
                            let var = VariableId(var);
                            if let Some(&val) = var_map.get(&(var, prev)) {
                                var_map.insert((var, i), val);
                            }
                        }
                    }

                    _ => {
                        for var in func.arg_types.len()..func.variables.len() {
                            let var = VariableId(var);
                            let operation = Operation::Phi(Vec::new());
                            let val = Value(func.value_index);
                            func.value_index += 1;
                            let phi = Instruction {
                                yielded: Some(val),
                                type_: func.variables[var.0].type_.clone(),
                                operation,
                            };
                            block.instructions.insert(0, phi);
                            var_map.insert((var, i), val);
                            phi_to_var_map.insert(val, var);
                        }
                    }
                }

                let mut to_remove = Vec::new();
                for (j, instruction) in block.instructions.iter_mut().enumerate() {
                    match instruction.operation {
                        Operation::GetVar(var) => {
                            match var_map.get(&(var, i)) {
                                Some(&val) => {
                                    instruction.operation = Operation::Identity(val);
                                }

                                None => {
                                    panic!("malformed ir");
                                }
                            }
                        }

                        Operation::SetVar(var, val) => {
                            var_map.insert((var, i), val);
                            to_remove.push(j);
                        }

                        _ => (),
                    }
                }

                for remove in to_remove.into_iter().rev() {
                    block.instructions.remove(remove);
                }
            }

            for block in func.blocks.iter_mut() {
                if block.deleted {
                    continue;
                }

                if block.predecessors.len() > 1 {
                    for instruction in block.instructions.iter_mut() {
                        if let Operation::Phi(mapping) = &mut instruction.operation {
                            if let Some(&var) = instruction.yielded.as_ref().and_then(|v| phi_to_var_map.get(v)) {
                                *mapping = block.predecessors.iter().filter_map(|&v| var_map.get(&(var, v.1)).map(|&u| (v, u))).collect();
                            }
                        }
                    }
                }
            }
        }

        self.internal
    }

    pub fn new_function(
        &mut self,
        name: &str,
        args: &[(&str, Type)],
        ret_type: &Type,
    ) -> FunctionId {
        let id = self.internal.functions.len();
        self.internal.functions.push(Function {
            name: name.to_owned(),
            arg_types: args.iter().map(|(_, t)| t.clone()).collect(),
            ret_type: ret_type.clone(),
            variables: args
                .iter()
                .map(|(n, t)| Variable {
                    name: (*n).to_owned(),
                    type_: t.clone(),
                })
                .collect(),
            blocks: Vec::new(),
            value_index: 0,
        });
        FunctionId(id)
    }

    pub fn switch_to_function(&mut self, id: FunctionId) {
        self.current_function = Some(id.0);
        self.current_block = None;
    }

    pub fn push_block(&mut self) -> Option<BasicBlockId> {
        if let Some(func_id) = self.current_function {
            let func = unsafe { self.internal.functions.get_unchecked_mut(func_id) };
            let block_id = func.blocks.len();
            func.blocks.push(BasicBlock {
                deleted: false,
                predecessors: HashSet::new(),
                instructions: Vec::new(),
                terminator: Terminator::NoTerminator,
            });
            Some(BasicBlockId(FunctionId(func_id), block_id))
        } else {
            None
        }
    }

    pub fn switch_to_block(&mut self, id: BasicBlockId) {
        match self.current_function {
            Some(x) if id.0 .0 == x => self.current_block = Some(id.1),
            _ => self.current_block = None,
        }
    }

    pub fn push_instruction(&mut self, type_: &Type, instr: Operation) -> Option<Value> {
        if let Some(func_id) = self.current_function {
            if let Some(block_id) = self.current_block {
                let yielded = match &instr {
                    Operation::SetVar(_, _) => false,
                    Operation::Call(f, _) => {
                        if let Some(f) = self.internal.functions.get(f.0) {
                            !matches!(f.ret_type, Type::Void)
                        } else {
                            false
                        }
                    }

                    _ => true,
                };
                let func = unsafe { self.internal.functions.get_unchecked_mut(func_id) };
                let block = unsafe { func.blocks.get_unchecked_mut(block_id) };
                let yielded = if yielded {
                    Some(Value(func.value_index))
                } else {
                    None
                };
                func.value_index += 1;
                block.instructions.push(Instruction {
                    yielded,
                    type_: type_.clone(),
                    operation: instr,
                });
                if let Type::Void = type_ {
                    return None;
                } else {
                    return yielded;
                }
            }
        }

        None
    }

    pub fn push_variable(&mut self, name: &str, type_: &Type) -> Option<VariableId> {
        if let Some(func_id) = self.current_function {
            let func = unsafe { self.internal.functions.get_unchecked_mut(func_id) };
            let id = func.variables.len();
            func.variables.push(Variable {
                name: name.to_owned(),
                type_: type_.clone(),
            });
            Some(VariableId(id))
        } else {
            None
        }
    }

    pub fn set_terminator(&mut self, terminator: Terminator) {
        if let Some(func_id) = self.current_function {
            if let Some(block_id) = self.current_block {
                let func = unsafe { self.internal.functions.get_unchecked_mut(func_id) };
                let block = unsafe { func.blocks.get_unchecked_mut(block_id) };
                block.terminator = terminator;
            }
        }
    }

    pub fn get_function(&self) -> Option<FunctionId> {
        self.current_function.map(FunctionId)
    }

    pub fn get_function_args(&self, func: FunctionId) -> Option<Vec<VariableId>> {
        self.internal
            .functions
            .get(func.0)
            .map(|f| (0..f.arg_types.len()).into_iter().map(VariableId).collect())
    }

    pub fn get_block(&self) -> Option<BasicBlockId> {
        if let Some(f) = self.get_function() {
            self.current_block.map(|b| BasicBlockId(f, b))
        } else {
            None
        }
    }
}
