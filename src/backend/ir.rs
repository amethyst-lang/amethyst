use std::fmt::Display;

#[derive(Default)]
pub struct Module {
    name: String,
    functions: Vec<Function>,
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
            Type::Integer(signed, width) => write!(f, "{}{}", if *signed { "i" } else { "u" }, width),
        }
    }
}

struct Function {
    name: String,
    arg_types: Vec<Type>,
    ret_type: Type,
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

        for (i, block) in self.blocks.iter().enumerate() {
            write!(f, "{}:\n{}", i, block)?;
        }
        write!(f, "}}")
    }
}

#[derive(Copy, Clone)]
pub struct FunctionId(usize);

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}


struct BasicBlock {
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

pub struct BasicBlockId(FunctionId, usize);

impl Display for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.1)
    }
}

struct Instruction {
    yielded: Option<Value>,
    operation: Operation,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(yielded) = &self.yielded {
            write!(f, "{} = ", yielded)?;
        }

        write!(f, "{}", self.operation)
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
    Integer(bool, Vec<u8>),
    Add(Value, Value)
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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

            Operation::Add(a, b) => write!(f, "addi {}, {}", a, b)
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

#[derive(Copy, Clone)]
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

    pub fn build(self) -> Module {
        self.internal
    }

    pub fn new_function(&mut self, name: &str, arg_types: &[Type], ret_type: &Type) -> FunctionId {
        let id = self.internal.functions.len();
        self.internal.functions.push(Function {
            name: name.to_owned(),
            arg_types: arg_types.to_owned(),
            ret_type: ret_type.clone(),
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
                instructions: vec![],
                terminator: Terminator::NoTerminator,
            });
            Some(BasicBlockId(FunctionId(func_id), block_id))
        } else {
            None
        }
    }

    pub fn switch_to_block(&mut self, id: BasicBlockId) {
        match self.current_function {
            Some(x) if id.0.0 == x => self.current_block = Some(id.1),
            _ => self.current_block = None,
        }
    }

    pub fn push_instruction(&mut self, instr: Operation) -> Option<Value> {
        if let Some(func_id) = self.current_function {
            if let Some(block_id) = self.current_block {
                let func = unsafe { self.internal.functions.get_unchecked_mut(func_id) };
                let block = unsafe { func.blocks.get_unchecked_mut(block_id) };
                let yielded = Some(Value(func.value_index));
                func.value_index += 1;
                block.instructions.push(Instruction {
                    yielded,
                    operation: instr,
                });
                return yielded
            }
        }

        None
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
}
