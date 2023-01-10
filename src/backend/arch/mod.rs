use std::{collections::HashMap, fmt::Display, marker::PhantomData};

use super::{
    ir::{BasicBlockId, FunctionId, Operation, Terminator, Type, Value},
    RegisterAllocator,
};

pub mod rv64;
pub mod x64;
pub mod urcl;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum VReg {
    RealRegister(usize),
    Virtual(usize),

    Spilled(usize),
}

impl Display for VReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VReg::RealRegister(r) => write!(f, "%r{}", r),
            VReg::Virtual(v) => write!(f, "${}", v),
            VReg::Spilled(s) => write!(f, "[spilled #{}]", s),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Location {
    InternalLabel(usize),
    Function(usize),
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::InternalLabel(label) => write!(f, ".L{}", label),
            Location::Function(func) => write!(f, "F{}", func),
        }
    }
}

pub trait Instr: Sized {
    fn get_regs() -> Vec<VReg>;

    fn collect_registers<A>(&self, alloc: &mut A)
    where
        A: RegisterAllocator;

    fn apply_reg_allocs(&mut self, alloc: &HashMap<VReg, VReg>);

    fn mandatory_transforms(vcode: &mut VCode<Self>);

    fn emit_assembly(vcode: &VCode<Self>);
}

pub struct VCode<I>
where
    I: Instr,
{
    pub name: String,
    pub functions: Vec<Function<I>>,
}

pub struct Function<I>
where
    I: Instr,
{
    pub name: String,
    pub labels: Vec<LabelledInstructions<I>>,
}

pub struct LabelledInstructions<I>
where
    I: Instr,
{
    pub instructions: Vec<I>,
}

impl<I> Display for VCode<I>
where
    I: Display + Instr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ";;; module = {}\n", self.name)?;

        for (i, func) in self.functions.iter().enumerate() {
            write!(f, "F{}:\n{}", i, func)?;
        }

        Ok(())
    }
}

impl<I> Display for Function<I>
where
    I: Display + Instr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for (i, labelled) in self.labels.iter().enumerate() {
            write!(f, ".L{}:\n{}", i, labelled)?;
        }

        Ok(())
    }
}

impl<I> Display for LabelledInstructions<I>
where
    I: Display + Instr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in self.instructions.iter() {
            writeln!(f, "    {}", instr)?;
        }

        Ok(())
    }
}

pub trait InstructionSelector: Default {
    type Instruction: Instr;

    fn select_instr(
        &mut self,
        gen: &mut VCodeGenerator<Self::Instruction, Self>,
        result: Option<Value>,
        type_: Type,
        op: Operation,
    );

    fn select_term(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>, op: Terminator);

    fn post_generation(&mut self, vcode: &mut VCode<Self::Instruction>);
}

pub struct VCodeGenerator<I, S>
where
    S: InstructionSelector<Instruction = I>,
    I: Instr,
{
    internal: VCode<I>,
    _phantom: PhantomData<S>,
    func_map: HashMap<FunctionId, usize>,
    label_map: HashMap<BasicBlockId, usize>,
    current_function: Option<usize>,
    current_block: Option<usize>,
}

impl<I, S> VCodeGenerator<I, S>
where
    S: InstructionSelector<Instruction = I>,
    I: Instr,
{
    pub fn new_module(name: &str) -> Self {
        Self {
            internal: VCode {
                name: name.to_owned(),
                functions: Vec::new(),
            },
            _phantom: PhantomData::default(),
            func_map: HashMap::new(),
            label_map: HashMap::new(),
            current_function: None,
            current_block: None,
        }
    }

    pub fn func_map(&self) -> &HashMap<FunctionId, usize> {
        &self.func_map
    }

    pub fn label_map(&self) -> &HashMap<BasicBlockId, usize> {
        &self.label_map
    }

    pub fn add_function(&mut self, name: &str, id: FunctionId) {
        let f = self.internal.functions.len();
        self.internal.functions.push(Function {
            name: name.to_owned(),
            labels: Vec::new(),
        });
        self.func_map.insert(id, f);
    }

    pub fn switch_to_function(&mut self, id: FunctionId) {
        self.current_function = self.func_map.get(&id).cloned();
        self.label_map.clear();
    }

    pub fn push_label(&mut self, id: BasicBlockId) {
        if let Some(func) = self
            .current_function
            .and_then(|v| self.internal.functions.get_mut(v))
        {
            let label = func.labels.len();
            func.labels.push(LabelledInstructions {
                instructions: Vec::new(),
            });
            self.label_map.insert(id, label);
        }
    }

    pub fn switch_to_label(&mut self, id: BasicBlockId) {
        self.current_block = self.label_map.get(&id).cloned();
    }

    pub fn push_instruction(&mut self, instruction: I) {
        if let Some(func) = self
            .current_function
            .and_then(|v| self.internal.functions.get_mut(v))
        {
            if let Some(labelled) = self.current_block.and_then(|v| func.labels.get_mut(v)) {
                labelled.instructions.push(instruction);
            }
        }
    }

    pub fn build(mut self, mut selector: S) -> VCode<I> {
        selector.post_generation(&mut self.internal);
        self.internal
    }
}

impl<I> VCode<I>
where
    I: Instr,
{
    pub fn allocate_regs<A>(&mut self)
    where
        A: RegisterAllocator,
    {
        for func in self.functions.iter_mut() {
            let mut allocator = A::default();

            for labelled in func.labels.iter() {
                for instr in labelled.instructions.iter() {
                    instr.collect_registers(&mut allocator);
                    allocator.next_live_step();
                }
            }

            let allocations = allocator.allocate_regs::<I>();
            for labelled in func.labels.iter_mut() {
                for instr in labelled.instructions.iter_mut() {
                    instr.apply_reg_allocs(&allocations);
                }
            }
        }

        I::mandatory_transforms(self);
    }

    pub fn emit_assembly(&self) {
        I::emit_assembly(self);
    }
}
