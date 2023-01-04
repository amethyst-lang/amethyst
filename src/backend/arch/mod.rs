use std::collections::HashMap;

use super::ir::{Operation, FunctionId, BasicBlockId, Terminator, Value};

pub mod rv64;
pub mod x64;

pub struct VCode<I> {
    pub name: String,
    pub functions: Vec<Function<I>>,
}

pub struct Function<I> {
    pub name: String,
    pub labels: Vec<LabelledInstructions<I>>,
}

pub struct LabelledInstructions<I> {
    pub instructions: Vec<I>,
}

pub trait InstructionSelector<I>: Default {
    fn select_instr(&mut self, result: Option<Value>, op: Operation, current_labelled_instructions: &mut Vec<I>);
    fn select_term(&mut self, op: Terminator, current_labelled_instructions: &mut Vec<I>);
}

pub struct VCodeGenerator<I, S>
    where S: InstructionSelector<I>
{
    internal: VCode<I>,
    instr_selector: S,
    func_map: HashMap<FunctionId, usize>,
    label_map: HashMap<BasicBlockId, usize>,
    current_function: Option<usize>,
    current_block: Option<usize>,
}

impl<I, S> VCodeGenerator<I, S>
    where S: InstructionSelector<I>
{
    pub fn new_module(name: &str) -> Self {
        Self {
            internal: VCode {
                name: name.to_owned(),
                functions: Vec::new(),
            },
            instr_selector: S::default(),
            func_map: HashMap::new(),
            label_map: HashMap::new(),
            current_function: None,
            current_block: None,
        }
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
        if let Some(func) = self.current_function.and_then(|v| self.internal.functions.get_mut(v)) {
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

    pub fn select_instructions(&mut self, result: Option<Value>, op: Operation) {
        if let Some(func) = self.current_function.and_then(|v| self.internal.functions.get_mut(v)) {
            if let Some(labelled) = self.current_block.and_then(|v| func.labels.get_mut(v)) {
                self.instr_selector.select_instr(result, op, &mut labelled.instructions);
            }
        }
    }

    pub fn select_terminator(&mut self, op: Terminator) {
        if let Some(func) = self.current_function.and_then(|v| self.internal.functions.get_mut(v)) {
            if let Some(labelled) = self.current_block.and_then(|v| func.labels.get_mut(v)) {
                self.instr_selector.select_term(op, &mut labelled.instructions);
            }
        }
    }

    pub fn build(self) -> VCode<I> {
        self.internal
    }
}

