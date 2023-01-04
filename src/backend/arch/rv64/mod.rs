use std::{collections::HashMap, fmt::Display};

use crate::backend::ir::{Value, Operation, Terminator, Type, VariableId};

use super::{InstructionSelector, VReg, Location, VCodeGenerator, VCode};

const RV_REGISTER_ZERO: usize = 0;
const RV_REGISTER_A0: usize = 10;

pub enum RvInstruction {
    PhiPlaceholder {
        rd: VReg,
        options: Vec<(Location, VReg)>,
    },

    Integer {
        rd: VReg,
        value: u64,
    },

    Add {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Jal {
        rd: VReg,
        location: Location,
    },

    Bne {
        rx: VReg,
        ry: VReg,
        location: Location,
    },

    Ret,
}

impl Display for RvInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RvInstruction::PhiPlaceholder { rd, .. } => write!(f, "phi {} ...", rd),
            RvInstruction::Integer { rd, value } => write!(f, "add {}, %r0, {}", rd, value),
            RvInstruction::Add { rd, rx, ry } => write!(f, "add {}, {}, {}", rd, rx, ry),
            RvInstruction::Jal { rd, location } => write!(f, "jal {}, {}", rd, location),
            RvInstruction::Bne { rx, ry, location } => write!(f, "bne {}, {}, {}", rx, ry, location),
            RvInstruction::Ret => write!(f, "ret"),
        }
    }
}

#[derive(Default)]
pub struct RvSelector {
    value_map: HashMap<Value, VReg>,
    var_map: HashMap<VariableId, VReg>,
    vreg_index: usize,
}

impl InstructionSelector for RvSelector {
    type Instruction = RvInstruction;

    fn select_instr(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>, result: Option<Value>, _type_: Type, op: Operation) {
        let rd = match result {
            Some(val) => {
                let dest = VReg::Virtual(self.vreg_index);
                self.vreg_index += 1;
                self.value_map.insert(val, dest);
                dest
            }

            None => {
                VReg::RealRegister(RV_REGISTER_ZERO)
            }
        };

        match op {
            Operation::Integer(_signed, mut value) => {
                // TODO: better way to do this
                while value.len() < 8 {
                    value.push(0);
                }
                let value = u64::from_le_bytes(value[..8].try_into().unwrap());
                gen.push_instruction(RvInstruction::Integer {
                    rd,
                    value,
                });
            }

            Operation::Add(a, b) => {
                if let Some(&rx) = self.value_map.get(&a) {
                    if let Some(&ry) = self.value_map.get(&b) {
                        gen.push_instruction(RvInstruction::Add {
                            rd,
                            rx,
                            ry,
                        });
                    }
                }
            }

            Operation::Sub(_, _) => todo!(),
            Operation::Mul(_, _) => todo!(),
            Operation::Div(_, _) => todo!(),
            Operation::Mod(_, _) => todo!(),
            Operation::Bsl(_, _) => todo!(),
            Operation::Bsr(_, _) => todo!(),
            Operation::Eq(_, _) => todo!(),
            Operation::Ne(_, _) => todo!(),
            Operation::Lt(_, _) => todo!(),
            Operation::Le(_, _) => todo!(),
            Operation::Gt(_, _) => todo!(),
            Operation::Ge(_, _) => todo!(),
            Operation::BitAnd(_, _) => todo!(),
            Operation::BitOr(_, _) => todo!(),
            Operation::BitXor(_, _) => todo!(),

            Operation::Phi(mapping) => {
                gen.push_instruction(RvInstruction::PhiPlaceholder {
                    rd,
                    options: mapping.into_iter().filter_map(|(b, v)| {
                        if let Some(&l) = gen.label_map().get(&b) {
                            if let Some(&r) = self.value_map.get(&v) {
                                return Some((Location::InternalLabel(l), r))
                            }
                        }

                        None
                    }).collect(),
                });
            }

            Operation::GetVar(var) => {
                if let Some(&rx) = self.var_map.get(&var) {
                    gen.push_instruction(RvInstruction::Add {
                        rd,
                        rx,
                        ry: VReg::RealRegister(RV_REGISTER_ZERO),
                    });
                }
            }

            Operation::SetVar(var, val) => {
                let rd = match self.var_map.get(&var) {
                    Some(&rd) => rd,
                    None => {
                        let dest = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        self.var_map.insert(var, dest);
                        dest
                    }
                };
                if let Some(&rx) = self.value_map.get(&val) {
                    gen.push_instruction(RvInstruction::Add {
                        rd,
                        rx,
                        ry: VReg::RealRegister(RV_REGISTER_ZERO),
                    });
                }
            }

            Operation::Call(_, _) => todo!(),
            Operation::CallIndirect(_, _) => todo!(),
        }
    }

    fn select_term(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>, op: Terminator) {
        match op {
            Terminator::NoTerminator => (),

            Terminator::ReturnVoid => {
                gen.push_instruction(RvInstruction::Ret);
            }

            Terminator::Return(v) => {
                if let Some(&rx) = self.value_map.get(&v) {
                    gen.push_instruction(RvInstruction::Add {
                        rd: VReg::RealRegister(RV_REGISTER_A0),
                        rx,
                        ry: VReg::RealRegister(RV_REGISTER_ZERO),
                    });
                }

                gen.push_instruction(RvInstruction::Ret);
            }

            Terminator::Jump(label) => {
                if let Some(&label) = gen.label_map().get(&label) {
                    gen.push_instruction(RvInstruction::Jal {
                        rd: VReg::RealRegister(RV_REGISTER_ZERO),
                        location: Location::InternalLabel(label),
                    });
                }
            }

            Terminator::Branch(v, l1, l2) => {
                if let Some(&rx) = self.value_map.get(&v) {
                    if let Some(&l1) = gen.label_map().get(&l1) {
                        gen.push_instruction(RvInstruction::Bne {
                            rx,
                            ry: VReg::RealRegister(RV_REGISTER_ZERO),
                            location: Location::InternalLabel(l1),
                        });
                    }
                    if let Some(&l2) = gen.label_map().get(&l2) {
                        gen.push_instruction(RvInstruction::Jal {
                            rd: VReg::RealRegister(RV_REGISTER_ZERO),
                            location: Location::InternalLabel(l2),
                        });
                    }
                }
            }
        }
    }

    fn post_generation(&mut self, vcode: &mut VCode<Self::Instruction>) {
        for func in vcode.functions.iter_mut() {
            let mut v = Vec::new();
            for (i, labelled) in func.labels.iter().enumerate() {
                for (j, instr) in labelled.instructions.iter().enumerate() {
                    if let RvInstruction::PhiPlaceholder { .. } = instr {
                        v.push((i, j));
                    }
                }
            }

            for (label_index, instr_index) in v.into_iter().rev() {
                let phi = func.labels[label_index].instructions.remove(instr_index);
                if let RvInstruction::PhiPlaceholder { rd, options } = phi {
                    for (label, rx) in options {
                        if let Location::InternalLabel(label) = label {
                            let labelled = &mut func.labels[label];
                            labelled.instructions.insert(labelled.instructions.len() - 1, RvInstruction::Add {
                                rd,
                                rx,
                                ry: VReg::RealRegister(RV_REGISTER_ZERO),
                            });
                        }
                    }
                }
            }
        }
    }
}
