use std::{collections::HashMap, fmt::Display};

use crate::backend::{
    ir::{Operation, Terminator, Type, Value},
    RegisterAllocator,
};

use super::{Instr, InstructionSelector, Location, VCode, VCodeGenerator, VReg};

pub const RV_REGISTER_ZERO: usize = 0;
pub const RV_REGISTER_RA: usize = 1;
pub const RV_REGISTER_SP: usize = 2;
pub const RV_REGISTER_GP: usize = 3;
pub const RV_REGISTER_TP: usize = 4;
pub const RV_REGISTER_T0: usize = 5;
pub const RV_REGISTER_T1: usize = 6;
pub const RV_REGISTER_T2: usize = 7;
pub const RV_REGISTER_FP: usize = 8;
pub const RV_REGISTER_S1: usize = 9;
pub const RV_REGISTER_A0: usize = 10;
pub const RV_REGISTER_A1: usize = 11;
pub const RV_REGISTER_A2: usize = 12;
pub const RV_REGISTER_A3: usize = 13;
pub const RV_REGISTER_A4: usize = 14;
pub const RV_REGISTER_A5: usize = 15;
pub const RV_REGISTER_A6: usize = 16;
pub const RV_REGISTER_A7: usize = 17;
pub const RV_REGISTER_S2: usize = 18;
pub const RV_REGISTER_S3: usize = 19;
pub const RV_REGISTER_S4: usize = 20;
pub const RV_REGISTER_S5: usize = 21;
pub const RV_REGISTER_S6: usize = 22;
pub const RV_REGISTER_S7: usize = 23;
pub const RV_REGISTER_S8: usize = 24;
pub const RV_REGISTER_S9: usize = 25;
pub const RV_REGISTER_S10: usize = 26;
pub const RV_REGISTER_S11: usize = 27;
pub const RV_REGISTER_T3: usize = 28;
pub const RV_REGISTER_T4: usize = 29;
pub const RV_REGISTER_T5: usize = 30;
pub const RV_REGISTER_T6: usize = 31;

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
            RvInstruction::Bne { rx, ry, location } => {
                write!(f, "bne {}, {}, {}", rx, ry, location)
            }
            RvInstruction::Ret => write!(f, "ret"),
        }
    }
}

impl Instr for RvInstruction {
    fn get_regs() -> Vec<VReg> {
        vec![
            VReg::RealRegister(RV_REGISTER_T0),
            VReg::RealRegister(RV_REGISTER_T1),
            VReg::RealRegister(RV_REGISTER_T2),
            VReg::RealRegister(RV_REGISTER_T3),
            VReg::RealRegister(RV_REGISTER_T4),
            VReg::RealRegister(RV_REGISTER_T5),
            VReg::RealRegister(RV_REGISTER_T6),
            VReg::RealRegister(RV_REGISTER_S1),
            VReg::RealRegister(RV_REGISTER_S2),
            VReg::RealRegister(RV_REGISTER_S3),
            VReg::RealRegister(RV_REGISTER_S4),
            VReg::RealRegister(RV_REGISTER_S5),
            VReg::RealRegister(RV_REGISTER_S6),
            VReg::RealRegister(RV_REGISTER_S7),
            VReg::RealRegister(RV_REGISTER_S8),
            VReg::RealRegister(RV_REGISTER_S9),
            VReg::RealRegister(RV_REGISTER_S10),
            VReg::RealRegister(RV_REGISTER_S11),
            VReg::RealRegister(RV_REGISTER_A0),
            VReg::RealRegister(RV_REGISTER_A1),
            VReg::RealRegister(RV_REGISTER_A2),
            VReg::RealRegister(RV_REGISTER_A3),
            VReg::RealRegister(RV_REGISTER_A4),
            VReg::RealRegister(RV_REGISTER_A5),
            VReg::RealRegister(RV_REGISTER_A6),
            VReg::RealRegister(RV_REGISTER_A7),
        ]
    }

    fn collect_registers<A>(&self, alloc: &mut A)
    where
        A: RegisterAllocator,
    {
        match self {
            RvInstruction::PhiPlaceholder { .. } => (),

            RvInstruction::Integer { rd, .. } => {
                alloc.add_def(*rd);
            }

            RvInstruction::Add { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
                alloc.used_simultaneously(&[*rx, *ry]);
            }

            RvInstruction::Jal { rd, .. } => {
                alloc.add_def(*rd);
            }

            RvInstruction::Bne { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
                alloc.used_simultaneously(&[*rx, *ry]);
            }

            RvInstruction::Ret => (),
        }
    }

    fn apply_reg_allocs(&mut self, alloc: &HashMap<VReg, VReg>) {
        match self {
            RvInstruction::PhiPlaceholder { .. } => (),

            RvInstruction::Integer { rd, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
            }

            RvInstruction::Add { rd, rx, ry } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
                if let Some(new) = alloc.get(ry) {
                    *ry = *new;
                }
            }

            RvInstruction::Jal { rd, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
            }

            RvInstruction::Bne { rx, ry, .. } => {
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
                if let Some(new) = alloc.get(ry) {
                    *ry = *new;
                }
            }

            RvInstruction::Ret => (),
        }
    }
}

#[derive(Default)]
pub struct RvSelector {
    value_map: HashMap<Value, VReg>,
    vreg_index: usize,
}

impl InstructionSelector for RvSelector {
    type Instruction = RvInstruction;

    fn select_instr(
        &mut self,
        gen: &mut VCodeGenerator<Self::Instruction, Self>,
        result: Option<Value>,
        _type_: Type,
        op: Operation,
    ) {
        let rd = match result {
            Some(val) => {
                let dest = VReg::Virtual(self.vreg_index);
                self.vreg_index += 1;
                self.value_map.insert(val, dest);
                dest
            }

            None => VReg::RealRegister(RV_REGISTER_ZERO),
        };

        match op {
            Operation::Integer(_signed, mut value) => {
                // TODO: better way to do this
                while value.len() < 8 {
                    value.push(0);
                }
                let value = u64::from_le_bytes(value[..8].try_into().unwrap());
                gen.push_instruction(RvInstruction::Integer { rd, value });
            }

            Operation::Add(a, b) => {
                if let Some(&rx) = self.value_map.get(&a) {
                    if let Some(&ry) = self.value_map.get(&b) {
                        gen.push_instruction(RvInstruction::Add { rd, rx, ry });
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
                    options: mapping
                        .into_iter()
                        .filter_map(|(b, v)| {
                            if let Some(&l) = gen.label_map().get(&b) {
                                if let Some(&r) = self.value_map.get(&v) {
                                    return Some((Location::InternalLabel(l), r));
                                }
                            }

                            None
                        })
                        .collect(),
                });
            }

            Operation::GetVar(_) => todo!(),

            Operation::SetVar(_, _) => todo!(),

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
                            labelled.instructions.insert(
                                labelled.instructions.len() - 1,
                                RvInstruction::Add {
                                    rd,
                                    rx,
                                    ry: VReg::RealRegister(RV_REGISTER_ZERO),
                                },
                            );
                        }
                    }
                }
            }
        }
    }
}
