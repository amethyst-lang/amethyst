use std::{collections::HashMap, fmt::Display, fs::File, io::Write};

use crate::backend::{
    ir::{Operation, Terminator, Type, Value},
    RegisterAllocator,
};

use super::{Instr, InstructionSelector, Location, VCode, VCodeGenerator, VReg};

pub const URCL_REGISTER_ZERO: usize = 0;
pub const URCL_REGISTER_PC: usize = 1;
pub const URCL_REGISTER_SP: usize = 2;
pub const URCL_REGISTER_R1: usize = 3;
pub const URCL_REGISTER_R2: usize = 3;
pub const URCL_REGISTER_R3: usize = 3;
pub const URCL_REGISTER_R4: usize = 3;
pub const URCL_REGISTER_R5: usize = 3;
pub const URCL_REGISTER_R6: usize = 3;

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

    Load {
        rd: VReg,
        imm: i16,
        rx: VReg,
    },

    Store {
        rx: VReg,
        imm: i16,
        ry: VReg,
    },
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

            RvInstruction::Load { rd, imm, rx } => write!(f, "load {}, {}({})", rd, imm, rx),

            RvInstruction::Store { rx, imm, ry } => write!(f, "store {}, {}({})", rx, imm, ry),
        }
    }
}

impl Instr for RvInstruction {
    fn get_regs() -> Vec<VReg> {
        vec![
            VReg::RealRegister(RV_REGISTER_T0),
            /*
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
            */
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
            }

            RvInstruction::Jal { rd, .. } => {
                alloc.add_def(*rd);
            }

            RvInstruction::Bne { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            RvInstruction::Ret => (),

            RvInstruction::Load { .. } => (),

            RvInstruction::Store { .. } => (),
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

            RvInstruction::Load { .. } => (),

            RvInstruction::Store { .. } => (),
        }
    }

    fn mandatory_transforms(vcode: &mut VCode<Self>) {
        enum SwapType {
            Rd,
            Rx,
            Ry
        }
        use SwapType::*;

        for func in vcode.functions.iter_mut() {
            for labelled in func.labels.iter_mut() {
                let mut swap = Vec::new();
                for (i, instruction) in labelled.instructions.iter().enumerate() {
                    match instruction {
                        RvInstruction::PhiPlaceholder { .. } => (),

                        RvInstruction::Integer { rd, .. } => {
                            if let VReg::Spilled(_) = *rd {
                                swap.push((i, *rd, Rd));
                            }
                        }

                        RvInstruction::Add { rd, rx, ry } => {
                            if let VReg::Spilled(_) = *rd {
                                swap.push((i, *rd, Rd));
                            }
                            if let VReg::Spilled(_) = *rx {
                                swap.push((i, *rx, Rx));
                            }
                            if let VReg::Spilled(_) = *ry {
                                swap.push((i, *ry, Ry));
                            }
                        }

                        RvInstruction::Jal { .. } => (),

                        RvInstruction::Bne { .. } => (),

                        RvInstruction::Ret => (),

                        RvInstruction::Load { .. } => (),

                        RvInstruction::Store { .. } => (),
                    }
                }

                let temp_reg = VReg::RealRegister(RV_REGISTER_T0);
                for (index, reg, swap_type) in swap.into_iter().rev() {
                    match swap_type {
                        Rd => todo!(),
                        Rx | Ry => {
                            labelled.instructions.insert(index, RvInstruction::Store {
                                rx: temp_reg,
                                imm: 0,
                                ry: VReg::RealRegister(RV_REGISTER_SP),
                            });
                            let spilled = if let VReg::Spilled(s) = reg {
                                s
                            } else {
                                unreachable!()
                            };
                            labelled.instructions.insert(index + 1, RvInstruction::Load {
                                rd: temp_reg,
                                imm: spilled as i16 * -8,
                                rx: VReg::RealRegister(RV_REGISTER_FP),
                            });
                        }
                    }
                }
            }
        }
    }

    fn emit_assembly(vcode: &VCode<Self>) {
        match File::create(format!("{}.s", vcode.name)) {
            Ok(mut file) => {
                let _ = writeln!(file, ".global main");
                for func in vcode.functions.iter() {
                    let _ = writeln!(file, "{}:", func.name);
                    for (i, labelled) in func.labels.iter().enumerate() {
                        let _ = writeln!(file, ".L{}:", i);
                        for instruction in labelled.instructions.iter() {
                            match instruction {
                                RvInstruction::PhiPlaceholder { .. } => (),

                                RvInstruction::Integer { rd, value } => {
                                    let _ = writeln!(file, "    li {}, {}", register(*rd), value);
                                }

                                RvInstruction::Add { rd, rx, ry } => {
                                    let _ = writeln!(file, "    add {}, {}, {}", register(*rd), register(*rx), register(*ry));
                                }

                                RvInstruction::Jal { rd, location } => {
                                    match *location {
                                        Location::InternalLabel(_) => {
                                            let _ = writeln!(file, "    jal {}, {}", register(*rd), location);
                                        }
                                        Location::Function(f) => {
                                            let _ = writeln!(file, "    jal {}, {}", register(*rd), vcode.functions[f].name);
                                        }
                                    }
                                }

                                RvInstruction::Bne { rx, ry, location } => {
                                    match *location {
                                        Location::InternalLabel(_) => {
                                            let _ = writeln!(file, "    bne {}, {}, {}", register(*rx), register(*ry), location);
                                        }
                                        Location::Function(f) => {
                                            let _ = writeln!(file, "    bne {}, {}, {}", register(*rx), register(*ry), vcode.functions[f].name);
                                        }
                                    }
                                }

                                RvInstruction::Ret => {
                                    let _ = write!(file, "    ret");
                                }

                                RvInstruction::Load { rd, imm, rx } => {
                                    let _ = writeln!(file, "    load {}, {}({})", rd, imm, rx);
                                }

                                RvInstruction::Store { rx, imm, ry } => {
                                    let _ = writeln!(file, "    store {}, {}({})", rx, imm, ry);
                                }
                            }
                        }
                    }

                    let _ = writeln!(file);
                }
            }
            Err(e) => {
                eprintln!("Could not open file `{}`: {}", vcode.name, e);
            }
        }
    }
}

fn register(reg: VReg) -> String {
    match reg {
        VReg::RealRegister(reg) => {
            String::from(match reg {
                v if v == RV_REGISTER_ZERO => "zero",
                v if v == RV_REGISTER_RA => "ra",
                v if v == RV_REGISTER_SP => "sp",
                v if v == RV_REGISTER_GP => "gp",
                v if v == RV_REGISTER_TP => "tp",
                v if v == RV_REGISTER_T0 => "t0",
                v if v == RV_REGISTER_T1 => "t1",
                v if v == RV_REGISTER_T2 => "t2",
                v if v == RV_REGISTER_FP => "s0",
                v if v == RV_REGISTER_S1 => "s1",
                v if v == RV_REGISTER_A0 => "a0",
                v if v == RV_REGISTER_A1 => "a1",
                v if v == RV_REGISTER_A2 => "a2",
                v if v == RV_REGISTER_A3 => "a3",
                v if v == RV_REGISTER_A4 => "a4",
                v if v == RV_REGISTER_A5 => "a5",
                v if v == RV_REGISTER_A6 => "a6",
                v if v == RV_REGISTER_A7 => "a7",
                v if v == RV_REGISTER_S2 => "s2",
                v if v == RV_REGISTER_S3 => "s3",
                v if v == RV_REGISTER_S4 => "s4",
                v if v == RV_REGISTER_S5 => "s5",
                v if v == RV_REGISTER_S6 => "s6",
                v if v == RV_REGISTER_S7 => "s7",
                v if v == RV_REGISTER_S8 => "s8",
                v if v == RV_REGISTER_S9 => "s9",
                v if v == RV_REGISTER_S10 => "s10",
                v if v == RV_REGISTER_S11 => "s11",
                v if v == RV_REGISTER_T3 => "t3",
                v if v == RV_REGISTER_T4 => "t4",
                v if v == RV_REGISTER_T5 => "t5",
                v if v == RV_REGISTER_T6 => "t6",
                _ => unreachable!(),
            })
        }

        VReg::Virtual(_) => unreachable!(),

        VReg::Spilled(s) => format!("-{}(fp)", 8 * s),
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
            Operation::Identity(value) => {
                if let Some(&rx) = self.value_map.get(&value) {
                    gen.push_instruction(RvInstruction::Add { rd, rx, ry: VReg::RealRegister(RV_REGISTER_ZERO) });
                }
            }

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

            Operation::GetVar(_) => unreachable!(),
            Operation::SetVar(_, _) => unreachable!(),

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
