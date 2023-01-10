use std::{collections::HashMap, fmt::Display, fs::File, io::Write};

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

pub enum RvAluOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Sll,
    Srl,
    And,
    Or,
    Xor,
    Slt,
}

impl Display for RvAluOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RvAluOp::Add => write!(f, "add"),
            RvAluOp::Sub => write!(f, "sub"),
            RvAluOp::Mul => write!(f, "mul"),
            RvAluOp::Div => write!(f, "div"),
            RvAluOp::Rem => write!(f, "rem"),
            RvAluOp::Sll => write!(f, "sll"),
            RvAluOp::Srl => write!(f, "srl"),
            RvAluOp::And => write!(f, "and"),
            RvAluOp::Or => write!(f, "or"),
            RvAluOp::Xor => write!(f, "xor"),
            RvAluOp::Slt => write!(f, "slt"),
        }
    }
}

pub enum RvInstruction {
    PhiPlaceholder {
        rd: VReg,
        options: Vec<(Location, Value)>,
    },

    Integer {
        rd: VReg,
        value: u64,
    },

    AluOp {
        op: RvAluOp,
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    AluOpImm {
        op: RvAluOp,
        rd: VReg,
        rx: VReg,
        imm: i16,
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

            RvInstruction::Integer { rd, value } => write!(f, "li {}, {}", rd, value),

            RvInstruction::AluOp { op, rd, rx, ry } => write!(f, "{} {}, {}, {}", op, rd, rx, ry),
            RvInstruction::AluOpImm { op, rd, rx, imm } => write!(f, "{} {}, {}, {}", op, rd, rx, imm),

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
            VReg::RealRegister(RV_REGISTER_A0),
            VReg::RealRegister(RV_REGISTER_A1),
            VReg::RealRegister(RV_REGISTER_A2),
            VReg::RealRegister(RV_REGISTER_A3),
            VReg::RealRegister(RV_REGISTER_A4),
            VReg::RealRegister(RV_REGISTER_A5),
            VReg::RealRegister(RV_REGISTER_A6),
            VReg::RealRegister(RV_REGISTER_A7),
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

            RvInstruction::AluOp { rd, rx, ry, .. } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            RvInstruction::AluOpImm { rd, rx, .. } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
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

            RvInstruction::AluOp { rd, rx, ry, .. } => {
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

            RvInstruction::AluOpImm { rd, rx, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
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
        for func in vcode.functions.iter_mut() {
            for labelled in func.labels.iter_mut() {
                let mut swaps = Vec::new();
                #[derive(Copy, Clone)]
                enum SwapType {
                    Rd,
                    Rx,
                    Ry,
                }
                use SwapType::*;

                for (i, instruction) in labelled.instructions.iter_mut().enumerate() {
                    match instruction {
                        RvInstruction::PhiPlaceholder { .. } => (),

                        RvInstruction::Integer { rd, .. } => {
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(RV_REGISTER_TP);
                            }
                        }

                        RvInstruction::AluOp { rd, rx, ry, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(RV_REGISTER_TP);
                            }
                            if let VReg::Spilled(spill) = *ry {
                                swaps.push((i, spill, Ry));
                                *ry = VReg::RealRegister(RV_REGISTER_T0);
                            }
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(RV_REGISTER_TP);
                            }
                        }

                        RvInstruction::AluOpImm { rd, rx, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(RV_REGISTER_TP);
                            }
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(RV_REGISTER_TP);
                            }
                        }

                        RvInstruction::Jal { .. } => (),

                        RvInstruction::Bne { rx, ry, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(RV_REGISTER_TP);
                            }
                            if let VReg::Spilled(spill) = *ry {
                                swaps.push((i, spill, Ry));
                                *rx = VReg::RealRegister(RV_REGISTER_T0);
                            }
                        }

                        RvInstruction::Ret => (),

                        RvInstruction::Load { .. } => (),

                        RvInstruction::Store { .. } => (),
                    }
                }

                for (index, spill, swap_type) in swaps.into_iter().rev() {
                    match swap_type {
                        Rd => {
                            labelled.instructions.insert(index + 1, RvInstruction::Store {
                                rx: VReg::RealRegister(RV_REGISTER_TP),
                                imm: spill as i16 * -8,
                                ry: VReg::RealRegister(RV_REGISTER_FP),
                            });
                        }

                        Rx => {
                            labelled.instructions.insert(index, RvInstruction::Load {
                                rd: VReg::RealRegister(RV_REGISTER_TP),
                                imm: spill as i16 * -8,
                                rx: VReg::RealRegister(RV_REGISTER_FP),
                            });
                        }

                        Ry => {
                            labelled.instructions.insert(index, RvInstruction::Load {
                                rd: VReg::RealRegister(RV_REGISTER_T0),
                                imm: spill as i16 * -8,
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
                    let _ = writeln!(file, "{}:\n    sd s0, (sp)\n    add s0, sp, zero", func.name);
                    for (i, labelled) in func.labels.iter().enumerate() {
                        let _ = writeln!(file, ".L{}:", i);
                        for instruction in labelled.instructions.iter() {
                            match instruction {
                                RvInstruction::PhiPlaceholder { .. } => (),

                                RvInstruction::Integer { rd, value } => {
                                    let _ = writeln!(file, "    li {}, {}", register(*rd), value);
                                }

                                RvInstruction::AluOp { op, rd, rx, ry } => {
                                    let _ = writeln!(file, "    {} {}, {}, {}", op, register(*rd), register(*rx), register(*ry));
                                }

                                RvInstruction::AluOpImm { op: RvAluOp::Sub, rd, rx, imm } => {
                                    let _ = writeln!(file, "    addi {}, {}, {}", register(*rd), register(*rx), -imm);
                                }

                                RvInstruction::AluOpImm { op, rd, rx, imm } => {
                                    let _ = writeln!(file, "    {} {}, {}, {}", op, register(*rd), register(*rx), imm);
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
                                    let _ = write!(file, "    add sp, s0, zero\n    ret");
                                }

                                RvInstruction::Load { rd, imm, rx } => {
                                    let _ = writeln!(file, "    ld {}, {}({})", register(*rd), imm, register(*rx));
                                }

                                RvInstruction::Store { rx, imm, ry } => {
                                    let _ = writeln!(file, "    sd {}, {}({})", register(*rx), imm, register(*ry));
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
                    gen.push_instruction(RvInstruction::AluOp { op: RvAluOp::Add, rd, rx, ry: VReg::RealRegister(RV_REGISTER_ZERO) });
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

            Operation::Add(a, b)
            | Operation::Sub(a, b)
            | Operation::Mul(a, b)
            | Operation::Div(a, b)
            | Operation::Mod(a, b)
            | Operation::Bsl(a, b)
            | Operation::Bsr(a, b)
            | Operation::Lt(a, b)
            | Operation::BitAnd(a, b)
            | Operation::BitOr(a, b)
            | Operation::BitXor(a, b) => {
                if let Some(&rx) = self.value_map.get(&a) {
                    if let Some(&ry) = self.value_map.get(&b) {
                        gen.push_instruction(RvInstruction::AluOp {
                            op: match op {
                                Operation::Add(_, _) => RvAluOp::Add,
                                Operation::Sub(_, _) => RvAluOp::Sub,
                                Operation::Mul(_, _) => RvAluOp::Mul,
                                Operation::Div(_, _) => RvAluOp::Div,
                                Operation::Mod(_, _) => RvAluOp::Rem,
                                Operation::Bsl(_, _) => RvAluOp::Sll,
                                Operation::Bsr(_, _) => RvAluOp::Srl,
                                Operation::Lt(_, _) => RvAluOp::Slt,
                                Operation::BitAnd(_, _) => RvAluOp::And,
                                Operation::BitOr(_, _) => RvAluOp::Or,
                                Operation::BitXor(_, _) => RvAluOp::Xor,
                                _ => unreachable!(),
                            }, rd, rx, ry });
                    }
                }
            }

            Operation::Eq(a, b) => {
                if let Some(&ry) = self.value_map.get(&a) {
                    if let Some(&rx) = self.value_map.get(&b) {
                        let d1 = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd: d1,
                            rx,
                            ry,
                        });
                        gen.push_instruction(RvInstruction::AluOpImm {
                            op: RvAluOp::Slt,
                            rd: d1,
                            rx: d1,
                            imm: 1
                        });
                        let d2 = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd: d2,
                            rx: ry,
                            ry: rx,
                        });
                        gen.push_instruction(RvInstruction::AluOpImm {
                            op: RvAluOp::Slt,
                            rd: d2,
                            rx: d2,
                            imm: 1
                        });
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::And,
                            rd,
                            rx: d1,
                            ry: d2,
                        });
                    }
                }
            }

            Operation::Ne(a, b) => {
                if let Some(&ry) = self.value_map.get(&a) {
                    if let Some(&rx) = self.value_map.get(&b) {
                        let d1 = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd: d1,
                            rx,
                            ry,
                        });
                        let d2 = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd: d2,
                            rx: ry,
                            ry: rx,
                        });
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Or,
                            rd,
                            rx: d1,
                            ry: d2,
                        });
                    }
                }
            }

            Operation::Le(a, b) => {
                if let Some(&ry) = self.value_map.get(&a) {
                    if let Some(&rx) = self.value_map.get(&b) {
                        let d1 = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd: d1,
                            rx: ry,
                            ry: rx,
                        });
                        gen.push_instruction(RvInstruction::AluOpImm {
                            op: RvAluOp::Slt,
                            rd,
                            rx: d1,
                            imm: 1
                        });
                    }
                }
            }

            Operation::Gt(a, b) => {
                if let Some(&ry) = self.value_map.get(&a) {
                    if let Some(&rx) = self.value_map.get(&b) {
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd,
                            rx,
                            ry,
                        });
                    }
                }
            }

            Operation::Ge(a, b) => {
                if let Some(&ry) = self.value_map.get(&a) {
                    if let Some(&rx) = self.value_map.get(&b) {
                        let d1 = VReg::Virtual(self.vreg_index);
                        self.vreg_index += 1;
                        gen.push_instruction(RvInstruction::AluOp {
                            op: RvAluOp::Slt,
                            rd: d1,
                            rx,
                            ry,
                        });
                        gen.push_instruction(RvInstruction::AluOpImm {
                            op: RvAluOp::Slt,
                            rd,
                            rx: d1,
                            imm: 1
                        });
                    }
                }
            }

            Operation::Phi(mapping) => {
                gen.push_instruction(RvInstruction::PhiPlaceholder {
                    rd,
                    options: mapping
                        .into_iter()
                        .filter_map(|(b, v)| {
                            if let Some(&l) = gen.label_map().get(&b) {
                                Some((Location::InternalLabel(l), v))
                            } else {
                                None
                            }
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
                    gen.push_instruction(RvInstruction::AluOp {
                        op: RvAluOp::Add,
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
                    for (label, v) in options {
                        if let Location::InternalLabel(label) = label {
                            if let Some(&rx) = self.value_map.get(&v) {
                                let labelled = &mut func.labels[label];
                                labelled.instructions.insert(
                                    labelled.instructions.len() - 1,
                                    RvInstruction::AluOp {
                                        op: RvAluOp::Add,
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
}
