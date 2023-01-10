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
pub const URCL_REGISTER_R2: usize = 4;
pub const URCL_REGISTER_R3: usize = 5;
pub const URCL_REGISTER_R4: usize = 6;
pub const URCL_REGISTER_R5: usize = 7;
pub const URCL_REGISTER_R6: usize = 8;
pub const URCL_REGISTER_R7: usize = 9;
pub const URCL_REGISTER_R8: usize = 10;

pub enum UrclInstruction {
    PhiPlaceholder {
        rd: VReg,
        options: Vec<(Location, VReg)>,
    },

    Imm {
        rd: VReg,
        value: u64,
    },

    Add {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Sub {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Mul {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Div {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Mod {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Bsl {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Bsr {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Bre {
        location: Location,
        rx: VReg,
        ry: VReg,
    },

    Bne {
        location: Location,
        rx: VReg,
        ry: VReg,
    },

    Brl {
        location: Location,
        rx: VReg,
        ry: VReg,
    },

    Ble {
        location: Location,
        rx: VReg,
        ry: VReg,
    },

    Brg {
        location: Location,
        rx: VReg,
        ry: VReg,
    },

    Bge {
        location: Location,
        rx: VReg,
        ry: VReg,
    },

    Jmp {
        location: Location,
    },

    And {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Or {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Xor {
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    Lod {
        rd: VReg,
        rx: VReg,
    },

    Str {
        rd: VReg,
        rx: VReg,
    },
}

impl Display for UrclInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UrclInstruction::PhiPlaceholder { rd, .. } => write!(f, "phi {} ...", rd),

            UrclInstruction::Imm { rd, value } => write!(f, "imm {} {}", rd, value),

            UrclInstruction::Add { rd, rx, ry } => write!(f, "add {} {} {}", rd, rx, ry),

            UrclInstruction::Jmp { location } => write!(f, "jmp {}", location),

            UrclInstruction::Bne { rx, ry, location } => write!(f, "bne {} {} {}", location, rx, ry),

            UrclInstruction::Bre { location, rx, ry } => write!(f, "bre {} {} {}", location, rx, ry),

            UrclInstruction::Brl { location, rx, ry } => write!(f, "brl {} {} {}", location, rx, ry),
            
            UrclInstruction::Ble { location, rx, ry } => write!(f, "ble {} {} {}", location, rx, ry),

            UrclInstruction::Brg { location, rx, ry } => write!(f, "brg {} {} {}", location, rx, ry),

            UrclInstruction::Bge { location, rx, ry } => write!(f, "bge {} {} {}", location, rx, ry),

            UrclInstruction::Sub { rd, rx, ry } => write!(f, "sub {} {} {}", rd, rx, ry),

            UrclInstruction::And { rd, rx, ry } => write!(f, "and {} {} {}", rd, rx, ry),

            UrclInstruction::Or { rd, rx, ry } => write!(f, "or {} {} {}", rd, rx, ry),

            UrclInstruction::Xor { rd, rx, ry } => write!(f, "xor {} {} {}", rd, rx, ry),

            UrclInstruction::Mod { rd, rx, ry } => write!(f, "mod {} {} {}", rd, rx, ry),

            UrclInstruction::Div { rd, rx, ry } => write!(f, "div {} {} {}", rd, rx, ry),

            UrclInstruction::Mul { rd, rx, ry } => write!(f, "mul {} {} {}", rd, rx, ry),

            UrclInstruction::Bsl { rd, rx, ry } => write!(f, "bsl {} {} {}", rd, rx, ry),

            UrclInstruction::Bsr { rd, rx, ry } => write!(f, "bsr {} {} {}", rd, rx, ry),

            UrclInstruction::Lod { rd,  rx } => write!(f, "lod {} {}", rd, rx),

            UrclInstruction::Str { rd, rx } => write!(f, "str {} {}", rd, rx),
        }
    }
}

impl Instr for UrclInstruction {
    fn get_regs() -> Vec<VReg> {
        vec![
            VReg::RealRegister(URCL_REGISTER_R1),
            VReg::RealRegister(URCL_REGISTER_R2),
            VReg::RealRegister(URCL_REGISTER_R3),
            VReg::RealRegister(URCL_REGISTER_R4),
            VReg::RealRegister(URCL_REGISTER_R5),
            VReg::RealRegister(URCL_REGISTER_R6),
            VReg::RealRegister(URCL_REGISTER_R7),
            VReg::RealRegister(URCL_REGISTER_R8),
        ]
    }

    fn collect_registers<A>(&self, alloc: &mut A)
    where
        A: RegisterAllocator,
    {
        match self {
            UrclInstruction::PhiPlaceholder { .. } => (),

            UrclInstruction::Imm { rd, .. } => {
                alloc.add_def(*rd);
            }

            UrclInstruction::Add { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Jmp { .. } => (),

            UrclInstruction::Lod { rd, rx } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
            },

            UrclInstruction::Str { rd, rx } => {
                alloc.add_use(*rd);
                alloc.add_use(*rx);
            },

            UrclInstruction::Brl { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Ble { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Brg { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Bge { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Bre { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Bne { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Sub { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Mul { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Div { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Mod { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::And { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Or { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Xor { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Bsl { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            UrclInstruction::Bsr { rd, rx, ry } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }
        }
    }




    fn apply_reg_allocs(&mut self, alloc: &HashMap<VReg, VReg>) {
        match self {
            UrclInstruction::PhiPlaceholder { .. } => (),

            UrclInstruction::Imm { rd, .. } => {
                apply_alloc(alloc, rd);
            }

            UrclInstruction::Add { rd, rx, ry } => {
                apply_alloc(alloc, rd);
                apply_alloc(alloc, rx);
                apply_alloc(alloc, ry);
            }

            UrclInstruction::Jmp { .. } => (),

            UrclInstruction::Bne { rx, ry, .. } => {
                apply_alloc(alloc, rx);
                apply_alloc(alloc, ry);
            }

            UrclInstruction::Lod { rd, rx } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
            },

            UrclInstruction::Str { rd, rx } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
            },

            UrclInstruction::Brl { rx, ry, .. } => {

            }
        }
    }

    fn mandatory_transforms(vcode: &mut VCode<Self>) {
        // TODO
    }

    fn emit_assembly(vcode: &VCode<Self>) {
        match File::create(format!("{}.s", vcode.name)) {
            Ok(mut file) => {
                for func in vcode.functions.iter() {
                    let _ = writeln!(file, ".{}", func.name);
                    for (i, labelled) in func.labels.iter().enumerate() {
                        let _ = writeln!(file, ".L{}:", i);
                        for instruction in labelled.instructions.iter() {
                            match instruction {
                                UrclInstruction::PhiPlaceholder { .. } => (),

                                UrclInstruction::Imm { rd, value } => {
                                    let _ = writeln!(file, "    imm {} {}", register(*rd), value);
                                }

                                UrclInstruction::Add { rd, rx, ry } => {
                                    let _ = writeln!(file, "    add {} {} {}", register(*rd), register(*rx), register(*ry));
                                }

                                UrclInstruction::Jmp { location } => {
                                    match *location {
                                        Location::InternalLabel(_) => {
                                            let _ = writeln!(file, "    jmp .{}", location);
                                        }
                                        Location::Function(f) => {
                                            let _ = writeln!(file, "    jmp .{}", vcode.functions[f].name);
                                        }
                                    }
                                }

                                UrclInstruction::Bne { rx, ry, location } => {
                                    match *location {
                                        Location::InternalLabel(_) => {
                                            let _ = writeln!(file, "    bne {} {} {}", location, register(*rx), register(*ry) );
                                        }
                                        Location::Function(f) => {
                                            let _ = writeln!(file, "    bne {} {} {}", vcode.functions[f].name, register(*rx), register(*ry) );
                                        }
                                    }
                                }

                                UrclInstruction::Lod { rd,  rx } => {
                                    let _ = writeln!(file, "    lod {} {}", rd, rx);
                                }

                                UrclInstruction::Str { rd, rx} => {
                                    let _ = writeln!(file, "    str {} {}", rd, rx);
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

fn apply_alloc(alloc: &HashMap<VReg, VReg>, reg: &mut VReg) {
    if let Some(new) = alloc.get(reg) {
        *reg = *new;
    }
}

fn register(reg: VReg) -> String {
    match reg {
        VReg::RealRegister(reg) => {
            String::from(match reg {
                v if v == URCL_REGISTER_ZERO => "r0",
                v if v == URCL_REGISTER_PC => "pc",
                v if v == URCL_REGISTER_SP => "sp",
                v if v == URCL_REGISTER_R1 => "r1",
                v if v == URCL_REGISTER_R2 => "r2",
                v if v == URCL_REGISTER_R3 => "r3",
                v if v == URCL_REGISTER_R4 => "r4",
                v if v == URCL_REGISTER_R5 => "r5",
                v if v == URCL_REGISTER_R6 => "r6",
                v if v == URCL_REGISTER_R7 => "r7",
                v if v == URCL_REGISTER_R8 => "r8",
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
    type Instruction = UrclInstruction;

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

            None => VReg::RealRegister(URCL_REGISTER_ZERO),
        };

        match op {
            Operation::Identity(value) => {
                if let Some(&rx) = self.value_map.get(&value) {
                    gen.push_instruction(UrclInstruction::Add { rd, rx, ry: VReg::RealRegister(URCL_REGISTER_ZERO) });
                }
            }

            Operation::Integer(_signed, mut value) => {
                // TODO: better way to do this
                while value.len() < 8 {
                    value.push(0);
                }
                let value = u64::from_le_bytes(value[..8].try_into().unwrap());
                gen.push_instruction(UrclInstruction::Imm { rd, value });
            }

            Operation::Add(a, b) => {
                if let Some(&rx) = self.value_map.get(&a) {
                    if let Some(&ry) = self.value_map.get(&b) {
                        gen.push_instruction(UrclInstruction::Add { rd, rx, ry });
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
                gen.push_instruction(UrclInstruction::PhiPlaceholder {
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
                gen.push_instruction(UrclInstruction::Ret);
            }

            Terminator::Return(v) => {
                if let Some(&rx) = self.value_map.get(&v) {
                    gen.push_instruction(UrclInstruction::Add {
                        rd: VReg::RealRegister(RV_REGISTER_A0),
                        rx,
                        ry: VReg::RealRegister(RV_REGISTER_ZERO),
                    });
                }

                gen.push_instruction(UrclInstruction::Ret);
            }

            Terminator::Jump(label) => {
                if let Some(&label) = gen.label_map().get(&label) {
                    gen.push_instruction(UrclInstruction::Jal {
                        rd: VReg::RealRegister(RV_REGISTER_ZERO),
                        location: Location::InternalLabel(label),
                    });
                }
            }

            Terminator::Branch(v, l1, l2) => {
                if let Some(&rx) = self.value_map.get(&v) {
                    if let Some(&l1) = gen.label_map().get(&l1) {
                        gen.push_instruction(UrclInstruction::Bne {
                            rx,
                            ry: VReg::RealRegister(RV_REGISTER_ZERO),
                            location: Location::InternalLabel(l1),
                        });
                    }
                    if let Some(&l2) = gen.label_map().get(&l2) {
                        gen.push_instruction(UrclInstruction::Jal {
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
                    if let UrclInstruction::PhiPlaceholder { .. } = instr {
                        v.push((i, j));
                    }
                }
            }

            for (label_index, instr_index) in v.into_iter().rev() {
                let phi = func.labels[label_index].instructions.remove(instr_index);
                if let UrclInstruction::PhiPlaceholder { rd, options } = phi {
                    for (label, rx) in options {
                        if let Location::InternalLabel(label) = label {
                            let labelled = &mut func.labels[label];
                            labelled.instructions.insert(
                                labelled.instructions.len() - 1,
                                UrclInstruction::Add {
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
