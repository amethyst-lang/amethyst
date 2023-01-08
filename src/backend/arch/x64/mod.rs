/*
instructions that will be used:
cmove
cmovne
cmovae
cmova
cmovb
cmovbe
cmovg
cmovge
cmovl
cmovle
xchg
movsx
movzx
push
pop

add
sub
imul
mul
idiv
div
inc
dec
neg
cmp

and
or
xor
not

shr
shl

jmp
je
jne
ja
jae
jb
jbe
jg
jl
jge
jle
*/

use std::{collections::HashMap, fmt::Display, fs::File, io::Write};

use crate::backend::{
    ir::{Operation, Terminator, Type, Value},
    RegisterAllocator,
};

use super::{Instr, InstructionSelector, Location, VCode, VCodeGenerator, VReg};

pub const X64_REGISTER_RAX: usize = 0;
pub const X64_REGISTER_RBX: usize = 1;
pub const X64_REGISTER_RCX: usize = 2;
pub const X64_REGISTER_RDX: usize = 3;
pub const X64_REGISTER_RSI: usize = 4;
pub const X64_REGISTER_RDI: usize = 5;
pub const X64_REGISTER_RSP: usize = 6;
pub const X64_REGISTER_RBP: usize = 7;
pub const X64_REGISTER_R8: usize = 8;
pub const X64_REGISTER_R9: usize = 9;
pub const X64_REGISTER_R10: usize = 10;
pub const X64_REGISTER_R11: usize = 11;
pub const X64_REGISTER_R12: usize = 12;
pub const X64_REGISTER_R13: usize = 13;
pub const X64_REGISTER_R14: usize = 14;
pub const X64_REGISTER_R15: usize = 15;

pub enum X64Instruction {
    PhiPlaceholder {
        dest: VReg,
        options: Vec<(Location, VReg)>,
    },

    Integer {
        dest: VReg,
        value: u64,
    },

    Add {
        dest: VReg,
        source: VReg,
    },

    Mov {
        dest: VReg,
        source: VReg,
    },

    CmpZero {
        source: VReg,
    },

    Jmp {
        location: Location,
    },

    Bne {
        location: Location,
    },

    Ret,
}

impl Display for X64Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            X64Instruction::PhiPlaceholder { dest, .. } => write!(f, "phi {} ...", dest),
            X64Instruction::Integer { dest, value } => write!(f, "mov {}, {}", dest, value),
            X64Instruction::Add { dest, source } => write!(f, "add {}, {}", dest, source),
            X64Instruction::Mov { dest, source } => write!(f, "mov {}, {}", dest, source),
            X64Instruction::CmpZero { source } => write!(f, "cmp {}, 0", source),
            X64Instruction::Jmp { location } => write!(f, "jmp {}", location),
            X64Instruction::Bne { location } => write!(f, "bne {}", location),
            X64Instruction::Ret => write!(f, "ret"),
        }
    }
}

impl Instr for X64Instruction {
    fn get_regs() -> Vec<VReg> {
        vec![
            VReg::RealRegister(X64_REGISTER_RAX),
            VReg::RealRegister(X64_REGISTER_RBX),
            VReg::RealRegister(X64_REGISTER_RCX),
            VReg::RealRegister(X64_REGISTER_RDX),
            VReg::RealRegister(X64_REGISTER_RSI),
            VReg::RealRegister(X64_REGISTER_RDI),
            VReg::RealRegister(X64_REGISTER_R8),
            VReg::RealRegister(X64_REGISTER_R9),
            VReg::RealRegister(X64_REGISTER_R10),
            VReg::RealRegister(X64_REGISTER_R11),
            VReg::RealRegister(X64_REGISTER_R12),
            VReg::RealRegister(X64_REGISTER_R13),
            VReg::RealRegister(X64_REGISTER_R14),
            VReg::RealRegister(X64_REGISTER_R15),
        ]
    }

    fn collect_registers<A>(&self, alloc: &mut A)
    where
        A: RegisterAllocator,
    {
        match self {
            X64Instruction::PhiPlaceholder { .. } => (),

            X64Instruction::Integer { dest, .. } => {
                alloc.add_def(*dest);
            }

            X64Instruction::Add { dest, source } => {
                alloc.add_def(*dest);
                alloc.add_use(*dest);
                alloc.add_use(*source);
                alloc.used_simultaneously(&[*dest, *source]);
            }

            X64Instruction::Mov { dest, source } => {
                alloc.add_def(*dest);
                alloc.add_use(*source);
            }

            X64Instruction::CmpZero { source } => {
                alloc.add_use(*source);
            }

            X64Instruction::Jmp { .. } => (),

            X64Instruction::Bne { .. } => (),

            X64Instruction::Ret => (),
        }
    }

    fn apply_reg_allocs(&mut self, alloc: &HashMap<VReg, VReg>) {
        match self {
            X64Instruction::PhiPlaceholder { .. } => (),

            X64Instruction::Integer { dest, .. } => {
                if let Some(new) = alloc.get(dest) {
                    *dest = *new;
                }
            }

            X64Instruction::Add { dest, source } => {
                if let Some(new) = alloc.get(dest) {
                    *dest = *new;
                }
                if let Some(new) = alloc.get(source) {
                    *source = *new;
                }
            }

            X64Instruction::Mov { dest, source } => {
                if let Some(new) = alloc.get(dest) {
                    *dest = *new;
                }
                if let Some(new) = alloc.get(source) {
                    *source = *new;
                }
            }

            X64Instruction::CmpZero { source } => {
                if let Some(new) = alloc.get(source) {
                    *source = *new;
                }
            }

            X64Instruction::Jmp { .. } => (),

            X64Instruction::Bne { .. } => (),

            X64Instruction::Ret => (),
        }
    }

    fn emit_assembly(vcode: &VCode<Self>) {
        match File::create(format!("{}.s", vcode.name)) {
            Ok(mut file) => {
                let _ = writeln!(file, ".intel_syntax\n.global main");

                for func in vcode.functions.iter() {
                    let _ = writeln!(file, "{}:", func.name);
                    for (i, labelled) in func.labels.iter().enumerate() {
                        let _ = writeln!(file, ".L{}:", i);
                        for instruction in labelled.instructions.iter() {
                            match instruction {
                                X64Instruction::PhiPlaceholder { .. } => (),

                                X64Instruction::Integer { dest, value } => {
                                    let _ = writeln!(file, "    mov {}, {}", register(*dest), value);
                                }

                                X64Instruction::Add { dest, source } => {
                                    let _ = writeln!(file, "    add {}, {}", register(*dest), register(*source));
                                }

                                X64Instruction::Mov { dest, source } => {
                                    let _ = writeln!(file, "    mov {}, {}", register(*dest), register(*source));
                                }

                                X64Instruction::CmpZero { source } => {
                                    let _ = writeln!(file, "    cmp {}, 0", register(*source));
                                }

                                X64Instruction::Jmp { location } => {
                                    match *location {
                                        Location::InternalLabel(_) => {
                                            let _ = writeln!(file, "    jmp {}", location);
                                        }
                                        Location::Function(f) => {
                                            let _ = writeln!(file, "    jmp {}", vcode.functions[f].name);
                                        }
                                    }
                                }

                                X64Instruction::Bne { location } => {
                                    match *location {
                                        Location::InternalLabel(_) => {
                                            let _ = writeln!(file, "    jne {}", location);
                                        }
                                        Location::Function(f) => {
                                            let _ = writeln!(file, "    jne {}", vcode.functions[f].name);
                                        }
                                    }
                                }

                                X64Instruction::Ret => {
                                    let _ = writeln!(file, "    ret");
                                }
                            }
                        }

                        let _ = writeln!(file);
                    }
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
                v if v == X64_REGISTER_RAX => "%rax",
                v if v == X64_REGISTER_RBX => "%rbx",
                v if v == X64_REGISTER_RCX => "%rcx",
                v if v == X64_REGISTER_RDX => "%rdx",
                v if v == X64_REGISTER_RSI => "%rsi",
                v if v == X64_REGISTER_RDI => "%rdi",
                v if v == X64_REGISTER_RSP => "%rsp",
                v if v == X64_REGISTER_RBP => "%rbp",
                v if v == X64_REGISTER_R8 => "%r8",
                v if v == X64_REGISTER_R9 => "%r9",
                v if v == X64_REGISTER_R10 => "%r10",
                v if v == X64_REGISTER_R11 => "%r11",
                v if v == X64_REGISTER_R12 => "%r12",
                v if v == X64_REGISTER_R13 => "%r13",
                v if v == X64_REGISTER_R14 => "%r14",
                v if v == X64_REGISTER_R15 => "%r15",
                _ => unreachable!(),
            })
        }
        VReg::Virtual(_) => unreachable!(),
        VReg::Spilled => todo!(),
    }
}

#[derive(Default)]
pub struct X64Selector {
    value_map: HashMap<Value, VReg>,
    vreg_index: usize,
}

impl InstructionSelector for X64Selector {
    type Instruction = X64Instruction;

    fn select_instr(
        &mut self,
        gen: &mut VCodeGenerator<Self::Instruction, Self>,
        result: Option<Value>,
        _type_: Type,
        op: Operation,
    ) {
        let dest = result.map(|v| {
            let dest = VReg::Virtual(self.vreg_index);
            self.vreg_index += 1;
            self.value_map.insert(v, dest);
            dest
        });

        match op {
            Operation::Identity(value) => {
                if let Some(dest) = dest {
                    if let Some(&source) = self.value_map.get(&value) {
                        gen.push_instruction(X64Instruction::Mov { dest, source });
                    }
                }
            }

            Operation::Integer(_signed, mut value) => {
                // TODO: better way to do this
                while value.len() < 8 {
                    value.push(0);
                }

                let value = u64::from_le_bytes(value[..8].try_into().unwrap());
                if let Some(dest) = dest {
                    gen.push_instruction(X64Instruction::Integer { dest, value });
                }
            }

            Operation::Add(a, b) => {
                if let Some(dest) = dest {
                    if let Some(&source) = self.value_map.get(&a) {
                        gen.push_instruction(X64Instruction::Mov { dest, source });
                        if let Some(&source) = self.value_map.get(&b) {
                            gen.push_instruction(X64Instruction::Add { dest, source });
                        }
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
                if let Some(dest) = dest {
                    gen.push_instruction(X64Instruction::PhiPlaceholder {
                        dest,
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
                gen.push_instruction(X64Instruction::Ret);
            }

            Terminator::Return(v) => {
                if let Some(&source) = self.value_map.get(&v) {
                    gen.push_instruction(X64Instruction::Mov {
                        dest: VReg::RealRegister(X64_REGISTER_RAX),
                        source,
                    });
                }

                gen.push_instruction(X64Instruction::Ret);
            }

            Terminator::Jump(label) => {
                if let Some(&label) = gen.label_map().get(&label) {
                    gen.push_instruction(X64Instruction::Jmp {
                        location: Location::InternalLabel(label),
                    });
                }
            }

            Terminator::Branch(v, l1, l2) => {
                if let Some(&source) = self.value_map.get(&v) {
                    gen.push_instruction(X64Instruction::CmpZero { source });
                    if let Some(&l1) = gen.label_map().get(&l1) {
                        gen.push_instruction(X64Instruction::Bne {
                            location: Location::InternalLabel(l1),
                        });
                    }
                    if let Some(&l2) = gen.label_map().get(&l2) {
                        gen.push_instruction(X64Instruction::Jmp {
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
                    if let X64Instruction::PhiPlaceholder { .. } = instr {
                        v.push((i, j));
                    }
                }
            }

            for (label_index, instr_index) in v.into_iter().rev() {
                let phi = func.labels[label_index].instructions.remove(instr_index);
                if let X64Instruction::PhiPlaceholder { dest, options } = phi {
                    for (label, source) in options {
                        if let Location::InternalLabel(label) = label {
                            let labelled = &mut func.labels[label];
                            labelled.instructions.insert(
                                labelled.instructions.len() - 1,
                                X64Instruction::Mov { dest, source },
                            );
                        }
                    }
                }
            }
        }
    }
}
