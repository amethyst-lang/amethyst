/*
instructions that will be used:
(cmov cannot take destination memory operand; everything else can)
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

pub enum X64CMovOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for X64CMovOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            X64CMovOp::Eq => write!(f, "e"),
            X64CMovOp::Ne => write!(f, "ne"),
            X64CMovOp::Lt => write!(f, "l"),
            X64CMovOp::Le => write!(f, "le"),
            X64CMovOp::Gt => write!(f, "g"),
            X64CMovOp::Ge => write!(f, "ge"),
        }
    }
}

pub enum X64AluOp {
    Add,
    Sub,
    IMul,
    And,
    Or,
    Xor,
}

impl Display for X64AluOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            X64AluOp::Add => write!(f, "add"),
            X64AluOp::Sub => write!(f, "sub"),
            X64AluOp::And => write!(f, "and"),
            X64AluOp::Or => write!(f, "or"),
            X64AluOp::Xor => write!(f, "xor"),
            X64AluOp::IMul => write!(f, "imul"),
        }
    }
}

pub enum X64Instruction {
    PhiPlaceholder {
        dest: VReg,
        options: Vec<(Location, Value)>,
    },

    Integer {
        dest: VReg,
        value: u64,
    },

    AluOp {
        op: X64AluOp,
        dest: VReg,
        source: VReg,
    },

    DivRem {
        rem: VReg, // RDX
        div: VReg, // RAX
        source: VReg,
    },

    Mov {
        dest: VReg,
        source: VReg,
    },

    CMov {
        op: X64CMovOp,
        dest: VReg,
        source: VReg,
    },

    Cmp {
        a: VReg,
        b: VReg,
    },

    CmpZero {
        source: VReg,
    },

    Jmp {
        location: Location,
    },

    Jne {
        location: Location,
    },

    Ret,

    Push {
        source: VReg,
    },

    Pop {
        dest: VReg,
    },
}

impl Display for X64Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            X64Instruction::PhiPlaceholder { dest, .. } => write!(f, "phi {} ...", dest),
            X64Instruction::Integer { dest, value } => write!(f, "mov {}, {}", dest, value),
            X64Instruction::AluOp { op, dest, source } => write!(f, "{} {}, {}", op, dest, source),
            X64Instruction::DivRem { source, .. } => write!(f, "idiv {}", source),
            X64Instruction::Mov { dest, source } => write!(f, "mov {}, {}", dest, source),
            X64Instruction::CMov { op, dest, source } => write!(f, "cmov{} {}, {}", op, dest, source),
            X64Instruction::Cmp{ a, b } => write!(f, "cmp {}, {}", a, b),
            X64Instruction::CmpZero { source } => write!(f, "cmp {}, 0", source),
            X64Instruction::Jmp { location } => write!(f, "jmp {}", location),
            X64Instruction::Jne { location } => write!(f, "bne {}", location),
            X64Instruction::Ret => write!(f, "ret"),
            X64Instruction::Push { source } => write!(f, "push {}", source),
            X64Instruction::Pop { dest } => write!(f, "pop {}", dest),
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

            X64Instruction::AluOp { dest, source, .. } => {
                alloc.add_def(*dest);
                alloc.add_use(*dest);
                alloc.add_use(*source);
            }

            X64Instruction::DivRem { rem, div, source } => {
                alloc.add_def(*rem);
                alloc.add_use(*rem);
                alloc.force_same(*rem, VReg::RealRegister(X64_REGISTER_RDX));
                alloc.add_def(*div);
                alloc.add_use(*div);
                alloc.force_same(*div, VReg::RealRegister(X64_REGISTER_RAX));
                alloc.add_use(*source);
            }

            X64Instruction::Mov { dest, source } => {
                alloc.add_def(*dest);
                alloc.add_use(*source);
            }

            X64Instruction::CMov { dest, source, .. } => {
                alloc.add_def(*dest);
                alloc.add_use(*source);
            }

            X64Instruction::Cmp { a, b } => {
                alloc.add_use(*a);
                alloc.add_use(*b);
            }

            X64Instruction::CmpZero { source } => {
                alloc.add_use(*source);
            }

            X64Instruction::Jmp { .. } => (),

            X64Instruction::Jne { .. } => (),

            X64Instruction::Ret => (),

            X64Instruction::Push { .. } => (),

            X64Instruction::Pop { .. } => (),
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

            X64Instruction::AluOp { dest, source, .. } => {
                if let Some(new) = alloc.get(dest) {
                    *dest = *new;
                }
                if let Some(new) = alloc.get(source) {
                    *source = *new;
                }
            }

            X64Instruction::DivRem { rem, div, source } => {
                if let Some(new) = alloc.get(rem) {
                    *rem = *new;
                }
                if let Some(new) = alloc.get(div) {
                    *div = *new;
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

            X64Instruction::Cmp { a, b } => {
                if let Some(new) = alloc.get(a) {
                    *a = *new;
                }
                if let Some(new) = alloc.get(b) {
                    *b = *new;
                }
            }

            X64Instruction::CMov { dest, source, .. } => {
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

            X64Instruction::Jne { .. } => (),

            X64Instruction::Ret => (),

            X64Instruction::Push { .. } => (),

            X64Instruction::Pop { .. } => (),
        }
    }

    fn mandatory_transforms(vcode: &mut VCode<Self>) {
        for func in vcode.functions.iter_mut() {
            for labelled in func.labels.iter_mut() {
                let mut swaps = Vec::new();
                for (i, instruction) in labelled.instructions.iter().enumerate() {
                    match instruction {
                        X64Instruction::AluOp { dest, source, .. }
                        | X64Instruction::Mov { dest, source } => {
                            if let (VReg::Spilled(_), VReg::Spilled(_)) = (*dest, *source) {
                                swaps.push((i, *source));
                            }
                        }

                        _ => (),
                    }
                }

                for (index, source) in swaps.into_iter().rev() {
                    labelled.instructions.insert(index, X64Instruction::Push {
                        source: VReg::RealRegister(X64_REGISTER_RAX),
                    });

                    labelled.instructions.insert(index + 1, X64Instruction::Mov {
                        dest: VReg::RealRegister(X64_REGISTER_RAX),
                        source,
                    });

                    match &mut labelled.instructions[index + 2] {
                        X64Instruction::AluOp { source, .. }
                        | X64Instruction::Mov { source, .. } => {
                            *source = VReg::RealRegister(X64_REGISTER_RAX);
                        }

                        _ => (),
                    }

                    labelled.instructions.insert(index + 3, X64Instruction::Pop {
                        dest: VReg::RealRegister(X64_REGISTER_RAX),
                    });
                }
            }
        }
    }

    fn emit_assembly(vcode: &VCode<Self>) {
        match File::create(format!("{}.s", vcode.name)) {
            Ok(mut file) => {
                let _ = writeln!(file, ".intel_syntax\n.global main");

                for func in vcode.functions.iter() {
                    let _ = writeln!(file, "{}:\n    push %rbp\n    mov %rbp, %rsp", func.name);
                    for (i, labelled) in func.labels.iter().enumerate() {
                        let _ = writeln!(file, ".L{}:", i);
                        for instruction in labelled.instructions.iter() {
                            match instruction {
                                X64Instruction::PhiPlaceholder { .. } => (),

                                X64Instruction::Integer { dest, value } => {
                                    let _ = writeln!(file, "    mov {}, {}", register(*dest), value);
                                }

                                X64Instruction::AluOp { op, dest, source } => {
                                    let _ = writeln!(file, "    {} {}, {}", op, register(*dest), register(*source));
                                }

                                X64Instruction::DivRem { source, .. } => {
                                    let _ = writeln!(file, "    div {}", register(*source));
                                }

                                X64Instruction::Mov { dest, source } => {
                                    let _ = writeln!(file, "    mov {}, {}", register(*dest), register(*source));
                                }

                                X64Instruction::CMov { op, dest, source } => {
                                    let _ = writeln!(file, "    cmov{} {}, {}", op, register(*dest), register(*source));
                                }

                                X64Instruction::Cmp { a, b } => {
                                    let _ = writeln!(file, "    cmp {}, {}", register(*a), register(*b));
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

                                X64Instruction::Jne { location } => {
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
                                    let _ = writeln!(file, "    mov %rsp, %rbp\n    pop %rbp\n    ret");
                                }

                                X64Instruction::Push { source } => {
                                    let _ = writeln!(file, "    push {}", register(*source));
                                }

                                X64Instruction::Pop { dest } => {
                                    let _ = writeln!(file, "    pop {}", register(*dest));
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

// TODO: add width stuff
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
        VReg::Spilled(s) => format!("qword ptr [%rbp - {}]", 8 * s),
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

            Operation::Add(a, b)
            | Operation::Sub(a, b)
            | Operation::Mul(a, b)
            | Operation::BitAnd(a, b)
            | Operation::BitOr(a, b)
            | Operation::BitXor(a, b) => {
                if let Some(dest) = dest {
                    if let Some(&source) = self.value_map.get(&a) {
                        gen.push_instruction(X64Instruction::Mov { dest, source });
                        if let Some(&source) = self.value_map.get(&b) {
                            gen.push_instruction(X64Instruction::AluOp { op: match op {
                                Operation::Add(_, _) => X64AluOp::Add,
                                Operation::Sub(_, _) => X64AluOp::Sub,
                                Operation::Mul(_, _) => X64AluOp::IMul,
                                Operation::BitAnd(_, _) => X64AluOp::And,
                                Operation::BitOr(_, _) => X64AluOp::Or,
                                Operation::BitXor(_, _) => X64AluOp::Xor,
                                _ => unreachable!(),
                            }, dest, source });
                        }
                    }
                }
            }

            Operation::Div(a, b) => {
                if let Some(dest) = dest {
                    if let Some(&a) = self.value_map.get(&a) {
                        if let Some(&source) = self.value_map.get(&b) {
                            let rem = VReg::Virtual(self.vreg_index);
                            self.vreg_index += 1;
                            gen.push_instruction(X64Instruction::Mov {
                                dest,
                                source: a,
                            });
                            gen.push_instruction(X64Instruction::Integer {
                                dest: rem,
                                value: 0,
                            });
                            gen.push_instruction(X64Instruction::DivRem {
                                rem,
                                div: dest,
                                source,
                            });
                        }
                    }
                }
            }

            Operation::Mod(a, b) => {
                if let Some(dest) = dest {
                    if let Some(&a) = self.value_map.get(&a) {
                        if let Some(&source) = self.value_map.get(&b) {
                            let div = VReg::Virtual(self.vreg_index);
                            self.vreg_index += 1;
                            gen.push_instruction(X64Instruction::Mov {
                                dest: div,
                                source: a,
                            });
                            gen.push_instruction(X64Instruction::Integer {
                                dest,
                                value: 0,
                            });
                            gen.push_instruction(X64Instruction::DivRem {
                                rem: dest,
                                div,
                                source,
                            });
                        }
                    }
                }
            }

            Operation::Bsl(_, _)
            | Operation::Bsr(_, _) => todo!(),

            Operation::Eq(a, b) 
            | Operation::Ne(a, b)
            | Operation::Lt(a, b)
            | Operation::Le(a, b)
            | Operation::Gt(a, b)
            | Operation::Ge(a, b) => {
                if let Some(dest) = dest {
                    if let Some(&a) = self.value_map.get(&a) {
                        if let Some(&b) = self.value_map.get(&b) {
                            gen.push_instruction(X64Instruction::Integer {
                                dest,
                                value: 0,
                            });
                            gen.push_instruction(X64Instruction::Cmp {
                                a,
                                b,
                            });
                            let source = VReg::Virtual(self.vreg_index);
                            self.vreg_index += 1;
                            gen.push_instruction(X64Instruction::Integer {
                                dest: source,
                                value: 1,
                            });
                            gen.push_instruction(X64Instruction::CMov {
                                op: match op {
                                    Operation::Eq(_, _) => X64CMovOp::Eq,
                                    Operation::Ne(_, _) => X64CMovOp::Ne,
                                    Operation::Lt(_, _) => X64CMovOp::Lt,
                                    Operation::Le(_, _) => X64CMovOp::Le,
                                    Operation::Gt(_, _) => X64CMovOp::Gt,
                                    Operation::Ge(_, _) => X64CMovOp::Ge,
                                    _ => unreachable!(),
                                },
                                dest,
                                source,
                            });
                        }
                    }
                }
            }

            Operation::Phi(mapping) => {
                if let Some(dest) = dest {
                    gen.push_instruction(X64Instruction::PhiPlaceholder {
                        dest,
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
                        gen.push_instruction(X64Instruction::Jne {
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
                    for (label, v) in options {
                        if let Location::InternalLabel(label) = label {
                            if let Some(&source) = self.value_map.get(&v) {
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
}
