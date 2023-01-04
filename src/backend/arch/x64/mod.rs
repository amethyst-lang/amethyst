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

use crate::backend::ir::{Value, Operation, Terminator, Type};

use super::{InstructionSelector, VCodeGenerator, VCode};

pub enum X64Instruction {
    Add {
    },

    Mov {
    },

    Jmp {
    },

    Branch {
    },

    Ret {
    },
}

#[derive(Default)]
pub struct X64Selector;

impl InstructionSelector for X64Selector {
    type Instruction = X64Instruction;

    fn select_instr(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>, result: Option<Value>, type_: Type, op: Operation) {
        todo!()
    }

    fn select_term(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>, op: Terminator) {
        todo!()
    }

    fn post_generation(&mut self, vcode: &mut VCode<Self::Instruction>) {
        todo!()
    }
}
