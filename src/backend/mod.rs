use std::collections::HashMap;

use self::arch::{Instr, VReg};

pub mod arch;
pub mod ir;
pub mod regalloc;
pub mod sexpr_lowering;

pub trait RegisterAllocator: Default {
    fn add_use(&mut self, reg: VReg);

    fn add_def(&mut self, reg: VReg);

    fn force_same(&mut self, reg: VReg, constraint: VReg);

    fn next_live_step(&mut self);

    fn allocate_regs<I>(self) -> HashMap<VReg, VReg>
    where
        I: Instr;
}
