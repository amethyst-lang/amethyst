use std::{collections::{HashMap, HashSet}, ops::Range};

use super::{
    arch::{Instr, VReg},
    RegisterAllocator,
};

#[derive(Default)]
pub struct RegAlloc {
    live_step_count: usize,
    nodes: Vec<RegNode>,
    edges: HashSet<(usize, usize)>,
}

struct RegNode {
    reg: VReg,
    colour: Option<VReg>,
    live_range: Range<usize>,
    usage_count: usize,
    edge_count: usize,
    in_stack: bool,
}

impl RegisterAllocator for RegAlloc {
    fn add_use(&mut self, reg: VReg) {
        if let Some(node) = self.nodes.iter_mut().find(|v| v.reg == reg) {
            node.usage_count += 1;
            node.live_range.end = self.live_step_count;
        } else if let VReg::Virtual(_) = reg {
            self.nodes.push(RegNode {
                reg,
                colour: None,
                live_range: self.live_step_count..self.live_step_count,
                usage_count: 1,
                edge_count: 0,
                in_stack: false,
            });
        }
    }

    fn add_def(&mut self, reg: VReg) {
        match reg {
            VReg::RealRegister(_) => (),

            VReg::Virtual(_) => {
                if let Some(node) = self.nodes.iter_mut().find(|v| v.reg == reg) {
                    node.live_range.end = self.live_step_count;
                } else {
                    self.nodes.push(RegNode {
                        reg,
                        colour: None,
                        live_range: self.live_step_count..self.live_step_count,
                        usage_count: 0,
                        edge_count: 0,
                        in_stack: false,
                    });
                }
            }

            VReg::Spilled(_) => (),
        }
    }

    fn force_same(&mut self, reg: VReg, constraint: VReg) {
        match constraint {
            VReg::RealRegister(_) => {
                for node in self.nodes.iter_mut() {
                    if node.reg == reg && node.colour.is_none() {
                        node.colour = Some(constraint);
                    }
                }
            }

            VReg::Virtual(_) => unreachable!(),
            VReg::Spilled(_) => unreachable!(),
        }
    }

    fn next_live_step(&mut self) {
        self.live_step_count += 1;
    }

    fn allocate_regs<I>(mut self) -> HashMap<VReg, VReg>
    where
        I: Instr,
    {
        for (i, ni) in self.nodes.iter().enumerate() {
            for (j, nj) in self.nodes.iter().enumerate().skip(i) {
                if ni.live_range.start <= nj.live_range.end && nj.live_range.start <= ni.live_range.end {
                    self.edges.insert((i, j));
                }
            }
        }

        for &(a, b) in self.edges.iter() {
            self.nodes[a].edge_count += 1;
            self.nodes[b].edge_count += 1;
        }

        let mut stack = Vec::new();
        let regs = I::get_regs();
        while stack.len() != self.nodes.len() {
            let mut pushed_to_stack = false;
            for (i, node) in self.nodes.iter_mut().enumerate() {
                if !node.in_stack
                    && (node.edge_count < regs.len() && node.colour.is_none())
                {
                    node.in_stack = true;
                    stack.push(i);
                    pushed_to_stack = true;
                }
            }

            if !pushed_to_stack {
                let mut min = usize::MAX;
                let mut index = 0;
                for (i, node) in self.nodes.iter().enumerate() {
                    if node.usage_count < min && !node.in_stack && node.colour.is_none() {
                        min = node.usage_count;
                        index = i;
                    }
                }

                if min != usize::MAX {
                    self.nodes[index].in_stack = true;
                    stack.push(index);
                } else {
                    for (i, node) in self.nodes.iter().enumerate() {
                        if node.colour.is_none() {
                            self.nodes[i].in_stack = true;
                            stack.push(i);
                            break;
                        }
                    }
                }
            }

            if let Some(&i) = stack.last() {
                for &(a, b) in self.edges.iter() {
                    if a == i {
                        self.nodes[b].edge_count -= 1;
                    } else if b == i {
                        self.nodes[a].edge_count -= 1;
                    }
                }
            }
        }

        let mut edge_registers = HashSet::new();
        while let Some(i) = stack.pop() {
            edge_registers.clear();
            for &(a, b) in self.edges.iter() {
                if a == i {
                    let reg = &self.nodes[b];
                    if let Some(reg) = reg.colour {
                        edge_registers.insert(reg);
                    }
                } else if b == i {
                    let reg = &self.nodes[a];
                    if let Some(reg) = reg.colour {
                        edge_registers.insert(reg);
                    }
                }
            }

            let mut spilled = true;
            for &reg in regs.iter() {
                if !edge_registers.contains(&reg) {
                    self.nodes[i].colour = Some(reg);
                    spilled = false;
                }
            }

            if spilled {
                let mut s = 0;
                while {
                    let reg = VReg::Spilled(s);
                    if !edge_registers.contains(&reg) {
                        self.nodes[i].colour = Some(reg);
                        false
                    } else {
                        s += 1;
                        true
                    }
                } {}
            }
        }

        let mut map = HashMap::new();
        for node in self.nodes {
            if let Some(colour) = node.colour {
                map.insert(node.reg, colour);
            }
        }

        map
    }
}
