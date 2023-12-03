use crate::{
    cpu::{instruction::OperationResult, Registers},
    io::RwDevice,
};

use super::{actions::spin, InstructionResult, InstructionState, Operation};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct InstructionIterator {
    pub state: InstructionState,
    pub cc: u8,
    pub operations: [Operation; 9],
    len: usize,
    cur: i8,
}

impl Default for InstructionIterator {
    fn default() -> Self {
        Self {
            state: InstructionState::default(),
            cc: 0,
            operations: [spin; 9],
            len: 0,
            cur: 0,
        }
    }
}

impl InstructionIterator {
    pub fn new(am: &[Operation], ops: &[Operation]) -> Self {
        let mut operations: [Operation; 9] = [spin; 9];

        for (i, op) in am.iter().enumerate() {
            operations[i] = *op;
        }

        for (i, op) in ops.iter().enumerate() {
            operations[i + am.len()] = *op;
        }

        Self {
            state: InstructionState::default(),
            cc: 0,
            operations,
            cur: 0,
            len: am.len() + ops.len(),
        }
    }

    pub fn clock(&mut self, reg: &mut Registers, bus: &mut dyn RwDevice) -> InstructionResult {
        self.cc = self.cc.wrapping_add(1);
        let oper = &self.operations[(self.cur - 1) as usize];
        let result = oper(reg, bus, &mut self.state);
        match result {
            OperationResult::Instant => {
                self.cc = self.cc.wrapping_sub(1);
                self.cur += 1;
                return self.clock(reg, bus);
            }
            OperationResult::Skip(count) => {
                self.cur += count;
            }
            OperationResult::SkipInstant(count) => {
                self.cc = self.cc.wrapping_sub(1);
                self.cur += 1 + count;
                return self.clock(reg, bus);
            }
            OperationResult::None => {}
        }

        let len = self.len as i8;
        match self.cur {
            x if x == len => InstructionResult::Result(self.state.addr_data, self.state.oper),
            _ => InstructionResult::Clock,
        }
    }

    pub fn waiting(&self) -> bool {
        self.cur >= (self.len as i8)
    }
}

impl Iterator for InstructionIterator {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        let len = self.len as i8;
        match self.cur {
            x if x < len => {
                self.cur += 1;
                Some(())
            }
            x if x == len => None,
            _ => None,
        }
    }
}
