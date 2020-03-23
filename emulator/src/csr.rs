
use crate::machine::RiscvMachineContext;
use corona_riscv::isa::{
  Register,
  CSRFunction
};
use std::collections::HashMap;

pub enum CSRRegister {
  MEPC = 0x341
}

pub type CSRIndex = u16;
pub type CSRValue = u64;

#[derive(Debug)]
pub struct CSR {
  registers: HashMap<CSRIndex, CSRValue>
}

impl CSR {
  pub fn new() -> Self {
    Self { 
      registers: HashMap::new()
    }
  }

  pub fn get(&self, register: CSRRegister) -> CSRValue {
    let index = register as CSRIndex;
    self.get_index(index)
  }

  pub fn get_index(&self, index: CSRIndex) -> CSRValue {
    *self.registers.get(&index).unwrap_or(&0)
  }

  pub fn set(&mut self, register: CSRRegister, value: CSRValue) {
    let index = register as CSRIndex;
    self.set_index(index, value)
  }

  pub fn set_index(&mut self, index: CSRIndex, value: CSRValue) {
    self.registers.insert(index, value);
  }

  

  pub fn execute(&mut self, function: CSRFunction, index: CSRIndex, source: Register, dest: Register, state: &mut RiscvMachineContext) {
    match function {
      CSRFunction::CSRRW => {
        let old = self.get_index(index);
        state.registers.set(dest, old);

        if source != Register::Zero {
          let new = state.registers.get(source);
          self.set_index(index, new);
        }
      },

      CSRFunction::CSRRWI => {
        let old = self.get_index(index);
        state.registers.set(dest, old);

        if source != Register::Zero {
          let new = source.encode() as CSRValue;
          self.set_index(index, new);
        }
      },

      CSRFunction::CSRRS => {
        let old = self.get_index(index);
        state.registers.set(dest, old);

        let mask = state.registers.get(dest);
        let new = old | mask;

        self.set_index(index, new);
      },

      CSRFunction::CSRRSI => {
        let old = self.get_index(index);
        state.registers.set(dest, old);

        let mask = dest.encode() as CSRValue;
        let new = old | mask;
        self.set_index(index, new);
      },

      CSRFunction::CSRRC => {
        let old = self.get_index(index);
        state.registers.set(dest, old);

        let mask = state.registers.get(dest);
        let new = old & !mask;
        self.set_index(index, new);
      },

      CSRFunction::CSRRCI => {
        let old = self.get_index(index);
        state.registers.set(dest, old);

        let mask = dest.encode() as CSRValue;
        let new = old & !mask;
        self.set_index(index, new);

      }
    }
  }
}
