
use crate::machine::RiscvMachineContext;
use riscy_isa::{
  Register,
  CSRFunction
};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum CSRRegister {
  SupervisorAddressTranslationAndProtection,
  MachineStatus,
  MachineExceptionDelegation,
  MachineInterruptDelegation,
  MachineInterruptEnable,
  MachineTrapHandlerBaseAddress,
  MachineExceptionProgramCounter,
  PhysMemProtectionConfig0,
  PhysMemProtectionAddr0,
  MachineHartId, 
}

impl From<u32> for CSRRegister {
  fn from(value: u32) -> Self {
    match value {
      0x180 => CSRRegister::SupervisorAddressTranslationAndProtection,
      0x300 => CSRRegister::MachineStatus,
      0x302 => CSRRegister::MachineExceptionDelegation,
      0x303 => CSRRegister::MachineInterruptDelegation,
      0x304 => CSRRegister::MachineInterruptEnable,
      0x305 => CSRRegister::MachineTrapHandlerBaseAddress,
      0x341 => CSRRegister::MachineExceptionProgramCounter,
      0x3a0 => CSRRegister::PhysMemProtectionConfig0,
      0x3b0 => CSRRegister::PhysMemProtectionAddr0,
      0xf14 => CSRRegister::MachineHartId,

      _ => unimplemented!("CSR {:#08x}", value)
    }
  }
}

impl Into<u32> for CSRRegister {
  fn into(self) -> u32 { 
    match self {
      CSRRegister::SupervisorAddressTranslationAndProtection => 0x180,
      CSRRegister::MachineStatus => 0x300,
      CSRRegister::MachineExceptionDelegation => 0x302,
      CSRRegister::MachineInterruptDelegation => 0x303,
      CSRRegister::MachineInterruptEnable => 0x304,
      CSRRegister::MachineTrapHandlerBaseAddress => 0x305,
      CSRRegister::MachineExceptionProgramCounter => 0x341,
      CSRRegister::PhysMemProtectionConfig0 => 0x3a0,
      CSRRegister::PhysMemProtectionAddr0 => 0x3b0,
      CSRRegister::MachineHartId => 0xf14,
    }
  }
}

type CSRValue = u64;

#[derive(Debug, Default)]
pub struct CSR {
  registers: HashMap<CSRRegister, CSRValue>
}

impl CSR {
  pub fn get(&self, register: CSRRegister) -> CSRValue {
    let value = *self.registers.get(&register).unwrap_or(&0);

    log::debug!("Read from {:?} ({:#08x}): {:#016x} ({})", register, register as u32, value, value);
    value
  }

  pub fn set(&mut self, register: CSRRegister, value: CSRValue) {
    log::debug!("Write to {:?} ({:#08x}): {:#016x} ({})", register, register as u32, value, value);
    self.registers.insert(register, value);
  } 

  pub fn execute(&mut self, function: CSRFunction, csr: CSRRegister, source: Register, dest: Register, state: &mut RiscvMachineContext) {
    let old = self.get(csr);

    match function {
      CSRFunction::CSRRW => {
        state.registers.set(dest, old);

        if source != Register::Zero {
          let new = state.registers.get(source);
          self.set(csr, new);
        }
      },

      CSRFunction::CSRRWI => {
        state.registers.set(dest, old);

        if source != Register::Zero {
          let new = source.encode() as CSRValue;
          self.set(csr, new);
        }
      },

      CSRFunction::CSRRS => {
        state.registers.set(dest, old);

        let mask = state.registers.get(dest);
        let new = old | mask;

        self.set(csr, new);
      },

      CSRFunction::CSRRSI => {
        state.registers.set(dest, old);

        let mask = dest.encode() as CSRValue;
        let new = old | mask;
        self.set(csr, new);
      },

      CSRFunction::CSRRC => {
        state.registers.set(dest, old);

        let mask = state.registers.get(dest);
        let new = old & !mask;
        self.set(csr, new);
      },

      CSRFunction::CSRRCI => {
        state.registers.set(dest, old);

        let mask = dest.encode() as CSRValue;
        let new = old & !mask;
        self.set(csr, new);
      }
    }
  }
}
