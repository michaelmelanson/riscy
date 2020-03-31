
use crate::machine::{RiscvRegisters};
use riscy_isa::{
  Register,
  CSRFunction
};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum CSRRegister {
  SupervisorAddressTranslationAndProtection,
  MachineStatus,
  MachineISA,
  MachineExceptionDelegation,
  MachineInterruptDelegation,
  MachineInterruptEnable,
  MachineTrapHandlerBaseAddress,
  MachineTrapScratch,
  MachineExceptionProgramCounter,
  PhysMemProtectionConfig0,
  PhysMemProtectionAddr0,
  MachineVendorId,
  MachineArchitectureId,
  MachineImplementationId,
  MachineHartId, 
}

impl CSRRegister {
  pub fn from_u32(value: u32) -> Self {
    match value {
      0x180 => CSRRegister::SupervisorAddressTranslationAndProtection,
      0x300 => CSRRegister::MachineStatus,
      0x301 => CSRRegister::MachineISA,
      0x302 => CSRRegister::MachineExceptionDelegation,
      0x303 => CSRRegister::MachineInterruptDelegation,
      0x304 => CSRRegister::MachineInterruptEnable,
      0x305 => CSRRegister::MachineTrapHandlerBaseAddress,
      0x340 => CSRRegister::MachineTrapScratch,
      0x341 => CSRRegister::MachineExceptionProgramCounter,
      0x3a0 => CSRRegister::PhysMemProtectionConfig0,
      0x3b0 => CSRRegister::PhysMemProtectionAddr0,
      0xf11 => CSRRegister::MachineVendorId,
      0xf12 => CSRRegister::MachineArchitectureId,
      0xf13 => CSRRegister::MachineImplementationId,
      0xf14 => CSRRegister::MachineHartId,

      _ => unimplemented!("CSR {:#08x}", value)
    }
  }

  pub fn as_u32(&self) -> u32 {
    match self {
      CSRRegister::SupervisorAddressTranslationAndProtection => 0x180,
      CSRRegister::MachineStatus => 0x300,
      CSRRegister::MachineISA => 0x301,
      CSRRegister::MachineExceptionDelegation => 0x302,
      CSRRegister::MachineInterruptDelegation => 0x303,
      CSRRegister::MachineInterruptEnable => 0x304,
      CSRRegister::MachineTrapHandlerBaseAddress => 0x305,
      CSRRegister::MachineTrapScratch => 0x340,
      CSRRegister::MachineExceptionProgramCounter => 0x341,
      CSRRegister::PhysMemProtectionConfig0 => 0x3a0,
      CSRRegister::PhysMemProtectionAddr0 => 0x3b0,
      CSRRegister::MachineVendorId => 0xf11,
      CSRRegister::MachineArchitectureId => 0xf12,
      CSRRegister::MachineImplementationId => 0xf13,
      CSRRegister::MachineHartId => 0xf14,
    }
  }
}

type CSRValue = u64;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct CSR {
  registers: HashMap<CSRRegister, CSRValue>
}

impl CSR {
  pub fn get(&self, register: CSRRegister) -> CSRValue {
    let value = *self.registers.get(&register).unwrap_or(&0);

    log::debug!("Read from {:?}: {:#016x} ({})", register, value, value);
    value
  }

  pub fn set(&mut self, register: CSRRegister, value: CSRValue) {
    log::debug!("Write to {:?}: {:#016x} ({})", register, value, value);
    self.registers.insert(register, value);
  }

  pub fn execute(&mut self, function: CSRFunction, csr: CSRRegister, source: Register, dest: Register, registers: &mut RiscvRegisters) {
    
    match function {
      CSRFunction::CSRRW => {
        let old = self.get(csr);
        let new = registers.get(source);
        
        if dest != Register::Zero {
          registers.set(dest, old);
        }
        
        if source != Register::Zero {
          self.set(csr, new);
        }
      },

      CSRFunction::CSRRWI => {
        let old = self.get(csr);
        let new = source.encode() as CSRValue;

        if dest != Register::Zero {
          registers.set(dest, old);
        }

        if source != Register::Zero {
          self.set(csr, new);
        }
      },

      CSRFunction::CSRRS => {
        let old = self.get(csr);
        
        let mask = registers.get(source);
        let new = old | mask;
        registers.set(dest, old);

        if source != Register::Zero {
          self.set(csr, new);
        }
      },

      CSRFunction::CSRRSI => {
        let old = self.get(csr);
        let mask = source.encode() as CSRValue;
        let new = old | mask;

        registers.set(dest, old);

        if mask != 0 {
          self.set(csr, new);
        }
      },

      CSRFunction::CSRRC => {
        let old = self.get(csr);
        let mask = registers.get(source);
        let new = old & !mask;
        
        registers.set(dest, old);
        self.set(csr, new);
      },

      CSRFunction::CSRRCI => {
        let old = self.get(csr);
        let mask = source.encode() as CSRValue;
        let new = old & !mask;
        
        registers.set(dest, old);
        self.set(csr, new);
      }
    }
  }
}
