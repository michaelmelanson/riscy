
use crate::machine::RiscvRegisters;
use riscv_isa::Register;

#[derive(Debug)]
pub enum SystemCall {
  Break { program_break: u64 },
  Exit { return_code: u64 },

  Unknown(u64)
}

impl SystemCall {
  pub fn from_registers(registers: &RiscvRegisters)-> SystemCall {
    match registers.get(Register::A7) {
      93 => SystemCall::Exit {
        return_code: registers.get(Register::A0) 
      },
      
      214 => SystemCall::Break { 
        program_break: registers.get(Register::A0) 
      },

      x => SystemCall::Unknown(x)
    }
  }
}
