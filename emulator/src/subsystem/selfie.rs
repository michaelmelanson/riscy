
use super::{SubsystemAction, Subsystem, SubsystemError};
use riscy_isa::Register;
use crate::machine::RiscvMachine;

#[derive(Default)]
pub struct Selfie;

impl Subsystem for Selfie {
  fn system_call(&mut self, machine: &mut RiscvMachine<Self>) -> Result<Option<SubsystemAction>, SubsystemError> { 
    let registers = &machine.state().registers;
    
    let syscall = registers.get(Register::A7);
    match syscall {
      93 => Ok(Some(SubsystemAction::Exit {
        status_code: registers.get(Register::A0) 
      })),
      
      214 => {
        let program_break = registers.get(Register::A0);
        let prior_program_break = machine.state().program_break;
        let is_valid = program_break >= prior_program_break
                          && program_break < machine.state().registers.get(Register::StackPointer)
                          && program_break % 4 == 0;

        let state = machine.state_mut();
        if is_valid {
          log::debug!("Updating program break from {:#08x} to {:#08x}", prior_program_break, program_break);
          state.program_break = program_break;
        } else {
          log::debug!("Returning program break of {:#08x}", prior_program_break);
          state.registers.set(Register::A0, prior_program_break);
        }
        
        Ok(None)
      },

      _ => Err(SubsystemError::UnknownSystemCall(syscall))
    }
   }
}