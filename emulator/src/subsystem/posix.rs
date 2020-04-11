
use super::{SubsystemAction, Subsystem, SubsystemError};
use riscy_isa::Register;
use crate::machine::RiscvMachine;

struct File {
  fd: u64,
  ref_count: u32
}

#[derive(Default)]
pub struct Posix;

impl Subsystem for Posix {
  fn system_call(&mut self, machine: &mut RiscvMachine<Self>) -> Result<Option<SubsystemAction>, SubsystemError> { 
    let registers = &machine.state().registers;
    
    let syscall = registers.get(Register::A7);
    match syscall {
      80 => {
        let fd = machine.state().registers.get(Register::A0);
        let st = machine.state().registers.get(Register::A1);
        log::debug!("fstat({}, {:#016x}) system call", fd, st);

        let file = match fd {
          0 => unimplemented!("fstat(STDIN)"),
          1 => File { fd: 1, ref_count: 0 },
          2 => unimplemented!("fstat(STDERR)"),
          fd => unimplemented!("fstat({})", fd)
        };

        machine.store_double_word(st + 0, file.fd);
        machine.store_word(st + 4, file.ref_count as u64);

        machine.state_mut().registers.set(Register::A0, 0);
        Ok(None)
      },

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