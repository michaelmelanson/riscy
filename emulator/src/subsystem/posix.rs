use super::{Subsystem, SubsystemAction};
use crate::machine::{RiscvMachine, RiscvMachineError};
use riscy_isa::Register;

struct File {
    fd: u64,
    ref_count: u32,
}

#[derive(Default)]
pub struct Posix;

impl Subsystem for Posix {
    fn system_call(
        &mut self,
        machine: &mut RiscvMachine<Self>,
    ) -> Result<Option<SubsystemAction>, RiscvMachineError> {
        let registers = &machine.state().registers;

        let syscall = registers.get(Register::A7);
        match syscall {
            // close
            57 => {
                let fd = machine.state().registers.get(Register::A0);
                log::info!("close(fd={})", fd);

                machine.state_mut().registers.set(Register::A0, 0);

                Ok(None)
            }

            // write
            64 => {
                let fd = machine.state().registers.get(Register::A0);
                let base = machine.state().registers.get(Register::A1);
                let size = machine.state().registers.get(Register::A2);
                log::debug!("write(fd={}, size={}, base={})", fd, size, base);

                let mut string = String::new();

                for address in base..base + size {
                    let byte = machine.memory.load_byte(address)? as u8;
                    string.push(byte as char);
                }

                log::info!("write({}): {}", fd, string);
                machine.state_mut().registers.set(Register::A0, size);

                Ok(None)
            }

            // fstat
            80 => {
                let fd = machine.state().registers.get(Register::A0);
                let st = machine.state().registers.get(Register::A1);
                log::debug!("fstat({}, {:#016x}) system call", fd, st);

                let file = match fd {
                    0 => unimplemented!("fstat(STDIN)"),
                    1 => File {
                        fd: 1,
                        ref_count: 0,
                    },
                    2 => unimplemented!("fstat(STDERR)"),
                    fd => unimplemented!("fstat({})", fd),
                };

                machine.memory.store_double_word(st + 0, file.fd)?;
                machine.memory.store_word(st + 4, file.ref_count as u64)?;

                machine.state_mut().registers.set(Register::A0, 0);
                Ok(None)
            }

            // exit
            93 => Ok(Some(SubsystemAction::Exit {
                status_code: registers.get(Register::A0),
            })),

            // program break
            214 => {
                let program_break = registers.get(Register::A0);
                let prior_program_break = machine.state().program_break;
                let is_valid = program_break >= prior_program_break
                    && program_break < machine.state().registers.get(Register::StackPointer)
                    && program_break % 4 == 0;

                let state = machine.state_mut();
                if is_valid {
                    log::debug!(
                        "Updating program break from {:#08x} to {:#08x}",
                        prior_program_break,
                        program_break
                    );
                    state.program_break = program_break;
                } else {
                    log::debug!("Returning program break of {:#08x}", prior_program_break);
                    state.registers.set(Register::A0, prior_program_break);
                }

                Ok(None)
            }

            _ => Err(RiscvMachineError::UnknownSystemCall(syscall)),
        }
    }
}
