use crate::machine::{RiscvMachine, RiscvMachineError};

mod posix;
pub use posix::Posix;

pub enum SubsystemAction {
    Exit { status_code: u64 },
}

pub trait Subsystem: Default {
    fn system_call(
        &mut self,
        context: &mut RiscvMachine<Self>,
    ) -> Result<Option<SubsystemAction>, RiscvMachineError>;
}
