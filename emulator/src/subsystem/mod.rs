use crate::machine::{RiscvMachine};

mod selfie;
pub use selfie::Selfie;

pub enum SubsystemError {
  UnknownSystemCall(u64)
}

pub enum SubsystemAction {
  Exit { status_code: u64 }
}

pub trait Subsystem: Default {
  fn system_call(&mut self, context: &mut RiscvMachine<Self>) -> Result<Option<SubsystemAction>, SubsystemError>;
}
