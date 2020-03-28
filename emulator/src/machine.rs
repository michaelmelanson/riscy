use crate::csr::CSRIndex;
use crate::csr::{CSRRegister, CSR};
use crate::{memory::Memory, subsystem::{SubsystemError, Subsystem, SubsystemAction}};
use riscy_isa::{DecodingStream, Instruction,  Register, Opcode, OpImmFunction, SystemFunction, OpFunction, StoreWidth, LoadWidth, BranchOperation, EnvironmentFunction, OpImm32Function, MiscMemFunction, Op32Function};
use std::{marker::PhantomData, collections::HashMap};

#[derive(Debug)]
pub enum RiscvMachineError {
  UnknownSystemCall(u64),
  Trap
}

impl From<SubsystemError> for RiscvMachineError {
  fn from(error: SubsystemError) -> Self { 
    match error {
      SubsystemError::UnknownSystemCall(syscall) => RiscvMachineError::UnknownSystemCall(syscall)
    }
  }
}

#[derive(Debug)]
pub struct RiscvMachine<S: Subsystem> {
  memory: Memory,
  contexts: HashMap<i32, RiscvMachineContext>,
  current_context: i32,
  halted: bool,
  csr: CSR,
  _phantom: PhantomData<S>
}

impl <S: Subsystem> RiscvMachine<S> {
  pub fn new(memory: Memory, entry: u64) -> Self {
    let mut contexts = HashMap::new();
    contexts.insert(0, RiscvMachineContext {
      pc: entry,
      registers: RiscvRegisters::new(),
      program_break: 0
    });

    Self {
      memory,
      contexts,
      current_context: 0,
      halted: false,
      csr: CSR::new(),
      _phantom: PhantomData::default()
    }
  }

  pub fn subsystem(&self) -> S { S::default() }

  pub fn state(&mut self) -> &RiscvMachineContext {
    self.contexts.get(&self.current_context).expect("Invalid context")
  }

  pub fn state_mut(&mut self) -> &mut RiscvMachineContext {
    self.contexts.get_mut(&self.current_context).expect("Invalid context")
  }

  pub fn halt(&mut self) { self.halted = true; }
  pub fn halted(&self) -> bool { self.halted }

  pub fn step(&mut self) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    let mut stream = DecodingStream::new(&self.memory.physical()[pc as usize..]);
    
    if let Some(instruction) = stream.next() {
      self.execute_instruction(instruction)
    } else {
      self.halt();
      Err(RiscvMachineError::Trap)
    }
  }

  fn execute_instruction(&mut self, instruction: Instruction) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    log::debug!("{:#016x}: Executing {:?}", pc, instruction);

    let mut action = RiscvMachineStepAction::ExecutedInstruction { instruction };
    let mut next_instruction = pc + instruction.width_bytes();

    match instruction {
      Instruction::R { rs2, rs1, rd, opcode } => match opcode {
        Opcode::Op(function) => {
          let registers = &mut self.state_mut().registers;
          let lhs = registers.get(rs1);
          let rhs = registers.get(rs2);

          let result = match function {
            OpFunction::ADD  => lhs.wrapping_add(rhs),
            OpFunction::REMU => lhs.wrapping_rem(rhs),
            OpFunction::SUB  => lhs.wrapping_sub(rhs),
            OpFunction::MUL  => lhs.wrapping_mul(rhs),
            OpFunction::DIV  => if rhs == 0 { -1i64 as u64 } else { (lhs as i64).overflowing_div(rhs as i64).0 as u64 },
            OpFunction::DIVU => if rhs == 0 { -1i64 as u64 } else { lhs.overflowing_div(rhs).0 },
            OpFunction::SLT  => if (lhs as i64) < (rhs as i64) { 1 } else { 0 },
            OpFunction::SLTU => if lhs < rhs { 1 } else { 0 },
            OpFunction::AND  => lhs & rhs,
            OpFunction::OR   => lhs | rhs,
            OpFunction::XOR  => lhs ^ rhs,
            OpFunction::SLL  => lhs.overflowing_shl(rhs as u32).0,
            OpFunction::SRL  => lhs.overflowing_shr(rhs as u32).0,
            OpFunction::SRA  => (lhs as i64).overflowing_shr(rhs as u32).0 as u64,

            _ => unimplemented!("Op function {:?}", function)
          };

          log::debug!("Op: {:#016x} ({}) {:?} {:#016x} ({}) = {:#016x} ({})", lhs, lhs, function, rhs, rhs, result, result);
          registers.set(rd, result as u64);
        },

        Opcode::Op32(function) => {
          let registers = &mut self.state_mut().registers;
          let lhs = registers.get(rs1) as u32;
          let rhs = registers.get(rs2) as u32;

          let result = match function {
            Op32Function::ADDW  => lhs.wrapping_add(rhs),
            Op32Function::SUBW  => lhs.wrapping_sub(rhs),
            Op32Function::SLLW  => lhs.overflowing_shl(rhs as u32).0,
            Op32Function::SRLW  => lhs.overflowing_shr(rhs as u32).0,
            Op32Function::SRAW  => (lhs as i32).overflowing_shr(rhs as u32).0 as u32,

            Op32Function::DIVW  => if rhs == 0 { -1i32 as u32 } else { (lhs as i32).overflowing_div(rhs as i32).0 as u32 },
            Op32Function::DIVUW => if rhs == 0 { -1i32 as u32 } else { lhs.overflowing_div(rhs).0 as u32 }
          };

          log::debug!("OP-32: {:#08x} ({}) {:?} {:#08x} ({}) = {:#08x} ({})", lhs, lhs, function, rhs, rhs, result, result);
          registers.set(rd, result as i32 as u64); // do u32 -> i32 -> u64 to trigger sign extension
        },

        
        _ => unimplemented!("R-type opcode {:?}", opcode)
      },
      
      Instruction::I { imm, rs1, rd, opcode } => match opcode {
        Opcode::JAlr => {
          let offset = imm;
          let base = self.state().registers.get(rs1);
          let link = next_instruction;

          let target = if offset < 0 {
            base.wrapping_sub((-offset) as u64)
          } else {
            base.wrapping_add(offset as u64)
          };

          // clear the low-order bit
          let target = target & !0b1;

          log::trace!("{:#016x}: Jumping to {:#08x} + {} = {:#08x} with link {:#08x}", pc, base, offset, target, link);
          self.state_mut().registers.set(rd, link);
          next_instruction = target;
        },

        Opcode::Load(width) => {
          let offset = imm;
          let base = self.state().registers.get(rs1);

          let address = if offset < 0 {
            base.wrapping_sub(-offset as u64)
          } else {
            base.wrapping_add(offset as u64)
          };     

          let value = match width {
            LoadWidth::DoubleWord => self.load_double_word(address),
            LoadWidth::Word => self.load_word(address),
            LoadWidth::WordUnsigned => self.load_word_unsigned(address),
            LoadWidth::HalfWord => self.load_halfword(address),
            LoadWidth::HalfWordUnsigned => self.load_halfword_unsigned(address),
            LoadWidth::Byte => self.load_byte(address),
            LoadWidth::ByteUnsigned => self.load_byte_unsigned(address),
          };

          self.state_mut().registers.set(rd, value);
        },

        Opcode::MiscMem(function) => match function {
          MiscMemFunction::FENCE => log::warn!("FENCE operation ignored"),
          MiscMemFunction::FENCEI => log::warn!("FENCEI operation ignored"),
        },

        Opcode::OpImm(function) => {
          let lhs = self.state().registers.get(rs1);
          let rhs = imm as u64;
          let shamount = rhs as u32 & 0b111111;

          let value = match function {
            OpImmFunction::ADDI => lhs.wrapping_add(rhs),
            OpImmFunction::SLLI => lhs.overflowing_shl(shamount).0,
            OpImmFunction::SRLI => lhs.overflowing_shr(shamount).0,
            OpImmFunction::SRAI => (lhs as i64).overflowing_shr(shamount).0 as u64,
            OpImmFunction::SLTI => if (lhs as i64) < (rhs as i64) { 1 } else { 0 },
            OpImmFunction::SLTIU => if lhs < rhs { 1 } else { 0 },
            OpImmFunction::ORI => lhs | rhs,
            OpImmFunction::ANDI => lhs & rhs,
            OpImmFunction::XORI => lhs ^ rhs,
          };

          log::debug!("OP-IMM: {:#016x} ({}) {:?} {:#016x} ({}, shamount={}) = {:#016x} ({})", lhs, lhs as i64, function, rhs, rhs as i64, shamount, value, value as i64);
          self.state_mut().registers.set(rd, value);
        },

        Opcode::OpImm32(function) => {
          let lhs = self.state().registers.get(rs1) as u32;
          let rhs = imm as u32;
          let shamount = rhs & 0b11111;

          let value = match function {
            OpImm32Function::ADDIW => lhs.wrapping_add(rhs),
            OpImm32Function::SLLIW => lhs.overflowing_shl(shamount).0,
            OpImm32Function::SRLIW => lhs.overflowing_shr(shamount).0,
            OpImm32Function::SRAIW => (lhs as i32).wrapping_shr(shamount) as u32,
          };

          log::debug!("OP-IMM32: {:#08x} ({}) {:?} {:#08x} ({}, shamount={}) = {:#016x} ({})", lhs, lhs as i64, function, rhs, rhs as i64, shamount, value, value as i64);

          self.state_mut().registers.set(rd, value as i32 as u64);
        },

        Opcode::System(function) => match function {
          SystemFunction::Environment(function) => match function {
            EnvironmentFunction::ECALL => {
              if let Some(subsystem_action) = self.subsystem().system_call(self)? {
                match subsystem_action {
                  SubsystemAction::Exit { status_code } => {
                    action = RiscvMachineStepAction::Exit { status_code };
                  }
                }
              }
            },

            EnvironmentFunction::MRET => {
              let mepc = self.csr.get(CSRRegister::MEPC);
              log::debug!("MRET returning to {:#016x}", mepc);
              self.state_mut().pc = mepc;
            },

            _ => unimplemented!("environment function {:?}", function)
          },

          SystemFunction::CSR(function) => {
            let state = self.contexts.get_mut(&self.current_context).expect("Invalid context");
            self.csr.execute(function, imm as CSRIndex, rs1, rd, state);
          },
        },

        _ => unimplemented!("I-type opcode {:?}", opcode)
      },

      Instruction::S { imm, rs2, rs1, opcode } => match opcode {
        Opcode::Store(width) => {
          let registers = &mut self.state_mut().registers;
          let base_address = registers.get(rs1);
          let effective_address = if imm > 0 {
            base_address.wrapping_add((imm) as u64)
          } else {
            base_address.wrapping_sub((-imm) as u64)
          };

          let value = registers.get(rs2);

          match width {
            StoreWidth::DoubleWord => self.store_double_word(effective_address, value),
            StoreWidth::Word => self.store_word(effective_address, value),
            StoreWidth::HalfWord => self.store_halfword(effective_address, value),
            StoreWidth::Byte => self.store_byte(effective_address, value),
          }
        },

        _ => unimplemented!("S-type opcode {:?}", opcode)
      },

      Instruction::B { imm, rs2, rs1, opcode } => match opcode {
        Opcode::Branch(operation) => {
          let registers = &mut self.state_mut().registers;

          let lhs = registers.get(rs1);
          let rhs = registers.get(rs2);
          let matches = match operation {
            BranchOperation::Equal => lhs == rhs,
            BranchOperation::NotEqual => lhs != rhs,
            BranchOperation::GreaterOrEqual => (lhs as i64) >= (rhs as i64),
            BranchOperation::GreaterOrEqualUnsigned => lhs >= rhs,
            BranchOperation::LessThan => (lhs as i64) < (rhs as i64),
            BranchOperation::LessThanUnsigned => lhs < rhs
          };

          if matches {
            let target = if imm > 0 {
              self.state().pc.wrapping_add(imm as u64)
            } else {
              self.state().pc.wrapping_sub((-imm) as u64)
            };

            log::trace!("{:#016x}: Branching to {:#016x} on condition ({:#016x} {:?} {:#016x}) ", pc, target, lhs, operation, rhs);
            next_instruction = target;
          } else {
            log::trace!("{:#016x}: Branch condition ({:#08x} {:?} {:#08x}) did not match", pc, lhs, operation, rhs);
          }
        },

        _ => unimplemented!("B-type opcode {:?}", opcode)
      },

      Instruction::U { imm, rd, opcode } => match opcode {
        Opcode::AuiPc => {
          let state = self.state_mut();
          let pc = state.pc as u64;
          let offset = imm as u64;
          let value = pc.wrapping_add(offset);
          log::debug!("AUIPC: pc={:#016x}, imm={:#016x}, offset={:#016x}, value={:#016x}", pc, imm, offset, value);
          state.registers.set(rd, value as u64);
        },

        Opcode::Lui => {
          let state = self.state_mut();
          let prior = state.registers.get(rd);
          let value = imm;
          log::debug!("Lui: prior={:#016x}, imm={:#016x}, value={:#016x}", prior, imm, value);
          state.registers.set(rd, value as u64);
        },
        
        _ => unimplemented!("U-type opcode {:?}", opcode)
      },

      Instruction::J { imm, rd, opcode } => match opcode {
        Opcode::JAl => {
          let state = self.state_mut();
          let pc = state.pc;
          let link = next_instruction;
          let offset = imm * 2;

          let target = if offset < 0 {
            pc.wrapping_sub(-offset as u64)
          } else {
            pc.wrapping_add(offset as u64)
          };

          log::debug!("{:#016x}: Jumping to {:#016x} + {} = {:#016x} with link {:#016x}", pc, pc, offset, target, link);
          next_instruction = target;
          state.registers.set(rd, link);
        },

        _ => unimplemented!("J-type opcode {:?}", opcode)

      },
    };

    self.state_mut().pc = next_instruction;

    Ok(action)
  }

  fn store_double_word(&mut self, address: u64, value: u64) {
    log::debug!("{:#016x}: Writing double word {:#016x} ({}) to memory address {:#016x}", self.state().pc, value, value, address);
    self.memory.physical()[address as usize + 7] = (value >> 56) as u8;
    self.memory.physical()[address as usize + 6] = (value >> 48) as u8;
    self.memory.physical()[address as usize + 5] = (value >> 40) as u8;
    self.memory.physical()[address as usize + 4] = (value >> 32) as u8;
    self.memory.physical()[address as usize + 3] = (value >> 24) as u8;
    self.memory.physical()[address as usize + 2] = (value >> 16) as u8;
    self.memory.physical()[address as usize + 1] = (value >> 8) as u8;
    self.memory.physical()[address as usize + 0] = (value >> 0) as u8;
  }

  fn store_word(&mut self, address: u64, value: u64) {
    log::debug!("{:#016x}: Writing word {:#08x} ({}) to memory address {:#016x}", self.state().pc, value, value, address);
    self.memory.physical()[address as usize + 3] = (value >> 24) as u8;
    self.memory.physical()[address as usize + 2] = (value >> 16) as u8;
    self.memory.physical()[address as usize + 1] = (value >> 8) as u8;
    self.memory.physical()[address as usize + 0] = (value >> 0) as u8;
  }

  fn store_halfword(&mut self, address: u64, value: u64) {
    log::debug!("{:#016x}: Writing half-word {:#04x} ({}) to memory address {:#016x}", self.state().pc, value, value, address);
    self.memory.physical()[address as usize + 1] = (value >> 8) as u8;
    self.memory.physical()[address as usize + 0] = (value >> 0) as u8;
  }

  fn store_byte(&mut self, address: u64, value: u64) {
    log::debug!("{:#016x}: Writing byte {:#02x} ({}) to memory address {:#016x}", self.state().pc, value, value, address);
    self.memory.physical()[address as usize + 0] = (value >> 0) as u8;
  }

  fn load_double_word(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 7] as u64) << 56
                   | (self.memory.physical()[address as usize + 6] as u64) << 48
                   | (self.memory.physical()[address as usize + 5] as u64) << 40
                   | (self.memory.physical()[address as usize + 4] as u64) << 32
                   | (self.memory.physical()[address as usize + 3] as u64) << 24
                   | (self.memory.physical()[address as usize + 2] as u64) << 16
                   | (self.memory.physical()[address as usize + 1] as u64) << 8
                   | (self.memory.physical()[address as usize + 0] as u64) << 0;

    log::debug!("{:#016x}: Loaded {:#016x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value
  }

  fn load_word(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 3] as u32) << 24
                   | (self.memory.physical()[address as usize + 2] as u32) << 16
                   | (self.memory.physical()[address as usize + 1] as u32) << 8
                   | (self.memory.physical()[address as usize + 0] as u32) << 0;

    log::debug!("{:#016x}: Loaded word {:#08x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value as i32 as u64
  }

  fn load_word_unsigned(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 3] as u32) << 24
                   | (self.memory.physical()[address as usize + 2] as u32) << 16
                   | (self.memory.physical()[address as usize + 1] as u32) << 8
                   | (self.memory.physical()[address as usize + 0] as u32) << 0;

    log::debug!("{:#016x}: Loaded word {:#08x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value as u64
  }

  fn load_halfword(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 1] as u16) << 8
                   | (self.memory.physical()[address as usize + 0] as u16) << 0;

    log::debug!("{:#016x}: Loaded half-word {:#04x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value as i16 as u64
  }

  fn load_halfword_unsigned(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 1] as u16) << 8
                   | (self.memory.physical()[address as usize + 0] as u16) << 0;

    log::debug!("{:#016x}: Loaded unsigned half-word {:#04x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value as u64
  }

  fn load_byte(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 0] as u8) << 0;

    log::debug!("{:#016x}: Loaded unsigned byte {:#02x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value as i8 as u64
  }

  fn load_byte_unsigned(&mut self, address: u64) -> u64 {
    let value = (self.memory.physical()[address as usize + 0] as u8) << 0;

    log::debug!("{:#016x}: Loaded unsigned byte {:#02x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value as u64
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RiscvMachineContext {
  pub registers: RiscvRegisters,
  pub pc: u64,
  pub program_break: u64
}

pub enum RiscvMachineStepAction {
  ExecutedInstruction { instruction: Instruction },
  Exit { status_code: u64 }
}

pub type RiscvMachineStepResult = Result<RiscvMachineStepAction, RiscvMachineError>;

#[derive(Debug, Clone, PartialEq)]
pub struct RiscvRegisters(HashMap<Register, u64>);

impl RiscvRegisters {
  pub fn new() -> Self {
    let registers = HashMap::new();
    
    Self(registers)
  }

  pub fn get(&self, register: Register) -> u64 {
    if register == Register::Zero { return 0; }
    *self.0.get(&register).unwrap_or(&0)
  }

  pub fn set(&mut self, register: Register, value: u64) {
    if register == Register::Zero { return; }

    self.0.insert(register, value);
  }

}

#[cfg(test)]
mod tests {
  use crate::{subsystem::Selfie, machine::RiscvMachine, memory::Memory};

  fn machine() -> RiscvMachine<Selfie> {
    let memory = Memory::new(0x100000);
    RiscvMachine::<Selfie>::new(memory, 0x10000)
  }

  #[test]
  fn test_store_double_word() {
    let mut machine = machine();
    
    machine.store_double_word(0x1000, 0x1234567890abcdefu64);

    assert_eq!(machine.memory.physical()[0x1000..=0x1007], [0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12]);
  }

  #[test]
  fn test_load_double_word() {
    let mut machine = machine();
    machine.memory.physical()[0x1007] = 0x12;
    machine.memory.physical()[0x1006] = 0x34;
    machine.memory.physical()[0x1005] = 0x56;
    machine.memory.physical()[0x1004] = 0x78;
    machine.memory.physical()[0x1003] = 0x90;
    machine.memory.physical()[0x1002] = 0xab;
    machine.memory.physical()[0x1001] = 0xcd;
    machine.memory.physical()[0x1000] = 0xef;

    let actual = machine.load_double_word(0x1000);

    assert_eq!(actual, 0x1234567890abcdefu64);
  }
}