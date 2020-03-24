use crate::csr::CSRIndex;
use crate::csr::{CSRRegister, CSR};
use riscv_isa::{DecodingStream, Instruction,  Register, Opcode, OpImmFunction, SystemFunction, OpFunction, StoreWidth, LoadWidth, BranchOperation, EnvironmentFunction, OpImm32Function, MiscMemFunction};
use std::collections::HashMap;
use memmap::MmapMut;

#[derive(Debug)]
pub struct RiscvMachine {
  memory: MmapMut,
  contexts: HashMap<i32, RiscvMachineContext>,
  current_context: i32,
  halted: bool,
  csr: CSR
}

impl RiscvMachine {
  pub fn new(memory: MmapMut, entry: u64) -> Self {
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
      csr: CSR::new()
    }
  }

  pub fn state(&mut self) -> &mut RiscvMachineContext {
    self.contexts.get_mut(&self.current_context).expect("Invalid context")
  }

  pub fn halt(&mut self) { self.halted = true; }
  pub fn halted(&self) -> bool { self.halted }

  pub fn step(&mut self) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    let mut stream = DecodingStream::new(&self.memory[pc as usize..]);
    
    if let Some(instruction) = stream.next() {
      self.execute_instruction(instruction)
    } else {
      self.halt();
      RiscvMachineStepResult::Trap
    }
  }

  fn execute_instruction(&mut self, instruction: Instruction) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    log::debug!("{:#016x}: Executing {:?}", pc, instruction);

    let mut result = RiscvMachineStepResult::ExecutedInstruction(instruction);
    let mut next_instruction = pc + instruction.width_bytes();

    match instruction {
      Instruction::R { rs2, rs1, rd, opcode } => match opcode {
        Opcode::Op(function) => {
          let registers = &mut self.state().registers;
          let lhs = registers.get(rs1);
          let rhs = registers.get(rs2);

          let result = match function {
            OpFunction::ADD  => lhs.wrapping_add(rhs),
            OpFunction::REMU => lhs.wrapping_rem(rhs),
            OpFunction::SUB  => lhs.wrapping_sub(rhs),
            OpFunction::MUL  => lhs.wrapping_mul(rhs),
            OpFunction::SLT  => if (lhs as i64) < (rhs as i64) { 1 } else { 0 },
            OpFunction::SLTU => if lhs < rhs { 1 } else { 0 },
            OpFunction::AND  => lhs & rhs,
            OpFunction::OR   => lhs | rhs,
            OpFunction::XOR  => lhs ^ rhs,
            OpFunction::SLL  => lhs.overflowing_shl(rhs as u32).0,
            OpFunction::SRL  => lhs.overflowing_shr(rhs as u32).0,

            _ => unimplemented!("Op function {:?}", function)
          };

          log::debug!("Op: {:#016x} ({}) {:?} {:#016x} ({}) = {:#016x} ({})", lhs, lhs, function, rhs, rhs, result, result);
          registers.set(rd, result as u64);
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
          self.state().registers.set(rd, link);
          next_instruction = target;
        },

        Opcode::Load(width) => {
          match width {
            LoadWidth::DoubleWord => {
              let offset = imm;
              let base = self.state().registers.get(rs1);

              let address = if offset < 0 {
                base.wrapping_sub(-offset as u64)
              } else {
                base.wrapping_add(offset as u64)
              };     
              
              let value = self.load_double_word(address);
              self.state().registers.set(rd, value);
            },

            _ => unimplemented!("Load with width {:?}", width)
          }
        },

        Opcode::MiscMem(function) => match function {
          MiscMemFunction::FENCE => log::warn!("FENCE operation ignored"),
        },

        Opcode::OpImm(function) => {
          let lhs = self.state().registers.get(rs1);
          let rhs = imm as u64;
          let shamount = rhs as u32 & 0b111111;

          let value = match function {
            OpImmFunction::ADDI => lhs.wrapping_add(rhs),
            OpImmFunction::SLLI => lhs.overflowing_shl(shamount).0,
            OpImmFunction::SRLI => lhs.overflowing_shr(shamount).0,
            OpImmFunction::ORI => lhs | rhs,
            OpImmFunction::ANDI => lhs & rhs,
            OpImmFunction::XORI => lhs ^ rhs,
            _ => unimplemented!("OP-Imm function {:?}", function)
          };

          log::debug!("OpImm: {:#016x} ({}) {:?} {:#016x} ({}) = {:#016x} ({})", lhs, lhs as i64, function, rhs, rhs as i64, value, value as i64);
          self.state().registers.set(rd, value);
        },

        Opcode::OpImm32(function) => {
          let lhs = self.state().registers.get(rs1) as i32;

          let value = match function {
            OpImm32Function::ADDIW => lhs.wrapping_add(imm as i32),
            OpImm32Function::SLLIW => lhs.overflowing_shl(imm as u32 & 0b11111).0,
            OpImm32Function::SRAIW => lhs.wrapping_shr(imm as u32 & 0b11111),
            OpImm32Function::SRLIW => lhs.overflowing_shr(imm as u32).0,
          };

          self.state().registers.set(rd, value as u64);
        },

        Opcode::System(function) => match function {
          SystemFunction::Environment(function) => match function {
            EnvironmentFunction::ECALL => {
              result = RiscvMachineStepResult::SystemCall;
            },

            EnvironmentFunction::MRET => {
              let mepc = self.csr.get(CSRRegister::MEPC);
              log::debug!("MRET returning to {:#016x}", mepc);
              self.state().pc = mepc;
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
          let registers = &mut self.state().registers;

          match width {
            StoreWidth::DoubleWord => {
              let base_address = registers.get(rs1);
              let effective_address = if imm > 0 {
                base_address.wrapping_add((imm) as u64)
              } else {
                base_address.wrapping_sub((-imm) as u64)
              };
              let value = registers.get(rs2);
              self.store_double_word(effective_address, value);
            },

            _ => unimplemented!("Store width {:?}", width)
          }
        },

        _ => unimplemented!("S-type opcode {:?}", opcode)
      },

      Instruction::B { imm, rs2, rs1, opcode } => match opcode {
        Opcode::Branch(operation) => {
          let registers = &mut self.state().registers;

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
          let pc = self.state().pc as u64;
          let offset = imm as u64;
          let value = pc.wrapping_add(offset);
          log::debug!("AUIPC: pc={:#016x}, imm={:#016x}, offset={:#016x}, value={:#016x}", pc, imm, offset, value);
          self.state().registers.set(rd, value as u64);
        },

        Opcode::Lui => {
          let prior = self.state().registers.get(rd);
          let value = imm;
          log::debug!("Lui: prior={:#016x}, imm={:#016x}, value={:#016x}", prior, imm, value);
          self.state().registers.set(rd, value as u64);
        },
        
        _ => unimplemented!("U-type opcode {:?}", opcode)
      },

      Instruction::J { imm, rd, opcode } => match opcode {
        Opcode::JAl => {
          let state = self.state();
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

    self.state().pc = next_instruction;

    result
  }

  fn store_double_word(&mut self, address: u64, value: u64) {
    log::debug!("{:#016x}: Writing {:#016x} ({}) to memory address {:#016x}", self.state().pc, value, value, address);
    self.memory[address as usize + 7] = (value >> 56) as u8;
    self.memory[address as usize + 6] = (value >> 48) as u8;
    self.memory[address as usize + 5] = (value >> 40) as u8;
    self.memory[address as usize + 4] = (value >> 32) as u8;
    self.memory[address as usize + 3] = (value >> 24) as u8;
    self.memory[address as usize + 2] = (value >> 16) as u8;
    self.memory[address as usize + 1] = (value >> 8) as u8;
    self.memory[address as usize + 0] = (value >> 0) as u8;
  }

  fn load_double_word(&mut self, address: u64) -> u64 {
    let value = (self.memory[address as usize + 7] as u64) << 56
                   | (self.memory[address as usize + 6] as u64) << 48
                   | (self.memory[address as usize + 5] as u64) << 40
                   | (self.memory[address as usize + 4] as u64) << 32
                   | (self.memory[address as usize + 3] as u64) << 24
                   | (self.memory[address as usize + 2] as u64) << 16
                   | (self.memory[address as usize + 1] as u64) << 8
                   | (self.memory[address as usize + 0] as u64) << 0;

    log::debug!("{:#016x}: Loaded {:#016x} ({}) from memory address {:#016x}", self.state().pc, value, value, address);

    value
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RiscvMachineContext {
  pub registers: RiscvRegisters,
  pub pc: u64,
  pub program_break: u64
}

#[derive(Debug)]
pub enum RiscvMachineStepResult {
  ExecutedInstruction(Instruction),
  Trap,
  SystemCall,
}

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
  use crate::machine::RiscvMachine;
  use memmap::MmapMut;
  use riscv_isa::{Opcode, Instruction, Register, OpImmFunction, OpFunction};

  fn machine() -> RiscvMachine {
    let memory = MmapMut::map_anon(0x100000).expect("Could not create memory map");
    RiscvMachine::new(memory, 0x10000)
  }

  #[test]
  fn test_store_double_word() {
    let mut machine = machine();
    
    machine.store_double_word(0x1000, 0x1234567890abcdefu64);

    assert_eq!(machine.memory[0x1000..=0x1007], [0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12]);
  }

  #[test]
  fn test_load_double_word() {
    let mut machine = machine();
    machine.memory[0x1007] = 0x12;
    machine.memory[0x1006] = 0x34;
    machine.memory[0x1005] = 0x56;
    machine.memory[0x1004] = 0x78;
    machine.memory[0x1003] = 0x90;
    machine.memory[0x1002] = 0xab;
    machine.memory[0x1001] = 0xcd;
    machine.memory[0x1000] = 0xef;

    let actual = machine.load_double_word(0x1000);

    assert_eq!(actual, 0x1234567890abcdefu64);
  }

  #[test]
  fn test_li() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::StackPointer, 0x0);
    machine.state().registers.set(Register::ReturnAddress, 0x0);
    machine.state().registers.set(Register::A4, 0xdeadbeefdeadbeef);
    
    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::StackPointer, 0xffffffffffff8000);
      state.registers.set(Register::A4, 0xffffffffffff8000);
      state.pc = 0x10008;
      state
    };

    machine.execute_instruction(Instruction::U { 
      opcode: Opcode::Lui, 
      rd: Register::StackPointer, 
      imm: -32768 
    });

    machine.execute_instruction(Instruction::R { 
      opcode: Opcode::Op(OpFunction::ADD), 
      rd: Register::A4, 
      rs1: Register::ReturnAddress, 
      rs2: Register::StackPointer 
    });

    println!("Expected SP: {:#016x}", expected.registers.get(Register::StackPointer));
    println!("Actual SP:   {:#016x}", machine.state().registers.get(Register::StackPointer));

    println!("Expected A4: {:#016x}", expected.registers.get(Register::A4));
    println!("Actual A4:   {:#016x}", machine.state().registers.get(Register::A4));

    assert_eq!(*machine.state(), expected);

  }

  #[test]
  fn test_add() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 0xffffffff80000000);
    machine.state().registers.set(Register::T1, 0x0000000000007fff);
    machine.state().registers.set(Register::T2, 0xdeadbeefdeadbeef);
    
    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 0xffffffff80007fff);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::ADD),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2,
    });

    println!("Expected T2: {:#016x}", expected.registers.get(Register::T2));
    println!("Actual T2:   {:#016x}", machine.state().registers.get(Register::T2));

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_addi() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 0x100);
    machine.state().registers.set(Register::T1, 0x0);
    
    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T1, 0x110);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::I {
      opcode: Opcode::OpImm(OpImmFunction::ADDI),
      rs1: Register::T0,
      rd: Register::T1,
      imm: 0x10,
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_op_add() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 55);
    machine.state().registers.set(Register::T1, 42);
    machine.state().registers.set(Register::T2, 0xffff);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 97);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::ADD),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_op_add_wrapping() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 0xfffffffffffffffe);
    machine.state().registers.set(Register::T1, 3);
    machine.state().registers.set(Register::T2, 0xbad);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 1);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::ADD),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_op_sub() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 55);
    machine.state().registers.set(Register::T1, 42);
    machine.state().registers.set(Register::T2, 0xffff);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 13);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::SUB),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_op_sub_wrapping() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, std::u64::MIN + 1);
    machine.state().registers.set(Register::T1, -2i32 as u64);
    machine.state().registers.set(Register::T2, 0xffff);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 3);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::SUB),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }


  #[test]
  fn test_op_rem() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 33);
    machine.state().registers.set(Register::T1, 5);
    machine.state().registers.set(Register::T2, 0xffff);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 3);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::REMU),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_op_sltu_false() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 100);
    machine.state().registers.set(Register::T1, 100);
    machine.state().registers.set(Register::T2, 55);
    
    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 0);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::SLTU),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_op_sltu_true() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 99);
    machine.state().registers.set(Register::T1, 100);
    machine.state().registers.set(Register::T2, 55);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T2, 1);
      state.pc = 0x10004;
      state
    };

    machine.execute_instruction(Instruction::R {
      opcode: Opcode::Op(OpFunction::SLTU),
      rs1: Register::T0,
      rs2: Register::T1,
      rd: Register::T2
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_jal() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 0);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T0, 0x10004); 
      state.pc = 0x10020; // adds imm to pc, in steps of two bytes
      state
    };

    machine.execute_instruction(Instruction::J {
      imm: 0x10,
      rd: Register::T0,
      opcode: Opcode::JAl
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_jalr_positive() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 0x10010);
    machine.state().registers.set(Register::T1, 0);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T1, 0x10004); 
      state.pc = 0x10012; // note: low bit is cleared
      state
    };

    machine.execute_instruction(Instruction::I {
      opcode: Opcode::JAlr,
      imm: 3, // note: low-bit is set here
      rs1: Register::T0,
      rd: Register::T1,
    });

    assert_eq!(*machine.state(), expected);
  }

  #[test]
  fn test_jalr_negative() {
    let mut machine = machine();
    machine.state().pc = 0x10000;
    machine.state().registers.set(Register::T0, 0x10010);
    machine.state().registers.set(Register::T1, 0);

    let expected = {
      let mut state = machine.state().clone();
      state.registers.set(Register::T1, 0x10004); 
      state.pc = 0x1000c; // note: low bit is cleared
      state
    };

    machine.execute_instruction(Instruction::I {
      opcode: Opcode::JAlr,
      imm: -3, // note: low-bit is set here
      rs1: Register::T0,
      rd: Register::T1,
    });

    assert_eq!(*machine.state(), expected);
  }

}