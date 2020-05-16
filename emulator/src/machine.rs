use crate::csr::{CSRRegister, CSR};
use crate::{memory::{MemoryError, Memory}, subsystem::{Subsystem, SubsystemAction}};
use riscy_isa::{DecodingStream, Instruction,  Register, Opcode, OpImmFunction, SystemFunction, OpFunction, StoreWidth, LoadWidth, BranchOperation, EnvironmentFunction, OpImm32Function, MiscMemFunction, Op32Function, AmoFunction, AmoWidth};
use std::{marker::PhantomData, collections::HashMap};

#[derive(Debug)]
pub enum TrapCause {
  MemoryError(MemoryError),
  InvalidInstruction
}

#[derive(Debug)]
pub enum RiscvMachineError {
  UnknownSystemCall(u64),
  Trap(TrapCause)
}

impl From<MemoryError> for RiscvMachineError {
  fn from(error: MemoryError) -> Self { 
    RiscvMachineError::Trap(TrapCause::MemoryError(error))
  }
}

#[derive(Debug)]
pub struct RiscvMachine<S: Subsystem> {
  pub memory: Memory,
  contexts: HashMap<i32, RiscvMachineContext>,
  current_context: i32,
  halted: bool,
  _phantom: PhantomData<S>
}

impl <S: Subsystem> RiscvMachine<S> {
  pub fn new(memory: Memory, entry: u64) -> Self {
    let mut csr = CSR::default();
    csr.set(CSRRegister::MachineISA, 
      2 << (64-2)
    );
    csr.set(CSRRegister::MachineStatus, 
      (2 << 32)
      | (2 << 34)
    );

    let mut contexts = HashMap::new();
    contexts.insert(0, RiscvMachineContext {
      pc: entry,
      registers: RiscvRegisters::new(),
      program_break: 0,
      csr
    });

    Self {
      memory,
      contexts,
      current_context: 0,
      halted: false,
      _phantom: PhantomData::default()
    }
  }

  pub fn subsystem(&self) -> S { S::default() }

  pub fn state(&self) -> &RiscvMachineContext {
    self.contexts.get(&self.current_context).expect("Invalid context")
  }

  pub fn state_mut(&mut self) -> &mut RiscvMachineContext {
    self.contexts.get_mut(&self.current_context).expect("Invalid context")
  }

  pub fn halt(&mut self) { self.halted = true; }
  pub fn halted(&self) -> bool { self.halted }

  pub fn step(&mut self) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    log::debug!("Executing at {:#016x}", pc);
    let mut stream = DecodingStream::new(self.memory.slice(pc)
      .map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?
    );
    
    if let Some(instruction) = stream.next() {
      self.execute_instruction(instruction)
    } else {
      self.halt();
      Err(RiscvMachineError::Trap(TrapCause::InvalidInstruction))
    }
  }

  fn execute_instruction(&mut self, instruction: Instruction) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    log::trace!("{:#016x}: Executing {:?}", pc, instruction);

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
            OpFunction::SUB  => lhs.wrapping_sub(rhs),
            OpFunction::SLT  => if (lhs as i64) < (rhs as i64) { 1 } else { 0 },
            OpFunction::SLTU => if lhs < rhs { 1 } else { 0 },
            OpFunction::AND  => lhs & rhs,
            OpFunction::OR   => lhs | rhs,
            OpFunction::XOR  => lhs ^ rhs,
            OpFunction::SLL  => lhs.overflowing_shl(rhs as u32).0,
            OpFunction::SRL  => lhs.overflowing_shr(rhs as u32).0,
            OpFunction::SRA  => (lhs as i64).overflowing_shr(rhs as u32).0 as u64,

            OpFunction::MUL  => lhs.wrapping_mul(rhs),
            OpFunction::MULH => ((lhs as i64 as i128).overflowing_mul(rhs as i64 as i128).0 >> 64) as u64,
            OpFunction::MULHU => ((lhs as u128).overflowing_mul(rhs as u128).0 >> 64) as u64,
            OpFunction::DIV  => if rhs == 0 { -1i64 as u64 } else { (lhs as i64).overflowing_div(rhs as i64).0 as u64 },
            OpFunction::DIVU => if rhs == 0 { -1i64 as u64 } else { lhs.overflowing_div(rhs).0 },
            OpFunction::REM  => if rhs == 0 { lhs } else { (lhs as i64).overflowing_rem(rhs as i64).0 as u64 },
            OpFunction::REMU => if rhs == 0 { lhs } else { (lhs as u64).overflowing_rem(rhs as u64).0 },

            _ => unimplemented!("Op function {:?}", function)
          };

          log::debug!("{:#016x}: Op computed {:#016x} ({}) = {:#016x} ({}) {:?} {:#016x} ({})", pc, result, result as i64, lhs, lhs as i64, function, rhs, rhs as i64);
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
            Op32Function::DIVUW => if rhs == 0 { -1i32 as u32 } else { lhs.overflowing_div(rhs).0 as u32 },
            Op32Function::MULW  => (lhs as i32).overflowing_mul(rhs as i32).0 as u32,
            Op32Function::REMW  => if rhs == 0 { lhs } else { (lhs as i32).overflowing_rem(rhs as i32).0 as u32 },
            Op32Function::REMUW => if rhs == 0 { lhs } else { lhs.overflowing_rem(rhs).0 },
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

          log::debug!("{:#016x}: JAlr jumping to {:#08x} + {} = {:#08x} with link {:#08x}", pc, base, offset, target, link);
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
            LoadWidth::DoubleWord => self.memory.load_double_word(address),
            LoadWidth::Word => self.memory.load_word(address),
            LoadWidth::WordUnsigned => self.memory.load_word_unsigned(address),
            LoadWidth::HalfWord => self.memory.load_halfword(address),
            LoadWidth::HalfWordUnsigned => self.memory.load_halfword_unsigned(address),
            LoadWidth::Byte => self.memory.load_byte(address),
            LoadWidth::ByteUnsigned => self.memory.load_byte_unsigned(address),
          }.map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?;

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

          log::debug!("{:#016x}: OP-IMM computed {:#016x} ({}) = {:#016x} ({}) {:?} {:#016x} ({}, shamount={})", pc, value, value as i64, lhs, lhs as i64, function, rhs, rhs as i64, shamount);
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

        _ => unimplemented!("I-type opcode {:?}", opcode)
      },

      Instruction::IS { imm, rd, rs1, opcode } => match opcode {
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
              let mepc = self.state().csr.get(CSRRegister::MachineExceptionProgramCounter);
              log::debug!("MRET returning to {:#016x}", mepc);
              self.state_mut().pc = mepc;
            },

            _ => unimplemented!("environment function {:?}", function)
          },

          SystemFunction::CSR(function) => {
            let state = self.contexts.get_mut(&self.current_context).expect("Invalid context");
            state.csr.execute(function, CSRRegister::from_u32(imm as u32), rs1, rd, &mut state.registers);
          },
        },

        _ => unimplemented!("I(S)-type opcode {:?}", opcode)

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
            StoreWidth::DoubleWord => self.memory.store_double_word(effective_address, value),
            StoreWidth::Word => self.memory.store_word(effective_address, value),
            StoreWidth::HalfWord => self.memory.store_halfword(effective_address, value),
            StoreWidth::Byte => self.memory.store_byte(effective_address, value),
          }.map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?
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

            log::debug!("{:#016x}: Branching to {:#016x} on condition ({:#016x} {:?} {:#016x}) ", pc, target, lhs, operation, rhs);
            next_instruction = target;
          } else {
            log::debug!("{:#016x}: Branch condition ({:#08x} {:?} {:#08x}) did not match", pc, lhs, operation, rhs);
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
          log::debug!("{:#016x}: AUIPC computed value {:#016x} = {:#016x} + {:#016x} ({})", pc, value, pc, offset, offset);
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
          let offset = imm;

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

      Instruction::CIW { opcode, imm, rd } => match opcode {
        Opcode::CADDI4SPN => {
          let state = self.state_mut();
          let source = state.registers.get(Register::StackPointer);
          let result = source + imm as u64;

          log::debug!("{:#016x}: C.ADDI4SPN added stack pointer {:#016x} + {} = {:#016x}", pc, source, imm, result);

          state.registers.set(rd, result);
        },

        _ => unimplemented!("CIW-type opcode {:?}", opcode)
      },

      Instruction::CL { opcode, rs1, rd, imm } => match opcode {
        Opcode::CLW | Opcode::CLD => {
          let base = self.state().registers.get(rs1);
          let offset = imm as u64;
          let address = base.wrapping_add(offset);

          let value = match opcode {
            Opcode::CLW => self.memory.load_word(address),
            Opcode::CLD => self.memory.load_double_word(address),
            _ => unimplemented!("opcode {:?} in load", opcode)
          }.map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?;

          log::debug!("{:#016x}: {:?} loaded {:#016x} ({}) from {:#016x} + {:#016x} ({}) = {:#016x}", pc, opcode, value, value as i64, base, offset, offset, address);

          self.state_mut().registers.set(rd, value);
        },

        _ => unimplemented!("CL-type opcode {:?}", opcode)
      },

      Instruction::CS { opcode, rs1, rs2, imm } => match opcode {
        Opcode::CSW | Opcode::CSD => {
          let base = self.state().registers.get(rs1);
          let offset = imm as u64;
          let address = base.wrapping_add(offset);
          let value = self.state().registers.get(rs2);

          log::debug!("{:#016x}: C.SW writing {:#016x} ({}) to {:#016x} + {:#016x} ({}) = {:#016x}", pc, value, value as i64, base, offset, offset, address);

          match opcode {
            Opcode::CSW => self.memory.store_word(address, value),
            Opcode::CSD => self.memory.store_double_word(address, value),
            _ => unimplemented!("CS storage width opcode {:?}", opcode)
          }.map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?;
        },

        _ => unimplemented!("CS-type opcode {:?}", opcode)
      },

      Instruction::CI { opcode, rd, imm } => match opcode {
        Opcode::CADDI16SP => {
          let source = self.state().registers.get(rd);
          let result = source.wrapping_add(imm as u64);

          log::debug!("{:#016x}: C.ADDI16SP added stack pointer {:#016x} + {} = {:#016x}", pc, source, imm, result);
          self.state_mut().registers.set(Register::StackPointer, result);
        },

        Opcode::CADDI => {
          let source = self.state().registers.get(rd);
          let result = source.wrapping_add(imm as u64);

          log::debug!("{:#016x}: C.ADDI added {:#016x} + {} = {:#016x}", pc, source, imm, result);
          self.state_mut().registers.set(rd, result);
        },

        Opcode::CADDIW => {
          let source = self.state().registers.get(rd) as u32;
          let result = if imm < 0 { 
            source.wrapping_sub(-imm as u32)
          } else {
            source.wrapping_add(imm as u32)
          } as i32 as i64 as u64;

          log::debug!("{:#016x}: C.ADDIW added {:#016x} + {} = {:#016x}", pc, source, imm, result);
          self.state_mut().registers.set(rd, result);
        },

        Opcode::CLI => {
          let imm = imm as u64;
          self.state_mut().registers.set(rd, imm);
          log::debug!("{:#016x}: C.LI loaded {:#016x} into {:?}", pc, imm, rd);
        },

        Opcode::CLUI => {
          let imm = (imm as u64) << 12;
          self.state_mut().registers.set(rd, imm);
          log::debug!("{:#016x}: C.LUI loaded {:#016x} into {:?}", pc, imm, rd);
        },

        Opcode::CSLLI => {
          let lhs = self.state().registers.get(rd);
          let shamt = imm as u32;

          let value = lhs.overflowing_shl(shamt).0;

          log::debug!("{:#016x}: C.SLLI computed {:#016x} << {:#016x} = {:#016x}", pc, lhs, shamt, value);
          self.state_mut().registers.set(rd, value);
        },

        Opcode::CLWSP | Opcode::CLDSP => {
          let base = self.state().registers.get(Register::StackPointer);
          let offset = imm as u64;
          let address = base.wrapping_add(offset);

          let value = match opcode {
            Opcode::CLWSP => self.memory.load_word(address),
            Opcode::CLDSP => self.memory.load_double_word(address),
            _ => unimplemented!("CI-type load width {:?}", opcode)
          }.map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?;

          log::debug!("{:#016x}: {:?} loaded {:#016x} from {:#016x} (SP) + {:#016x} ({}) = {:#016x}", pc, opcode, value, base, offset, offset, address);
          self.state_mut().registers.set(rd, value);
        },

        _ => unimplemented!("CI-type opcode {:?}", opcode)
      },

      Instruction::CNOP => {},

      Instruction::CB { opcode, rs1, imm } => match opcode {

        Opcode::CSRLI | Opcode::CSRAI | Opcode::CANDI => {
          let lhs = self.state().registers.get(rs1);
          let rhs = imm;

          let result = match opcode {
            Opcode::CSRLI => lhs.overflowing_shr(rhs as i32 as u32).0 as u64,
            Opcode::CSRAI => (lhs as i64).overflowing_shr(rhs as i32 as u32).0 as u64,
            Opcode::CANDI => lhs & (rhs as i64 as u64),
            _ => unimplemented!("CB-type arithmetic function {:?}", opcode)
          };

          log::debug!("{:#016x}: {:?} Computed {:#016x} {:?} {:#x} ({}) = {:#016x}", pc, opcode, lhs, opcode, rhs, rhs, result);

          self.state_mut().registers.set(rs1, result);
        },

        Opcode::CBEQZ | Opcode::CBNEZ => {
          let value = self.state().registers.get(rs1);

          let matches = match opcode {
            Opcode::CBEQZ => value == 0,
            Opcode::CBNEZ => value != 0,
            _ => unimplemented!("CB-type branch function {:?}", opcode)
          };

          if matches {
            let pc = self.state().pc;
            let offset = imm;
            let target = if offset > 0 {
              pc.wrapping_add(offset as u64)
            } else {
              pc.wrapping_sub((-offset) as u64)
            };
 
            log::debug!("{:#016x}: {:?} condition matches; jumping to {:#016x} + {:#x} ({}) = {:#016x}", pc, opcode, pc, offset, offset, target);
            next_instruction = target;
          } else {
            log::debug!("{:#016x}: {:?} condition does not match (value={:#016x})", pc, opcode, value);

          }
        },

        _ => unimplemented!("CB-type instruction {:?}", opcode)
      },

      Instruction::CA { opcode, rs2, rd } => {
        let lhs = self.state().registers.get(rd);
        let rhs = self.state().registers.get(rs2);

        let result = match opcode {
          Opcode::CAND => lhs & rhs,
          Opcode::COR => lhs | rhs,
          Opcode::CXOR => lhs ^ rhs,
          Opcode::CSUB => lhs.wrapping_sub(rhs),
          Opcode::CADDW => (lhs as u32 as i32).wrapping_add(rhs as u32 as i32) as i64 as u64,
          Opcode::CSUBW => (lhs as u32 as i32).wrapping_sub(rhs as u32 as i32) as i64 as u64,
          _ => unimplemented!("CB-type operator {:?}", opcode)
        };

        log::debug!("{:#016x}: {:?} Computed {:#016x} {:?} {:#x} ({}) = {:#016x}", pc, opcode, lhs, opcode, rhs, rhs, result);

        self.state_mut().registers.set(rd, result);
      },

      Instruction::CJ { opcode, imm } => {
        let state = self.state();
        let pc = state.pc;
        let offset = imm;

        let target = if offset > 0 {
          pc.wrapping_add(offset as u64)
        } else {
          pc.wrapping_sub((-offset) as u64)
        };

        log::debug!("{:#016x}: {:?} jumping to {:#016x} + {:#x} ({}) = {:#016x}", pc, opcode, pc, offset, offset, target);
        next_instruction = target;

        match opcode {
          Opcode::CJ => {},
          _ => unimplemented!("CJ-type behaviour for {:?}", opcode)
        }
      },

      Instruction::CR { opcode, rs1, rs2} => match opcode {
        Opcode::CJR => {
          let address = self.state().registers.get(rs1);
          log::debug!("{:#016x}: C.JR jumping to {:#016x}", pc, address);
          next_instruction = address;
        },

        Opcode::CMV => {
          let value = self.state().registers.get(rs2);

          log::debug!("{:#016x}: C.MV copying {:#016x} from {:?} into {:?}", pc, value, rs2, rs1);
          self.state_mut().registers.set(rs1, value);
        },

        Opcode::CJALR => {
          let address = self.state().registers.get(rs1);
          let link = next_instruction;

          log::debug!("{:#016x}: C.JALR jumping to {:#016x} with link {:#016x}", pc, address, link);
          self.state_mut().registers.set(Register::ReturnAddress, link);
          next_instruction = address;
        },

        Opcode::CADD => {
          let lhs = self.state().registers.get(rs1) as i64;
          let rhs = self.state().registers.get(rs2) as i64;
          let value = lhs.wrapping_add(rhs);

          log::debug!("{:#016x}: C.CADD computed {:#016x} ({}) = {:#016x} ({}) + {:#016x} ({})", pc, value, value, lhs, lhs, rhs, rhs);
          self.state_mut().registers.set(rs1, value as u64)

        },

        _ => unimplemented!("CR-type instruction {:?}", opcode)
      },

      Instruction::CSS { opcode, imm, rs2 } => {
        let base = self.state().registers.get(Register::StackPointer);
        let offset = imm as u64;
        let address = base.wrapping_add(offset);
        let value = self.state().registers.get(rs2);

        log::debug!("{:#016x}: {:?} writing {:#016x} to {:#016x} (SP) + {:#016x} ({}) = {:#016x}", pc, opcode, value, base, offset, offset, address);
        match opcode {
          Opcode::CSWSP => self.memory.store_word(address, value),
          Opcode::CSDSP => self.memory.store_double_word(address, value),
          _ => unimplemented!("CSS-type instruction {:?}", opcode)
        }.map_err(|e| RiscvMachineError::Trap(TrapCause::MemoryError(e)))?;
      },

      Instruction::AR { opcode, aq: _, rl: _, rs1, rs2, rd } => match opcode {
        Opcode::Amo(func, width) => {
          let lhs = self.state().registers.get(rs1);
          let value = match width {
            AmoWidth::DoubleWord => self.memory.load_double_word(lhs)?,
            AmoWidth::Word => self.memory.load_word(lhs)?,
          };

          let rhs = self.state().registers.get(rs2);

          let result = match func {
            AmoFunction::ADD => (value as i64).wrapping_add(rhs as i64) as u64,
            AmoFunction::AND => value & rhs,
            AmoFunction::OR => value | rhs,
            AmoFunction::XOR => value ^ rhs,
            AmoFunction::MAX => (value as i64).max(rhs as i64) as u64,
            AmoFunction::MAXU => value.max(rhs),
            AmoFunction::MIN => (value as i64).min(rhs as i64) as u64,
            AmoFunction::MINU => value.min(rhs),
            AmoFunction::SWAP => rhs,

            _ => unimplemented!("AMO function {:?}", func)
          };

          self.state_mut().registers.set(rd, value);

          match width {
            AmoWidth::DoubleWord => self.memory.store_double_word(lhs, result)?,
            AmoWidth::Word => self.memory.store_word(lhs, result)?,
          }
        },

        _ => unimplemented!("AR-type instruction {:?}", opcode)
      },
    };

    self.state_mut().pc = next_instruction;

    Ok(action)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RiscvMachineContext {
  pub registers: RiscvRegisters,
  pub pc: u64,
  pub program_break: u64,
  pub csr: CSR,
}

pub enum RiscvMachineStepAction {
  ExecutedInstruction { instruction: Instruction },
  Exit { status_code: u64 }
}

pub type RiscvMachineStepResult = Result<RiscvMachineStepAction, RiscvMachineError>;

#[derive(Debug, Clone, PartialEq)]
pub struct RiscvRegisters(pub HashMap<Register, u64>);

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
  use crate::{subsystem::Posix, machine::RiscvMachine, memory::{Region, Memory}};

  fn machine() -> RiscvMachine<Posix> {
    let mut memory = Memory::new();
    memory.add_region(Region::readwrite_memory(0x000000, 0x100000));

    RiscvMachine::<Posix>::new(memory, 0x10000)
  }

  #[test]
  fn test_store_double_word() {
    let mut machine = machine();
    
    machine.memory.store_double_word(0x1000, 0x1234567890abcdefu64).unwrap();

    assert_eq!(machine.memory.slice(0x1000).expect("slice")[0..=0x7], [0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12]);
  }

  #[test]
  fn test_load_double_word() {
    let mut machine = machine();
    machine.memory.store_byte(0x1007, 0x12).expect("load failed");
    machine.memory.store_byte(0x1006, 0x34).expect("load failed");
    machine.memory.store_byte(0x1005, 0x56).expect("load failed");
    machine.memory.store_byte(0x1004, 0x78).expect("load failed");
    machine.memory.store_byte(0x1003, 0x90).expect("load failed");
    machine.memory.store_byte(0x1002, 0xab).expect("load failed");
    machine.memory.store_byte(0x1001, 0xcd).expect("load failed");
    machine.memory.store_byte(0x1000, 0xef).expect("load failed");

    let actual = machine.memory.load_double_word(0x1000).expect("load failed");

    assert_eq!(actual, 0x1234567890abcdefu64);
  }
}