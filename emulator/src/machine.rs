use corona_riscv::isa::{DecodingStream, Instruction,  Register, Opcode, OpImmFunction, SystemFunction, OpFunction, StoreWidth, LoadWidth};
use std::collections::HashMap;
use memmap::MmapMut;

#[derive(Debug)]
pub struct RiscvMachine {
  memory: MmapMut,
  contexts: HashMap<i32, RiscvMachineContext>,
  current_context: i32,
  halted: bool
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
      halted: false
    }
  }

  pub fn state(&mut self) -> &mut RiscvMachineContext {
    self.contexts.get_mut(&self.current_context).expect(&format!("Invalid context: {:?}", self.current_context))
  }

  pub fn halt(&mut self) { self.halted = true; }
  pub fn halted(&self) -> bool { self.halted }

  pub fn step(&mut self) -> RiscvMachineStepResult {
    let pc = self.state().pc;
    let mut stream = DecodingStream::new(&self.memory[pc as usize..]);
    
    if let Some(instruction) = stream.next() {
      println!("Instruction at {:#08x}: {:?}", pc, instruction);

      let mut result = RiscvMachineStepResult::ExecutedInstruction(instruction);

      match instruction {
        Instruction::R { rs2, rs1, rd, opcode } => match opcode {
          Opcode::Op(function) => {
            match function {
              OpFunction::ADD => {
                let registers = &mut self.state().registers;
                let src2 = registers.get(rs2) as i64;
                let src1 = registers.get(rs1) as i64;
                let difference = src1 + src2;
                println!("Addition: {} + {} = {}", src1, src2, difference);
                registers.set(rd, difference as u64);
              },

              OpFunction::REMU => {
                let registers = &mut self.state().registers;
                let dividend = registers.get(rs1) as u64;
                let divisor = registers.get(rs2) as u64;
                let remainder = dividend % divisor;
                println!("Unsigned remainder: {} % {} = {}", dividend, divisor, remainder);
                registers.set(rd, remainder);
              },

              OpFunction::SUB => {
                let registers = &mut self.state().registers;
                let src2 = registers.get(rs2) as i64;
                let src1 = registers.get(rs1) as i64;
                let difference = src1 - src2;
                println!("Subtraction: {} - {} = {}", src1, src2, difference);
                registers.set(rd, difference as u64);
              },

              _ => unimplemented!("Op function {:?}", function)
            }
          },

          _ => unimplemented!("R-type opcode {:?}", opcode)
        },
        
        Instruction::I { imm, rs1, rd, opcode } => match opcode {
          Opcode::JAlr => {
            let offset = imm;
            let base = self.state().registers.get(rs1);
            let link = self.state().pc + 4;

            let address = if offset < 0 {
              base.wrapping_sub(-offset as u64)
            } else {
              base.wrapping_add(offset as u64)
            };

            self.state().registers.set(rd, link);
            self.state().pc = address;

            println!("Jumping to {:#08x} + {} = {:#08x} with link {:#08x}", base, offset, address, link);

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

          Opcode::OpImm(function) => {
            match function {
              OpImmFunction::ADD => {
                let lhs = self.state().registers.get(rs1);
                let rhs = imm as u64;
                let value = lhs.wrapping_add(rhs);
                println!("Addition: {:#08x} ({}) + {:#08x} ({}) = {:#08x} ({})", lhs, lhs, rhs, rhs, value, value);
                self.state().registers.set(rd, value);
              },
              
              _ => unimplemented!("OP-Imm function {:?}", function)
            }
          },

          Opcode::System(function) => match function {
            SystemFunction::ECALL => {
              result = RiscvMachineStepResult::SystemCall
            },

            _ => unimplemented!("System function {:?}", function)
          },

          _ => unimplemented!("I-type opcode {:?}", opcode)
        },

        Instruction::S { imm, rs2, rs1, opcode } => match opcode {
          Opcode::Store(width) => {
            let registers = &mut self.state().registers;

            match width {
              StoreWidth::DoubleWord => {
                let effective_address = registers.get(rs1) + imm as u64;
                let value = registers.get(rs2);
                self.store_double_word(effective_address, value);
              },

              _ => unimplemented!("Store width {:?}", width)
            }
          },

          _ => unimplemented!("S-type opcode {:?}", opcode)
        },

        Instruction::U { imm, rd, opcode } => match opcode {
          Opcode::Lui => {
            let value = imm | (self.state().registers.get(rd) & 0xffff) as i32;
            self.state().registers.set(rd, value as u64);
          },
          
          _ => unimplemented!("U-type opcode {:?}", opcode)
        },

        Instruction::J { imm, rd, opcode } => match opcode {
          Opcode::JAl => {
            let state = self.state();
            let pc = state.pc;
            let link = pc + 4;
            let offset = imm * 2;

            state.registers.set(rd, link);

            let target = if offset < 0 {
              pc.wrapping_sub(-offset as u64)
            } else {
              pc.wrapping_add(offset as u64)
            };

            println!("Jumping to {:#08x} + {} = {:#08x} with link {:#08x}", pc, offset, target, link);
            self.state().pc = target;
          },

          _ => unimplemented!("J-type opcode {:?}", opcode)

        },

        _ => unimplemented!("Instruction type {:?}", instruction)
      };

      self.state().pc += instruction.width_bytes();

      result
    } else {
      self.halt();
      RiscvMachineStepResult::Trap
    }
  }

  fn store_double_word(&mut self, address: u64, value: u64) {
    println!("Writing {:#08x} ({}) to memory address {:#08x}", value, value, address);
    self.memory[address as usize + 0] = (value >> 0) as u8;
    self.memory[address as usize + 1] = (value >> 8) as u8;
    self.memory[address as usize + 2] = (value >> 16) as u8;
    self.memory[address as usize + 3] = (value >> 24) as u8;
  }

  fn load_double_word(&mut self, address: u64) -> u64 {
    let value = (self.memory[address as usize + 0] as u64) << 0
                   | (self.memory[address as usize + 1] as u64) << 8
                   | (self.memory[address as usize + 2] as u64) << 16
                   | (self.memory[address as usize + 3] as u64) << 24;

    println!("Loaded {:#08x} ({}) from memory address {:#08x}", value, value, address);

    value
  }
}

#[derive(Debug)]
pub struct RiscvMachineContext {
  pub registers: RiscvRegisters,
  pub pc: u64,
  pub program_break: u64
}

#[derive(Debug)]
pub enum RiscvMachineStepResult {
  ExecutedInstruction(Instruction),
  Trap,
  SystemCall
}

#[derive(Debug)]
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


