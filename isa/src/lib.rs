#![feature(leading_trailing_ones)]

use std::io::Cursor;
use byteorder::{LittleEndian, ReadBytesExt};

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
  Load(LoadWidth),
  LoadFp,
  Custom0,
  MiscMem(MiscMemFunction),
  OpImm(OpImmFunction),
  AuiPc,
  OpImm32(OpImm32Function),

  Store(StoreWidth),
  StoreFp,
  Custom1,
  Amo,
  Op(OpFunction),
  Lui,
  Op32(Op32Function),

  MAdd,
  MSub,
  NMSub,
  NMAdd,
  OpFp,
  Reserved0,
  Custom2,

  Branch(BranchOperation),
  JAlr,
  Reserved1,
  JAl,
  System(SystemFunction),
  Reserved2,
  Custom3,
}

impl Opcode {

  fn new(base: u16, func3: u8, imm11_0: u16, func7: u8) -> Opcode {
    let opcode = base & 0b1111111;
    match opcode {
      0b0000011 => Opcode::Load(LoadWidth::from_func3(func3)),
      0b0000111 => Opcode::LoadFp,
      0b0001011 => Opcode::Custom0,
      0b0001111 => Opcode::MiscMem(MiscMemFunction::from_func3(func3)),
      0b0010011 => Opcode::OpImm(OpImmFunction::from_imm11_0_func3(imm11_0, func3)),
      0b0010111 => Opcode::AuiPc,
      0b0011011 => Opcode::OpImm32(OpImm32Function::from_func3_imm(func3, imm11_0)),

      0b0100011 => Opcode::Store(StoreWidth::from_func3(func3)),
      0b0100111 => Opcode::StoreFp,
      0b0101011 => Opcode::Custom1,
      0b0101111 => Opcode::Amo,
      0b0110011 => Opcode::Op(OpFunction::from_func3_func7(func3, func7)),
      0b0110111 => Opcode::Lui,
      0b0111011 => Opcode::Op32(Op32Function::from_func3_func7(func3, func7)),

      0b1000011 => Opcode::MAdd,
      0b1000111 => Opcode::MSub,
      0b1001011 => Opcode::NMSub,
      0b1001111 => Opcode::NMAdd,
      0b1010011 => Opcode::OpFp,
      0b1010111 => Opcode::Reserved0,
      0b1011011 => Opcode::Custom2,

      0b1100011 => Opcode::Branch(BranchOperation::from_func3(func3)),
      0b1100111 => Opcode::JAlr,
      0b1101011 => Opcode::Reserved1,
      0b1101111 => Opcode::JAl,
      0b1110011 => Opcode::System(SystemFunction::from_func3_imm11_0(func3, imm11_0)),
      0b1110111 => Opcode::Reserved2,
      0b1111011 => Opcode::Custom3,

      _ => panic!("Unknown opcode")
    }
  }

  fn opcode_field(&self) -> i32 {
    match self {
      Opcode::Load(_) => 0b0000011,
      Opcode::LoadFp => 0b0000111, 
      Opcode::Custom0 => 0b0001011, 
      Opcode::MiscMem(_) => 0b0001111, 
      Opcode::OpImm(_) => 0b0010011, 
      Opcode::AuiPc => 0b0010111, 
      Opcode::OpImm32(_) => 0b0011011, 

      Opcode::Store(_) => 0b0100011, 
      Opcode::StoreFp => 0b0100111, 
      Opcode::Custom1 => 0b0101011, 
      Opcode::Amo => 0b0101111, 
      Opcode::Op(_) => 0b0110011, 
      Opcode::Lui => 0b0110111, 
      Opcode::Op32(_) => 0b0111011, 

      Opcode::MAdd => 0b1000011, 
      Opcode::MSub => 0b1000111, 
      Opcode::NMSub => 0b1001011, 
      Opcode::NMAdd => 0b1001111, 
      Opcode::OpFp => 0b1010011, 
      Opcode::Reserved0 => 0b1010111, 
      Opcode::Custom2 => 0b1011011, 

      Opcode::Branch(_) => 0b1100011, 
      Opcode::JAlr => 0b1100111, 
      Opcode::Reserved1 => 0b1101011, 
      Opcode::JAl => 0b1101111,
      Opcode::System(_) => 0b1110011, 
      Opcode::Reserved2 => 0b1110111, 
      Opcode::Custom3 => 0b1111011
    }
  }

  pub fn funct3_field(&self) -> i32 {
    match self {
      Opcode::Load(function) => function.to_func3(),
      Opcode::OpImm(function) => function.to_func3(),
      Opcode::Store(function) => function.to_func3(),
      Opcode::Op(function) => function.to_func3(),
      Opcode::Branch(function) => function.to_func3(),
      Opcode::System(_) => 0,

      _ => unimplemented!("opcode {:?} does not have funct3 field", self)
    }
  }

  pub fn funct7_field(&self) -> i32 {
    match self {
      Opcode::Op(function) => function.to_func7(),

      _ => unimplemented!("opcode {:?} does not have funct7 field", self)
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Register {
  Zero,  
  ReturnAddress,                  
  StackPointer,                             
  GlobalPointer, 
  ThreadPointer, 
  T0, T1, T2, 
  S0, S1, 
  A0, A1, A2, A3, A4, A5, A6, A7,     
  S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
  T3, T4, T5, T6,           
}

impl Register {
  pub fn from_u8(value: u8) -> Register {
    match value {
      0 => Register::Zero,  

      1 => Register::ReturnAddress,                  
      2 => Register::StackPointer,                             
      3 => Register::GlobalPointer, 
      4 => Register::ThreadPointer, 

      5 => Register::T0, 
      6 => Register::T1, 
      7 => Register::T2, 

      8 => Register::S0, 
      9 => Register::S1, 

      10 => Register::A0, 
      11 => Register::A1,
      12 => Register::A2, 
      13 => Register::A3,       
      14 => Register::A4,                   
      15 => Register::A5,                 
      16 => Register::A6, 
      17 => Register::A7,     

      18 => Register::S2,
      19 => Register::S3,
      20 => Register::S4,
      21 => Register::S5,
      22 => Register::S6,
      23 => Register::S7,
      24 => Register::S8,
      25 => Register::S9, 
      26 => Register::S10,
      27 => Register::S11,

      28 => Register::T3, 
      29 => Register::T4, 
      30 => Register::T5,
      31 => Register::T6,                  

      _ => unimplemented!("register {}", value)
    }
  }
  pub fn encode(&self) -> i32 {
    match self {
      Register::Zero => 0,  

      Register::ReturnAddress => 1,                  
      Register::StackPointer => 2,                             
      Register::GlobalPointer => 3, 
      Register::ThreadPointer => 4, 

      Register::T0 => 5, 
      Register::T1 => 6, 
      Register::T2 => 7, 

      Register::S0 => 8, 
      Register::S1 => 9, 

      Register::A0 => 10, 
      Register::A1 => 11,
      Register::A2 => 12, 
      Register::A3 => 13,       
      Register::A4 => 14,                   
      Register::A5 => 15,                 
      Register::A6 => 16, 
      Register::A7 => 17,     

      Register::S2 => 18,
      Register::S3 => 19,
      Register::S4 => 20,
      Register::S5 => 21,
      Register::S6 => 22,
      Register::S7 => 23,
      Register::S8 => 24,
      Register::S9 => 25, 
      Register::S10 => 26,
      Register::S11 => 27,

      Register::T3 => 28, 
      Register::T4 => 29, 
      Register::T5 => 30,
      Register::T6 => 32,                  
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FPRegister {
  FS0, FS1, FS2, FS3, FS4, FS5,   // f0-15 (FP saved registers)
  FS6, FS7, FS8, FS9, FS10, FS11,
  FS12, FS13, FS14, FS15,        
  FV0, FV1,                       // f16-17 (FP return values)
  FA0, FA1, FA2, FA3, FA4, FA5,   // f18-25 (FP arguments)
  FA6, FA7,
  FT0, FT1, FT2, FT3, FT4, FT5    // ft0-5 (FP temporaries)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MiscMemFunction {
  FENCE,
}

impl MiscMemFunction {
  pub fn from_func3(func3: u8) -> Self {
    match func3 {
      0b000 => MiscMemFunction::FENCE,

      _ => unimplemented!("MISC-MEM for func3={:#03b}", func3)
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      MiscMemFunction::FENCE  => 0b000,
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpImmFunction {
  ADDI,
  SLTI,
  SLTIU,

  ANDI,
  ORI,
  XORI,

  SLLI,
  SRLI,
  SRAI,
}

impl OpImmFunction {
  pub fn from_imm11_0_func3(imm11_0: u16, func3: u8) -> Self {
    match func3 {
      0b000 => OpImmFunction::ADDI,
      0b010 => OpImmFunction::SLTI,
      0b011 => OpImmFunction::SLTIU,
      0b100 => OpImmFunction::XORI,
      0b110 => OpImmFunction::ORI,
      0b111 => OpImmFunction::ANDI,

      0b001 => OpImmFunction::SLLI,
      0b101 if imm11_0 & 0b111111000000 == 0 => OpImmFunction::SRLI,
      0b101 => OpImmFunction::SRAI,

      _ => unimplemented!("OP-IMM for func3={:#03b}", func3)
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      OpImmFunction::ADDI  => 0b000,
      OpImmFunction::SLTI  => 0b010,
      OpImmFunction::SLTIU => 0b011,
      OpImmFunction::XORI  => 0b100,
      OpImmFunction::ORI   => 0b110,
      OpImmFunction::ANDI  => 0b111,

      OpImmFunction::SLLI => 0b001,
      OpImmFunction::SRLI => 0b101,
      OpImmFunction::SRAI => 0b101,
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpImm32Function {
  ADDIW,
  SLLIW,
  SRLIW,
  SRAIW,
}

impl OpImm32Function {
  pub fn from_func3_imm(func3: u8, imm11_0: u16) -> Self {
    match (func3, imm11_0 >> 5) {
      (0b000, _) => OpImm32Function::ADDIW,
      (0b001, high) if high == 0b0000000 => OpImm32Function::SLLIW,
      (0b101, high) if high == 0b0000000 => OpImm32Function::SRLIW,
      (0b101, high) if high == 0b0100000 => OpImm32Function::SRAIW,

      _ => unimplemented!("OP-IMM with func3={:#03b}, imm11_0={:#08x}", func3, imm11_0)
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      OpImm32Function::ADDIW => 0b000,
      OpImm32Function::SLLIW => 0b001,
      OpImm32Function::SRLIW => 0b101,
      OpImm32Function::SRAIW => 0b101,
    }
  }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpFunction {
  ADD,
  SUB,
  SLL,
  SLT,
  SLTU,
  XOR,
  SRL,
  SRA,
  OR,
  AND,

  MUL,
  MULH,
  MULHSU,
  MULHU,
  DIV,
  DIVU,
  REM,
  REMU
}

impl OpFunction {
  pub fn from_func3_func7(func3: u8, func7: u8) -> Self {
    match (func7, func3) {
      (0b0000000, 0b000) => Self::ADD,
      (0b0100000, 0b000) => Self::SUB,
      (0b0000000, 0b001) => Self::SLL,
      (0b0000000, 0b010) => Self::SLT,
      (0b0000000, 0b011) => Self::SLTU,
      (0b0000000, 0b100) => Self::XOR,
      (0b0000000, 0b101) => Self::SRL,
      (0b0100000, 0b101) => Self::SRA,
      (0b0000000, 0b110) => Self::OR,
      (0b0000000, 0b111) => Self::AND,
      (0b0000001, 0b000) => Self::MUL,
      (0b0000001, 0b001) => Self::MULH,
      (0b0000001, 0b010) => Self::MULHSU,
      (0b0000001, 0b011) => Self::MULHU,
      (0b0000001, 0b100) => Self::DIV,
      (0b0000001, 0b101) => Self::DIVU,
      (0b0000001, 0b110) => Self::REM,
      (0b0000001, 0b111) => Self::REMU,

      _ => unimplemented!("Op func7={:07b} func3={:03b}", func7, func3)
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      Self::ADD => 0b000,
      Self::SUB => 0b000,
      Self::SLL => 0b001,
      Self::SLT => 0b010,
      Self::SLTU => 0b011,
      Self::XOR => 0b100,
      Self::SRL => 0b101,
      Self::SRA => 0b101,
      Self::OR => 0b110,
      Self::AND => 0b111,
      Self::MUL => 0b000,
      Self::MULH => 0b001,
      Self::MULHSU => 0b010,
      Self::MULHU => 0b011,
      Self::DIV => 0b100,
      Self::DIVU => 0b101,
      Self::REM => 0b110,
      Self::REMU => 0b111
    }
  }

  pub fn to_func7(&self) -> i32 {
    match self {
      Self::ADD => 0b0000000,
      Self::SUB => 0b0100000,
      Self::SLL => 0b0000000,
      Self::SLT => 0b0000000,
      Self::SLTU => 0b0000000,
      Self::XOR => 0b0000000,
      Self::SRL => 0b0000000,
      Self::SRA => 0b0100000,
      Self::OR => 0b0000000,
      Self::AND => 0b0000000,
      Self::MUL => 0b0000001,
      Self::MULH => 0b0000001,
      Self::MULHSU => 0b0000001,
      Self::MULHU => 0b0000001,
      Self::DIV => 0b0000001,
      Self::DIVU => 0b0000001,
      Self::REM => 0b0000001,
      Self::REMU => 0b0000001
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Op32Function {
  ADDW,
  SUBW,
  SLLW,
  SRLW,
  SRAW,
}

impl Op32Function {
  pub fn from_func3_func7(func3: u8, func7: u8) -> Self {
    match (func7, func3) {
      (0b0000000, 0b000) => Op32Function::ADDW,
      (0b0100000, 0b000) => Op32Function::SUBW,
      (0b0000000, 0b001) => Op32Function::SLLW,
      (0b0000000, 0b101) => Op32Function::SRLW,
      (0b0100000, 0b101) => Op32Function::SRAW,

      _ => unimplemented!("OP-32 with func7={:#07b}, func3={:#03b}", func7, func3)
    }
  }
  
  pub fn to_func3(&self) -> i32 {
    match self {
      Op32Function::ADDW => 0b000,
      Op32Function::SUBW => 0b000,
      Op32Function::SLLW => 0b001,
      Op32Function::SRLW => 0b101,
      Op32Function::SRAW => 0b101,
    }
  }

  pub fn to_func7(&self) -> i32 {
    match self {
      Op32Function::ADDW => 0b0000000,
      Op32Function::SUBW => 0b0100000,
      Op32Function::SLLW => 0b0000000,
      Op32Function::SRLW => 0b0000000,
      Op32Function::SRAW => 0b0100000,
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SystemFunction {
  // base
  Environment(EnvironmentFunction),

  //Zicsr
  CSR(CSRFunction),
}

impl SystemFunction {
  pub fn from_func3_imm11_0(func3: u8, imm11_0: u16) -> Self {
    match func3 {
      0 => SystemFunction::Environment(EnvironmentFunction::from_imm11_0(imm11_0)),
      0b001 | 0b010 | 0b011 | 0b101 | 0b110 | 0b111 => SystemFunction::CSR(CSRFunction::from_func3(func3)),

      _ => unimplemented!("system function {:#04x}", func3)
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum EnvironmentFunction {
  ECALL,
  EBREAK,

  URET,
  SRET,
  MRET,
}

impl EnvironmentFunction {
  pub fn from_imm11_0(imm11_0: u16) -> Self {
    match imm11_0 {
      0b000000000000 => EnvironmentFunction::ECALL,
      0b000000000001 => EnvironmentFunction::EBREAK,

      0b000000000010 => EnvironmentFunction::URET,
      0b000100000010 => EnvironmentFunction::SRET,
      0b001100000010 => EnvironmentFunction::MRET,

      _ => unimplemented!("environment function {:#012b}", imm11_0)
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CSRFunction {
  CSRRW,
  CSRRS,
  CSRRC,
  CSRRWI,
  CSRRSI,
  CSRRCI
}

impl CSRFunction {
  pub fn from_func3(func3: u8) -> Self {
    match func3 {
      0b001 => CSRFunction::CSRRW,
      0b010 => CSRFunction::CSRRS,
      0b011 => CSRFunction::CSRRC,
      0b101 => CSRFunction::CSRRWI,
      0b110 => CSRFunction::CSRRSI,
      0b111 => CSRFunction::CSRRCI,

      _ => unimplemented!("csr function {:#03b}", func3)
    }
  }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BranchOperation {
  Equal,
  NotEqual,
  LessThan,
  GreaterOrEqual,
  LessThanUnsigned,
  GreaterOrEqualUnsigned
}

impl BranchOperation {
  pub fn from_func3(func3: u8) -> Self {
    match func3 {
      0b000 => BranchOperation::Equal,
      0b001 => BranchOperation::NotEqual,
      0b100 => BranchOperation::LessThan,
      0b101 => BranchOperation::GreaterOrEqual,
      0b110 => BranchOperation::LessThanUnsigned,
      0b111 => BranchOperation::GreaterOrEqualUnsigned,

      _ => unimplemented!()
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      BranchOperation::Equal => 0b000,
      BranchOperation::NotEqual => 0b001,
      BranchOperation::LessThan => 0b100,
      BranchOperation::GreaterOrEqual => 0b101,
      BranchOperation::LessThanUnsigned => 0b110,
      BranchOperation::GreaterOrEqualUnsigned => 0b111
    }
  }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadWidth {
  Byte,
  HalfWord,
  Word,
  DoubleWord,
  ByteUnsigned,
  HalfWordUnsigned,
  WordUnsigned,
}

impl LoadWidth {
  pub fn from_func3(func3: u8) -> Self {
    match func3 {
      0b000 => LoadWidth::Byte,
      0b001 => LoadWidth::HalfWord,
      0b010 => LoadWidth::Word,
      0b011 => LoadWidth::DoubleWord,
      0b100 => LoadWidth::ByteUnsigned,
      0b101 => LoadWidth::HalfWordUnsigned,
      0b110 => LoadWidth::WordUnsigned,

      _ => unimplemented!("Load width {:03b}", func3)
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      LoadWidth::Byte => 0b000,
      LoadWidth::HalfWord => 0b001,
      LoadWidth::Word => 0b010,
      LoadWidth::DoubleWord => 0b011,
      LoadWidth::ByteUnsigned => 0b100,
      LoadWidth::HalfWordUnsigned => 0b101,
      LoadWidth::WordUnsigned => 0b110
    }
  }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StoreWidth {
  Byte,
  HalfWord,
  Word,
  DoubleWord,
}

impl StoreWidth {
  pub fn from_func3(func3: u8) -> Self {
    match func3 {
      0b000 => Self::Byte,
      0b001 => Self::HalfWord,
      0b010 => Self::Word,
      0b011 => Self::DoubleWord,

      _ => unimplemented!("Store width {:03b}", func3)
    }
  }

  pub fn to_func3(&self) -> i32 {
    match self {
      Self::Byte => 0b000,
      Self::HalfWord => 0b001,
      Self::Word => 0b010,
      Self::DoubleWord => 0b011
    }
  }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
  R { opcode: Opcode, rd: Register, /*func3: u8,*/ rs1: Register, rs2: Register /*, funct7: u8 */ },
  I { opcode: Opcode, rd: Register, /*func3: u8,*/ rs1: Register, imm: i32 },
  S { opcode: Opcode,               /*func3: u8,*/ rs1: Register, rs2: Register, imm: i32 },
  B { opcode: Opcode,               /*func3: u8,*/ rs1: Register, rs2: Register, imm: i32 },
  U { opcode: Opcode, rd: Register,                               imm: i32 },
  J { opcode: Opcode, rd: Register,                               imm: i32 }
}

impl Instruction {
  pub fn from_32bits(encoded: u32) -> Instruction {
    let funct3   = (encoded >> 12) & 0b111;
    let funct7   = (encoded >> 25) & 0b1111111;
    let rd       = (encoded >> 7) & 0b11111;
    let rs1      = (encoded >> 15) & 0b11111;
    let rs2      = (encoded >> 20) & 0b11111;
    
    let imm31_12  = encoded >> 12;
    let imm19_12  = (encoded >> 12) & 0b11111111;
    let imm12     = (encoded >> 31) & 0b1;
    let imm11_5   = (encoded >> 25) & 0b1111111;
    let imm10_5   = (encoded >> 25) & 0b111111;
    let imm10_1   = (encoded >> 22) & 0b1111111111;
    let imm4_1    = (encoded >> 8) & 0b1111;
    let imm11_0   = (encoded >> 20) & 0b111111111111;
    let imm4_0    = (encoded >> 7) & 0b11111;

    let sign_bit = (encoded >> 31) & 0b1;
    let opcode = Opcode::new(
      encoded as u16, 
      funct3 as u8,
      imm11_0 as u16,
      funct7 as u8
    );

    let from_r_type = |opcode| {
      Instruction::R { 
        opcode,
        rs2: Register::from_u8(rs2 as u8),
        rs1: Register::from_u8(rs1 as u8),
        rd: Register::from_u8(rd as u8)
      }
    };

    let from_i_type = |opcode| {
      let imm: u32 = (if sign_bit > 0 { 0b11111111111111111111 << 12 } else { 0 })
                   | imm11_0;

      Instruction::I { 
        opcode, 
        rd: Register::from_u8(rd as u8), 
        imm: imm as i32, 
        rs1: Register::from_u8(rs1 as u8) 
      }
    };

    let from_s_type = |opcode| {
      let imm = (if sign_bit > 0 { 0b11111111111111111111 << 12 } else { 0 }) 
                   | (imm11_5 << 5) 
                   | imm4_0;

      Instruction::S { 
        opcode, 
        rs2: Register::from_u8(rs2 as u8),
        rs1: Register::from_u8(rs1 as u8), 
        imm: imm as i32
      }
    };

    let from_b_type = |opcode| {
      let imm11 = (encoded >> 7) & 0b1;
      let imm = (if sign_bit > 0 { 0b11111111111111111111 << 12 } else { 0 })
                   | (imm12 << 12)
                   | (imm11 << 11)
                   | (imm10_5 << 5)
                   | (imm4_1 << 1);

      Instruction::B {
        opcode, 
        rs1: Register::from_u8(rs1 as u8),
        rs2: Register::from_u8(rs2 as u8), 
        imm: imm as i32
      }
    };

    let from_u_type = |opcode| {
      let imm = imm31_12 << 12;

      Instruction::U {
        opcode, 
        rd: Register::from_u8(rd as u8), 
        imm: imm as i32 
      }
    };

    let from_j_type = |opcode| {
      let imm11 = (encoded >> 20) & 0b1;

      let imm = (if sign_bit > 0 { 0b111111111111 << 20 } else { 0 })
                   | (imm19_12 << 12)
                   | (imm11 << 11)
                   | (imm10_1 << 1);

      Instruction::J {
        opcode,
        rd: Register::from_u8(rd as u8),
        imm: imm as i32
      }
    };

    let instruction = match opcode {
      Opcode::Lui       => from_u_type(opcode),
      Opcode::AuiPc     => from_u_type(opcode),
      Opcode::JAl       => from_j_type(opcode),
      Opcode::JAlr      => from_i_type(opcode),
      Opcode::Branch(_) => from_b_type(opcode),
      Opcode::Load(_)   => from_i_type(opcode),
      Opcode::Store(_)  => from_s_type(opcode),
      Opcode::OpImm(_)  => from_i_type(opcode),
      Opcode::OpImm32(_) => from_i_type(opcode),
      Opcode::Op(_)     => from_r_type(opcode),
      Opcode::Op32(_)   => from_r_type(opcode),
      Opcode::System(_) => from_i_type(opcode),
      Opcode::MiscMem(_) => from_i_type(opcode),

      _ => unimplemented!("opcode {:?}", opcode)
    };

    instruction
  }

  pub fn bytes(&self) -> Vec<u8> {
    let encoded = match self {

      Instruction::R { rs2, rs1, rd, opcode } => (
        (opcode.funct7_field() << 25) |
        (rs2.encode() << 20) |
        (rs1.encode() << 15) |
        (opcode.funct3_field() << 12) |
        (rd.encode() << 7) |
        opcode.opcode_field()
      ),

      Instruction::I { imm, rs1, rd, opcode } => {
        let funct3 = opcode.funct3_field();

        (imm << 20) | 
        (rs1.encode() << 15) | 
        (funct3 << 12) | 
        (rd.encode() << 7) | 
        (opcode.opcode_field())
      },

      Instruction::S { imm, rs2, rs1, opcode } => (
        (((imm >> 5) & 0b1111111) << 25) |
        (rs2.encode() << 20) |
        (rs1.encode() << 15) |
        (opcode.funct3_field() << 12) |
        ((imm & 0b11111) << 7) |
        opcode.opcode_field()
      ),

      Instruction::B { imm, rs2, rs1, opcode } => (
        (((imm >> 20) & 0b1) << 31) |
        (((imm >> 5) & 0b111111) << 25) |
        (rs2.encode() << 20) |
        (rs1.encode() << 15) |
        (opcode.funct3_field() << 12) |
        (((imm >> 1) & 0b1111) << 8) |
        (((imm >> 11) & 0b1) << 7) |
        opcode.opcode_field()
      ),

      Instruction::U { imm, rd, opcode } => 
        rd.encode() << 7
        | opcode.opcode_field()
        | (imm & 0b11111111111111111111),


      Instruction::J { imm, rd, opcode } => (
        (((imm >> 20) & 0b1) << 31) |
        (((imm >> 1) & 0b1111111111) << 22) |
        (((imm >> 11) & 0b1) << 20) |
        (((imm >> 12) & 0b11111111) << 12) |
        (rd.encode() << 7) |
        opcode.opcode_field()
      )
    };

    encoded.to_le_bytes().to_vec()
  }

  pub fn width_bytes(&self) -> u64 {
    4
  }
}

#[derive(Debug)]
pub enum ReadError {
  InvalidInstruction
}

pub struct DecodingStream<'a> {
  cursor: Cursor<&'a [u8]>
}

impl <'a> DecodingStream<'a> {
  pub fn new(bytes: &'a [u8]) -> DecodingStream {
    DecodingStream {
      cursor: Cursor::new(bytes)
    }
  }
}

impl<'a> Iterator for DecodingStream<'a> {
  type Item = Instruction;

  fn next(&mut self) -> Option<<Self as Iterator>::Item> { 
    let base: u16 = match self.cursor.read_u16::<LittleEndian>() {
      Ok(base) => base,
      Err(_) => { return None; }
    };

    if base == 0 { return None; }

    let ones = base.trailing_ones();
    
    match ones {
      1 => unimplemented!("16-bit instruction: {:?}", base),

      2..=4 => {
        let second: u32 = self.cursor.read_u16::<LittleEndian>().unwrap() as u32;
        let encoded: u32 = (second << 16) | (base as u32);

        Some(Instruction::from_32bits(encoded))
      },

      5 => { 
        let parcels = [
          base,
          self.cursor.read_u16::<LittleEndian>().unwrap(),
          self.cursor.read_u16::<LittleEndian>().unwrap()
        ];

        unimplemented!("48-bit instruction: {:?}", parcels);
      },

      6 => {
        let parcels = [
          base,
          self.cursor.read_u16::<LittleEndian>().unwrap(),
          self.cursor.read_u16::<LittleEndian>().unwrap(),
          self.cursor.read_u16::<LittleEndian>().unwrap()
        ];

        unimplemented!("64-bit instruction: {:?}", parcels);
      },

      7..=11 => unimplemented!("Large instruction ({} trailing ones): {:#018b}", ones, base),
      _ => unimplemented!("Invalid instruction ({} trailing ones): {:#018b}", ones, base),
    }
  }
}

#[test]
pub fn test_stream_decoding_add() {
  let input = [183, 2, 1, 0, 147, 130, 2, 24, 147, 129, 2, 0, 19, 5, 0, 0, 147, 8, 96, 13, 115, 0, 0, 0, 19, 5, 117, 0, 147, 2, 128, 0, 179, 114, 85, 2, 51, 5, 85, 64, 147, 8, 96, 13, 115, 0, 0, 0, 35, 188, 161, 254, 19, 5, 0, 0, 147, 2, 129, 0, 19, 1, 129, 255, 35, 48, 81, 0, 239, 0, 64, 15, 19, 1, 129, 255, 35, 48, 161, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 208, 5, 115, 0, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 240, 3, 115, 0, 0, 0, 103, 128, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 0, 4, 115, 0, 0, 0, 103, 128, 0, 0, 131, 54, 1, 0, 19, 1, 129, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 19, 5, 192, 249, 147, 8, 128, 3, 115, 0, 0, 0, 103, 128, 0, 0, 131, 50, 1, 0, 19, 1, 129, 0, 147, 130, 114, 0, 19, 3, 128, 0, 51, 243, 98, 2, 179, 130, 98, 64, 3, 179, 129, 255, 51, 5, 83, 0, 147, 8, 96, 13, 115, 0, 0, 0, 99, 4, 101, 0, 99, 8, 0, 0, 99, 6, 80, 0, 19, 5, 0, 0, 99, 6, 0, 0, 35, 188, 161, 254, 19, 5, 3, 0, 103, 128, 0, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 16, 25, 115, 0, 0, 0, 19, 5, 8, 0, 103, 128, 0, 0, 19, 1, 129, 255, 35, 48, 17, 0, 19, 1, 129, 255, 35, 48, 129, 0, 19, 4, 1, 0, 147, 2, 112, 3, 19, 3, 160, 2, 179, 130, 98, 0, 19, 133, 2, 0, 111, 0, 64, 0, 19, 1, 4, 0, 3, 52, 1, 0, 19, 1, 129, 0, 131, 48, 1, 0, 19, 1, 129, 1, 103, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
  let stream = DecodingStream::new(&input);

  let instructions = stream.collect::<Vec<Instruction>>();

  assert_eq!(instructions.len(), 94);
}

#[test]
pub fn test_instruction_decoding() {

  fn decode_test(bytes: &[u8], expected: Instruction) {
    let mut stream = DecodingStream::new(bytes);
    let actual = stream.next().unwrap();
    assert_eq!(actual, expected);
  }
  
  /*
    183, 2, 1, 0,     // lui t0,0x10
    147, 130, 2, 24,  // addi t0,t0,384
    147, 129, 2, 0,   // addi gp,t0,0
    19, 5, 0, 0,      // addi a0,zero,0
    147, 8, 96, 13,   // addi a7,zero,214
    115, 0, 0, 0,     // ecall
    19, 5, 117, 0,    // addi a0,a0,7
    147, 2, 128, 0,   // addi t0,zero,8
    179, 114, 85, 2,  // remu t0,a0,t0
    51, 5, 85, 64,    // sub a0,a0,t0
    147, 8, 96, 13,   // addi a7,zero,214
    115, 0, 0, 0,     // ecall
    35, 188, 161, 254,// sd a0,-8(gp)
    19, 5, 0, 0,      // addi a0,zero,0
    147, 2, 129, 0,   // addi t0,sp,8
    19, 1, 129, 255,  // addi sp,sp,-8
    35, 48, 81, 0,    // sd t0,0(sp)
    239, 0, 64, 15,   // jal ra,61       
    19, 1, 129, 255,  // addi sp,sp,-8        
    35, 48, 161, 0,   // sd a0,0(sp)
    3, 53, 1, 0,      // ld a0,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    147, 8, 208, 5,   // addi a7,zero,93
    115, 0, 0, 0,     // ecall
    3, 54, 1, 0,      // ld a2,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    131, 53, 1, 0,    // ld a1,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    3, 53, 1, 0,      // ld a0,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    147, 8, 240, 3,   // addi a7,zero,63
    115, 0, 0, 0,     // ecall
    103, 128, 0, 0,   // jalr zero,0(ra)
    3, 54, 1, 0,      // ld a2,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    131, 53, 1, 0,    // ld a1,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    3, 53, 1, 0,      // ld a0,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    147, 8, 0, 4,     // addi a7,zero,64
    115, 0, 0, 0,     // ecall
    103, 128, 0, 0,   // jalr zero,0(ra)
    131, 54, 1, 0,    // ld a3,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    3, 54, 1, 0,      // ld a2,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    131, 53, 1, 0,    // ld a1,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    19, 5, 192, 249,  // addi a0,zero,-100
    147, 8, 128, 3,   // addi a7,zero,56
    115, 0, 0, 0,     // ecall
    103, 128, 0, 0,   // jalr zero,0(ra)
    131, 50, 1, 0,    // ld t0,0(sp)
    19, 1, 129, 0,    // addi sp,sp,8
    147, 130, 114, 0, // addi t0,t0,7
    19, 3, 128, 0,    // addi t1,zero,8
    51, 243, 98, 2,   // remu t1,t0,t1
    179, 130, 98, 64, // sub t0,t0,t1
    3, 179, 129, 255, // ld t1,-8(gp)
    51, 5, 83, 0,     // add a0,t1,t0
    147, 8, 96, 13,   // addi a7,zero,214
    115, 0, 0, 0,     // ecall
    99, 4, 101, 0,    // beq a0,t1,2
    99, 8, 0, 0,      // beq zero,zero,4
    99, 6, 80, 0,     // beq zero,t0,3
    19, 5, 0, 0,      // addi a0,zero,0
    99, 6, 0, 0,      // beq zero,zero,3
    35, 188, 161, 254,// sd a0,-8(gp)          
    19, 5, 3, 0,      // addi a0,t1,0    
    103, 128, 0, 0,   // jalr zero,0(ra)       
    131, 53, 1, 0,    // ld a1,0(sp)      
    19, 1, 129, 0,    // addi sp,sp,8      
    3, 53, 1, 0,      // ld a0,0(sp)    
    19, 1, 129, 0,    // addi sp,sp,8      
    147, 8, 16, 25,   // addi a7,zero,401       
    115, 0, 0, 0,     // ecall     
    19, 5, 8, 0,      // addi a0,a6,0    
    103, 128, 0, 0,   // jalr zero,0(ra)       
    19, 1, 129, 255,  // addi sp,sp,-8        
    35, 48, 17, 0,    // sd ra,0(sp)      
    19, 1, 129, 255,  // addi sp,sp,-8        
    35, 48, 129, 0,   // sd s0,0(sp)
    19, 4, 1, 0,      // addi s0,sp,0
    147, 2, 112, 3,   // addi t0,zero,55
    19, 3, 160, 2,    // addi t1,zero,42
    179, 130, 98, 0,  // add t0,t0,t1        
    19, 133, 2, 0,    // addi a0,t0,0      
    111, 0, 64, 0,    // jal zero,1
    19, 1, 4, 0,      // addi sp,s0,0    
    3, 52, 1, 0,      // ld s0,0(sp)    
    19, 1, 129, 0,    // addi sp,sp,8
    131, 48, 1, 0,    // ld ra,0(sp)
    19, 1, 129, 1,    // addi sp,sp,24
    103, 128, 0, 0,   // jalr zero,0(ra)
  */

  decode_test(&[183, 2, 1, 0],    Instruction::U { opcode: Opcode::Lui, imm: 0x10000, rd: Register::T0});
  decode_test(&[147, 130, 2, 24], Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::T0, rs1: Register::T0, imm: 384 });
  decode_test(&[19, 5, 0, 0],     Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::A0, rs1: Register::Zero, imm: 0 });
  decode_test(&[147, 8, 96, 13],  Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::A7, rs1: Register::Zero, imm: 214 });
  decode_test(&[0x93, 0x87, 0xe0, 0xFC], Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::A5, rs1: Register::ReturnAddress, imm: -50 });
  decode_test(&[115, 0, 0, 0],    Instruction::I { opcode: Opcode::System(SystemFunction::Environment(EnvironmentFunction::ECALL)), rd: Register::Zero, rs1: Register::Zero, imm: 0 });
  decode_test(&[239, 0, 64, 15],  Instruction::J { opcode: Opcode::JAl, rd: Register::ReturnAddress, imm: 122});
  decode_test(&[99, 4, 101, 0],   Instruction::B { opcode: Opcode::Branch(BranchOperation::Equal), rs1: Register::A0, rs2: Register::T1, imm: /*2?*/ 8});
  decode_test(&[0x83, 0x21, 0x72, 0x03], Instruction::I { opcode: Opcode::Load(LoadWidth::Word), imm: 55, rs1: Register::ThreadPointer, rd: Register::GlobalPointer });
  decode_test(&[35, 48, 81, 0], Instruction::S { opcode: Opcode::Store(StoreWidth::DoubleWord), rs2: Register::T0, rs1: Register::StackPointer, imm: 0 });
  decode_test(&[179, 130, 98, 64], Instruction::R { opcode: Opcode::Op(OpFunction::SUB), rs1: Register::T0, rs2: Register::T1, rd: Register::T0 });
}

pub struct EncodingStream {
  bytes: Vec<u8>
}

impl EncodingStream {
  pub fn new() -> Self {
    Self { 
      bytes: Vec::new()
    }
  }

  pub fn push(&mut self, instruction: Instruction) {
    self.bytes.append(&mut instruction.bytes());
  }

  pub fn bytes(&self) -> &[u8] {
    &self.bytes
  }
}

#[test]
pub fn test_instruction_roundtrip() {

  fn roundtrip_test(input: &[u8]) {
    let mut stream = DecodingStream::new(input);
    let instruction = stream.next().unwrap();

    let mut stream = EncodingStream::new();
    stream.push(instruction);

    let output = stream.bytes();
    assert_eq!(output, input);
  }

  roundtrip_test(&[183, 2, 1, 0]);
  roundtrip_test(&[147, 130, 2, 24]);
  roundtrip_test(&[19, 5, 0, 0]);
  roundtrip_test(&[147, 8, 96, 13]);
  roundtrip_test(&[0x93, 0x87, 0xe0, 0xFC]);
  roundtrip_test(&[115, 0, 0, 0]);
  roundtrip_test(&[239, 0, 64, 15]);
  roundtrip_test(&[99, 4, 101, 0]);
  roundtrip_test(&[0x83, 0x21, 0x72, 0x03]);
  roundtrip_test(&[35, 48, 81, 0]);
  roundtrip_test(&[179, 130, 98, 64]);
}
