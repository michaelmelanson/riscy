use std::io::Cursor;
use byteorder::{LittleEndian, ReadBytesExt};

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
  Load(LoadWidth),
  LoadFp,
  Custom0,
  MiscMem,
  OpImm(OpImmFunction),
  AuiPc,
  OpImm32,

  Store(StoreWidth),
  StoreFp,
  Custom1,
  Amo,
  Op(OpFunction),
  Lui,
  Op32,

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


#[derive(Debug, PartialEq)]
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
      32 => Register::T6,                  

      _ => unimplemented!()
    }
  }
}

pub enum FPRegister {
  FS0, FS1, FS2, FS3, FS4, FS5,   // f0-15 (FP saved registers)
  FS6, FS7, FS8, FS9, FS10, FS11,
  FS12, FS13, FS14, FS15,        
  FV0, FV1,                       // f16-17 (FP return values)
  FA0, FA1, FA2, FA3, FA4, FA5,   // f18-25 (FP arguments)
  FA6, FA7,
  FT0, FT1, FT2, FT3, FT4, FT5    // ft0-5 (FP temporaries)
}

#[derive(Debug, PartialEq)]
pub enum OpImmFunction {
  ADD,
  SLT,
  SLTU,
  XOR,
  OR,
  AND,
}

impl OpImmFunction {
  pub fn from_u8(value: u8) -> Self {
    match value {
      0b000 => OpImmFunction::ADD,
      0b010 => OpImmFunction::SLT,
      0b011 => OpImmFunction::SLTU,
      0b110 => OpImmFunction::OR,
      0b111 => OpImmFunction::AND,

      _ => unimplemented!()
    }
  }
}
#[derive(Debug, PartialEq)]
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
}

#[derive(Debug, PartialEq)]
pub enum SystemFunction {
  ECALL,
  EBREAK,
}

impl SystemFunction {
  pub fn from_imm(imm11_0: u16) -> Self {
    match imm11_0 {
      0 => SystemFunction::ECALL,
      1 => SystemFunction::EBREAK,
      _ => unimplemented!()
    }
  }
}


#[derive(Debug, PartialEq)]
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
}


#[derive(Debug, PartialEq)]
pub enum LoadWidth {
  Byte,
  HalfWord,
  Word,
  DoubleWord,
  ByteUnsigned,
  HalfWordUnsigned
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

      _ => unimplemented!("Load width {:03b}", func3)
    }
  }
}


#[derive(Debug, PartialEq)]
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
}


#[derive(Debug, PartialEq)]
pub enum Instruction {
  R { opcode: Opcode, rd: Register, /*func3: u8,*/ rs1: Register, rs2: Register /*, funct7: u8 */ },
  I { opcode: Opcode, rd: Register, /*func3: u8,*/ rs1: Register, imm: i32 },
  S { opcode: Opcode,               /*func3: u8,*/ rs1: Register, rs2: Register, imm: i32 },
  B { opcode: Opcode,               /*func3: u8,*/ rs1: Register, rs2: Register, imm: i32 },
  U { opcode: Opcode, rd: Register,                               imm: i32 },
  J { opcode: Opcode, rd: Register,                               imm: i32 }
}

impl Instruction {

  fn opcode(base: u16, func3: u8, imm11_0: u16, func7: u8) -> Opcode {
    let opcode = base & 0b1111111;
    match opcode {
      0b0000011 => Opcode::Load(LoadWidth::from_func3(func3)),
      0b0000111 => Opcode::LoadFp,
      0b0001011 => Opcode::Custom0,
      0b0001111 => Opcode::MiscMem,
      0b0010011 => Opcode::OpImm(OpImmFunction::from_u8(func3)),
      0b0010111 => Opcode::AuiPc,
      0b0011011 => Opcode::OpImm32,

      0b0100011 => Opcode::Store(StoreWidth::from_func3(func3)),
      0b0100111 => Opcode::StoreFp,
      0b0101011 => Opcode::Custom1,
      0b0101111 => Opcode::Amo,
      0b0110011 => Opcode::Op(OpFunction::from_func3_func7(func3, func7)),
      0b0110111 => Opcode::Lui,
      0b0111011 => Opcode::Op32,

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
      0b1110011 => Opcode::System(SystemFunction::from_imm(imm11_0)),
      0b1110111 => Opcode::Reserved2,
      0b1111011 => Opcode::Custom3,

      _ => panic!("Unknown opcode")
    }
  }


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
    let opcode = Instruction::opcode(
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
      let imm = (imm11_5 << 5) | imm4_0;

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

      let imm = (if sign_bit > 0 { 0b11111111111 << 20 } else { 0 })
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
      Opcode::Op(_)     => from_r_type(opcode),
      Opcode::System(_) => from_i_type(opcode),

      _ => unimplemented!("opcode {:?}", opcode)
    };

    println!("Instruction: {:?}", instruction);

    instruction
  }

}

#[derive(Debug)]
pub enum ReadError {
  InvalidInstruction
}

pub struct InstructionStream<'a> {
  cursor: Cursor<&'a [u8]>
}

impl <'a> InstructionStream<'a> {
  pub fn new(bytes: &'a [u8]) -> InstructionStream {
    InstructionStream {
      cursor: Cursor::new(bytes)
    }
  }
}

impl<'a> Iterator for InstructionStream<'a> {
  type Item = Instruction;

  fn next(&mut self) -> Option<<Self as Iterator>::Item> { 
    let base: u16 = match self.cursor.read_u16::<LittleEndian>() {
      Ok(base) => base,
      Err(_) => { return None; }
    };

    if base == 0 { return None; }

    let ones = base.trailing_ones();
    
    match ones {
      1 => { 
        println!("16-bit instruction: {:#018b}", base);
        unimplemented!();
      },

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

        println!("48-bit instruction: {:?}", parcels);
        unimplemented!();
      },

      6 => {
        let parcels = [
          base,
          self.cursor.read_u16::<LittleEndian>().unwrap(),
          self.cursor.read_u16::<LittleEndian>().unwrap(),
          self.cursor.read_u16::<LittleEndian>().unwrap()
        ];
        println!("64-bit instruction: {:?}", parcels);
        unimplemented!();
      },

      7..=11 => { 
        println!("Large instruction ({} trailing ones): {:#018b}", ones, base);
        unimplemented!();
      },

      _ => { 
        println!("Invalid instruction ({} trailing ones): {:#018b}", ones, base); 
        None
      },
    }
  }
}

#[test]
pub fn test_stream_decoding_add() {
  let input = [183, 2, 1, 0, 147, 130, 2, 24, 147, 129, 2, 0, 19, 5, 0, 0, 147, 8, 96, 13, 115, 0, 0, 0, 19, 5, 117, 0, 147, 2, 128, 0, 179, 114, 85, 2, 51, 5, 85, 64, 147, 8, 96, 13, 115, 0, 0, 0, 35, 188, 161, 254, 19, 5, 0, 0, 147, 2, 129, 0, 19, 1, 129, 255, 35, 48, 81, 0, 239, 0, 64, 15, 19, 1, 129, 255, 35, 48, 161, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 208, 5, 115, 0, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 240, 3, 115, 0, 0, 0, 103, 128, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 0, 4, 115, 0, 0, 0, 103, 128, 0, 0, 131, 54, 1, 0, 19, 1, 129, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 19, 5, 192, 249, 147, 8, 128, 3, 115, 0, 0, 0, 103, 128, 0, 0, 131, 50, 1, 0, 19, 1, 129, 0, 147, 130, 114, 0, 19, 3, 128, 0, 51, 243, 98, 2, 179, 130, 98, 64, 3, 179, 129, 255, 51, 5, 83, 0, 147, 8, 96, 13, 115, 0, 0, 0, 99, 4, 101, 0, 99, 8, 0, 0, 99, 6, 80, 0, 19, 5, 0, 0, 99, 6, 0, 0, 35, 188, 161, 254, 19, 5, 3, 0, 103, 128, 0, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 16, 25, 115, 0, 0, 0, 19, 5, 8, 0, 103, 128, 0, 0, 19, 1, 129, 255, 35, 48, 17, 0, 19, 1, 129, 255, 35, 48, 129, 0, 19, 4, 1, 0, 147, 2, 112, 3, 19, 3, 160, 2, 179, 130, 98, 0, 19, 133, 2, 0, 111, 0, 64, 0, 19, 1, 4, 0, 3, 52, 1, 0, 19, 1, 129, 0, 131, 48, 1, 0, 19, 1, 129, 1, 103, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
  let stream = InstructionStream::new(&input);

  let instructions = stream.collect::<Vec<Instruction>>();

  assert_eq!(instructions.len(), 94);
}

#[test]
pub fn test_instruction_decoding() {

  fn decode_test(bytes: &[u8], expected: Instruction) {
    let mut stream = InstructionStream::new(bytes);
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
  decode_test(&[147, 130, 2, 24], Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADD), rd: Register::T0, rs1: Register::T0, imm: 384 });
  decode_test(&[19, 5, 0, 0],     Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADD), rd: Register::A0, rs1: Register::Zero, imm: 0 });
  decode_test(&[147, 8, 96, 13],  Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADD), rd: Register::A7, rs1: Register::Zero, imm: 214 });
  decode_test(&[0x93, 0x87, 0xe0, 0xFC], Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADD), rd: Register::A5, rs1: Register::ReturnAddress, imm: -50 });
  decode_test(&[115, 0, 0, 0],    Instruction::I { opcode: Opcode::System(SystemFunction::ECALL), rd: Register::Zero, rs1: Register::Zero, imm: 0 });
  decode_test(&[239, 0, 64, 15],  Instruction::J { opcode: Opcode::JAl, rd: Register::ReturnAddress, imm: 122});
  decode_test(&[99, 4, 101, 0],   Instruction::B { opcode: Opcode::Branch(BranchOperation::Equal), rs1: Register::A0, rs2: Register::T1, imm: /*2?*/ 8});
  decode_test(&[0x83, 0x21, 0x72, 0x03], Instruction::I { opcode: Opcode::Load(LoadWidth::Word), imm: 55, rs1: Register::ThreadPointer, rd: Register::GlobalPointer });
  decode_test(&[35, 48, 81, 0], Instruction::S { opcode: Opcode::Store(StoreWidth::DoubleWord), rs2: Register::T0, rs1: Register::StackPointer, imm: 0 });
}
