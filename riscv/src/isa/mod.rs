use std::io::Cursor;
use byteorder::{LittleEndian, ReadBytesExt};

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
  Load,
  LoadFp,
  Custom0,
  MiscMem,
  OpImm(OpImmFunction),
  AuiPc,
  OpImm32,

  Store,
  StoreFp,
  Custom1,
  Amo,
  Op,
  Lui,
  Op32,

  MAdd,
  MSub,
  NMSub,
  NMAdd,
  OpFp,
  Reserved0,
  Custom2,

  Branch,
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
  ADDI,
  SLTI,
  SLTIU,
  XORI,
  ORI,
  ANDI,
}

impl OpImmFunction {
  pub fn from_u8(value: u8) -> Self {
    match value {
      0b000 => OpImmFunction::ADDI,
      0b010 => OpImmFunction::SLTI,
      0b011 => OpImmFunction::SLTIU,
      0b110 => OpImmFunction::ORI,
      0b111 => OpImmFunction::ANDI,

      _ => unimplemented!()
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
pub enum Instruction {
  R { opcode: Opcode, rd: Register, /*func3: u8,*/ rs1: Register, rs2: Register, funct7: u8 },
  I { opcode: Opcode, rd: Register, /*func3: u8,*/ rs1: Register, imm: u16 },
  S { opcode: Opcode,               /*func3: u8,*/ rs1: Register, imm: u16 },
  U { opcode: Opcode, rd: Register,                               imm: u32 },
}

impl Instruction {

  fn opcode(base: u16, func3: u8, imm11_0: u16) -> Opcode {
    let opcode = base & 0b1111111;
    match opcode {
      0b0000011 => Opcode::Load,
      0b0000111 => Opcode::LoadFp,
      0b0001011 => Opcode::Custom0,
      0b0001111 => Opcode::MiscMem,
      0b0010011 => Opcode::OpImm(OpImmFunction::from_u8(func3)),
      0b0010111 => Opcode::AuiPc,
      0b0011011 => Opcode::OpImm32,

      0b0100011 => Opcode::Store,
      0b0100111 => Opcode::StoreFp,
      0b0101011 => Opcode::Custom1,
      0b0101111 => Opcode::Amo,
      0b0110011 => Opcode::Op,
      0b0110111 => Opcode::Lui,
      0b0111011 => Opcode::Op32,

      0b1000011 => Opcode::MAdd,
      0b1000111 => Opcode::MSub,
      0b1001011 => Opcode::NMSub,
      0b1001111 => Opcode::NMAdd,
      0b1010011 => Opcode::OpFp,
      0b1010111 => Opcode::Reserved0,
      0b1011011 => Opcode::Custom2,

      0b1100011 => Opcode::Branch,
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
    let func3: u8     = ((encoded >> 12) as u8)  & 0b111;
    let rd: u8        = ((encoded >> 7) as u8)   & 0b11111;
    let imm11_0: u16  = ((encoded >> 20) as u16) & 0b11111111111;
    let opcode = Instruction::opcode(encoded as u16, func3, imm11_0);

    let from_u_type = |opcode| {
      let imm31_12: u32 = encoded >> 12;
      Instruction::U { opcode, rd: Register::from_u8(rd), imm: imm31_12 }
    };

    let from_i_type = |opcode| {
      let rs1: u8       = ((encoded >> 15) as u8)  & 0b11111;
      Instruction::I { opcode, rd: Register::from_u8(rd), imm: imm11_0, rs1: Register::from_u8(rs1) }
    };

    // let imm4_0: u8    = ((encoded >> 7) as u8)   & 0b11111;
    // let rs2: u8       = ((encoded >> 20) as u8)  & 0b1111;
    // let func7: u8     = ((encoded >> 25) as u8)  & 0b1111111;

    let instruction = match opcode {
      Opcode::Lui | 
      Opcode::AuiPc => from_u_type(opcode),
      Opcode::OpImm(_) | Opcode::System(_) => from_i_type(opcode),

      _ => unimplemented!()
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

//#[test]
pub fn test_stream_decoding() {
  let input = [183, 2, 1, 0, 147, 130, 2, 24, 147, 129, 2, 0, 19, 5, 0, 0, 147, 8, 96, 13, 115, 0, 0, 0, 19, 5, 117, 0, 147, 2, 128, 0, 179, 114, 85, 2, 51, 5, 85, 64, 147, 8, 96, 13, 115, 0, 0, 0, 35, 188, 161, 254, 19, 5, 0, 0, 147, 2, 129, 0, 19, 1, 129, 255, 35, 48, 81, 0, 239, 0, 64, 15, 19, 1, 129, 255, 35, 48, 161, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 208, 5, 115, 0, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 240, 3, 115, 0, 0, 0, 103, 128, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 0, 4, 115, 0, 0, 0, 103, 128, 0, 0, 131, 54, 1, 0, 19, 1, 129, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 19, 5, 192, 249, 147, 8, 128, 3, 115, 0, 0, 0, 103, 128, 0, 0, 131, 50, 1, 0, 19, 1, 129, 0, 147, 130, 114, 0, 19, 3, 128, 0, 51, 243, 98, 2, 179, 130, 98, 64, 3, 179, 129, 255, 51, 5, 83, 0, 147, 8, 96, 13, 115, 0, 0, 0, 99, 4, 101, 0, 99, 8, 0, 0, 99, 6, 80, 0, 19, 5, 0, 0, 99, 6, 0, 0, 35, 188, 161, 254, 19, 5, 3, 0, 103, 128, 0, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 16, 25, 115, 0, 0, 0, 19, 5, 8, 0, 103, 128, 0, 0, 19, 1, 129, 255, 35, 48, 17, 0, 19, 1, 129, 255, 35, 48, 129, 0, 19, 4, 1, 0, 147, 2, 112, 3, 19, 3, 160, 2, 179, 130, 98, 0, 19, 133, 2, 0, 111, 0, 64, 0, 19, 1, 4, 0, 3, 52, 1, 0, 19, 1, 129, 0, 131, 48, 1, 0, 19, 1, 129, 1, 103, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
  let stream = InstructionStream::new(&input);

  let instructions = stream.collect::<Vec<Instruction>>();

  assert_eq!(instructions.len(), 94);
}

#[test]
pub fn test_instruction_decoding() {
  let input = [
    183, 2, 1, 0, 
    147, 130, 2, 24, 
    147, 129, 2, 0, 
    19, 5, 0, 0, 
    147, 8, 96, 13, 
    115, 0, 0, 0, 19, 5, 117, 0, 147, 2, 128, 0, 179, 114, 85, 2, 51, 5, 85, 64, 147, 8, 96, 13, 115, 0, 0, 0, 35, 188, 161, 254, 19, 5, 0, 0, 147, 2, 129, 0, 19, 1, 129, 255, 35, 48, 81, 0, 239, 0, 64, 15, 19, 1, 129, 255, 35, 48, 161, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 208, 5, 115, 0, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 240, 3, 115, 0, 0, 0, 103, 128, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 0, 4, 115, 0, 0, 0, 103, 128, 0, 0, 131, 54, 1, 0, 19, 1, 129, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 19, 5, 192, 249, 147, 8, 128, 3, 115, 0, 0, 0, 103, 128, 0, 0, 131, 50, 1, 0, 19, 1, 129, 0, 147, 130, 114, 0, 19, 3, 128, 0, 51, 243, 98, 2, 179, 130, 98, 64, 3, 179, 129, 255, 51, 5, 83, 0, 147, 8, 96, 13, 115, 0, 0, 0, 99, 4, 101, 0, 99, 8, 0, 0, 99, 6, 80, 0, 19, 5, 0, 0, 99, 6, 0, 0, 35, 188, 161, 254, 19, 5, 3, 0, 103, 128, 0, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 16, 25, 115, 0, 0, 0, 19, 5, 8, 0, 103, 128, 0, 0, 19, 1, 129, 255, 35, 48, 17, 0, 19, 1, 129, 255, 35, 48, 129, 0, 19, 4, 1, 0, 147, 2, 112, 3, 19, 3, 160, 2, 179, 130, 98, 0, 19, 133, 2, 0, 111, 0, 64, 0, 19, 1, 4, 0, 3, 52, 1, 0, 19, 1, 129, 0, 131, 48, 1, 0, 19, 1, 129, 1, 103, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ];

  let mut stream = InstructionStream::new(&input);

  assert_eq!(stream.next().unwrap(), Instruction::U { opcode: Opcode::Lui, imm: 0x10, rd: Register::T0});
  assert_eq!(stream.next().unwrap(), Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::T0, rs1: Register::T0, imm: 384 });
  assert_eq!(stream.next().unwrap(), Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::GlobalPointer, rs1: Register::T0, imm: 0 });
  assert_eq!(stream.next().unwrap(), Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::A0, rs1: Register::Zero, imm: 0 });
  assert_eq!(stream.next().unwrap(), Instruction::I { opcode: Opcode::OpImm(OpImmFunction::ADDI), rd: Register::A7, rs1: Register::Zero, imm: 214 });
  assert_eq!(stream.next().unwrap(), Instruction::I { opcode: Opcode::System(SystemFunction::ECALL), rd: Register::Zero, rs1: Register::Zero, imm: 0 });
  //ecall
}
