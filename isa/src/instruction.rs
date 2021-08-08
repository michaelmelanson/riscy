use crate::{AmoOrdering, Opcode, Register};

/// A single RISC-V instruction. Each enum variant here is a different
/// instruction format, which have their different parameters.
///
/// # Resources
/// * [RISC-V specification v2.2](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf)
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
    /// R-type instructions
    R {
        opcode: Opcode,
        rd: Register,
        /*func3: u8,*/ rs1: Register,
        rs2: Register, /*, funct7: u8 */
    },
    I {
        opcode: Opcode,
        rd: Register,
        /*func3: u8,*/ rs1: Register,
        imm: i32,
    },
    IS {
        opcode: Opcode,
        rd: Register,
        /*func3: u8,*/ rs1: Register,
        imm: u32,
    },
    S {
        opcode: Opcode,
        /*func3: u8,*/ rs1: Register,
        rs2: Register,
        imm: i32,
    },
    B {
        opcode: Opcode,
        /*func3: u8,*/ rs1: Register,
        rs2: Register,
        imm: i32,
    },
    U {
        opcode: Opcode,
        rd: Register,
        imm: i32,
    },
    J {
        opcode: Opcode,
        rd: Register,
        imm: i32,
    },

    // "C" (Compressed) extension
    CIW {
        opcode: Opcode,
        rd: Register,
        imm: u16,
    },
    CS {
        opcode: Opcode,
        rs1: Register,
        rs2: Register,
        imm: u16,
    },
    CL {
        opcode: Opcode,
        rs1: Register,
        rd: Register,
        imm: u16,
    },
    CI {
        opcode: Opcode,
        rd: Register,
        imm: i64,
    },
    CNOP,
    CB {
        opcode: Opcode,
        rs1: Register,
        imm: i16,
    },
    CA {
        opcode: Opcode,
        rd: Register,
        rs2: Register,
    },
    CJ {
        opcode: Opcode,
        imm: i16,
    },
    CR {
        opcode: Opcode,
        rs1: Register,
        rs2: Register,
    },
    CSS {
        opcode: Opcode,
        imm: u16,
        rs2: Register,
    },

    // "A" (Atomic) extension
    AR {
        opcode: Opcode,
        aq: AmoOrdering,
        rl: AmoOrdering,
        rs2: Register,
        rs1: Register,
        rd: Register,
    },
}

impl Instruction {
    pub fn from_16bits(encoded: u16) -> Instruction {
        let opcode = Opcode::new_compressed(encoded);

        match opcode {
            Opcode::CADDI4SPN => {
                let nzuimm_5_4 = ((encoded >> 11) & 0b11) << 4;
                let nzuimm_9_6 = ((encoded >> 7) & 0b1111) << 6;
                let nzuimm_2 = ((encoded >> 6) & 0b1) << 2;
                let nzuimm_3 = ((encoded >> 5) & 0b1) << 3;
                let nzuimm = nzuimm_5_4 | nzuimm_9_6 | nzuimm_2 | nzuimm_3;

                let rd = ((encoded >> 2) & 0b111) as u8;

                let imm = nzuimm;
                let rd = Register::from_u8(rd + 8);

                Instruction::CIW { opcode, imm, rd }
            }

            Opcode::CNOP => Instruction::CNOP,

            Opcode::CADDI | Opcode::CSLLI => {
                let nzimm_5 = ((encoded >> 12) & 0b1) << 5;
                let nzimm_4_0 = (encoded >> 2) & 0b11111;

                let sign_bit = nzimm_5 != 0;

                let imm = (if sign_bit {
                    0b1111111111111111u16 << 6
                } else {
                    0
                }) | nzimm_5
                    | nzimm_4_0;
                let imm = imm as i16 as i64;

                let rd = Register::from_u8(((encoded >> 7) & 0b11111) as u8);

                Instruction::CI { opcode, imm, rd }
            }

            Opcode::CFLD => {
                let uimm_5_3 = ((encoded >> 10) & 0b111) << 3;
                let uimm_7_6 = ((encoded >> 5) & 0b11) << 6;
                let imm = uimm_5_3 | uimm_7_6;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);
                let rd = Register::from_rd_prime(((encoded >> 2) & 0b111) as u8);

                Instruction::CL {
                    opcode,
                    rs1,
                    rd,
                    imm,
                }
            }

            Opcode::CLW => {
                let imm = ((encoded >> 6) & 0b1) << 2
                    | ((encoded >> 10) & 0b111) << 3
                    | ((encoded >> 5) & 0b1) << 6;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);
                let rd = Register::from_rd_prime(((encoded >> 2) & 0b111) as u8);

                Instruction::CL {
                    opcode,
                    imm,
                    rs1,
                    rd,
                }
            }

            Opcode::CLD => {
                let imm = ((encoded >> 10) & 0b111) << 3 | ((encoded >> 5) & 0b11) << 6;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);
                let rd = Register::from_rd_prime(((encoded >> 2) & 0b111) as u8);

                Instruction::CL {
                    opcode,
                    imm,
                    rs1,
                    rd,
                }
            }

            Opcode::CADDI16SP => {
                let nzuimm_9 = ((encoded >> 12) & 0b1) << 9;
                let nzuimm_4 = ((encoded >> 6) & 0b1) << 4;
                let nzuimm_6 = ((encoded >> 5) & 0b1) << 6;
                let nzuimm_8_7 = ((encoded >> 3) & 0b11) << 7;
                let nzuimm_5 = ((encoded >> 2) & 0b1) << 5;

                let sign_bit = nzuimm_9 != 0;
                let imm = ((if sign_bit {
                    0b1111111111111111 << 10
                } else {
                    0
                }) | nzuimm_9
                    | nzuimm_4
                    | nzuimm_6
                    | nzuimm_8_7
                    | nzuimm_5) as i16 as i64;

                let rd = Register::StackPointer;

                Instruction::CI { opcode, imm, rd }
            }

            Opcode::CSW => {
                let imm = ((encoded >> 6) & 0b1) << 2
                    | ((encoded >> 10) & 0b111) << 3
                    | ((encoded >> 5) & 0b1) << 6;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);
                let rs2 = Register::from_rd_prime(((encoded >> 2) & 0b111) as u8);

                Instruction::CS {
                    opcode,
                    rs1,
                    rs2,
                    imm,
                }
            }

            Opcode::CSD => {
                let imm = ((encoded >> 10) & 0b111) << 3 | ((encoded >> 5) & 0b11) << 6;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);
                let rs2 = Register::from_rd_prime(((encoded >> 2) & 0b111) as u8);

                Instruction::CS {
                    opcode,
                    rs1,
                    rs2,
                    imm,
                }
            }

            Opcode::CADDIW => {
                let sign_bit = (encoded >> 12) & 0b1;

                let imm = (if sign_bit > 0 {
                    0b1111111111111111 << 5
                } else {
                    0
                }) | ((encoded >> 2) & 0b11111);
                let imm = imm as i16 as i64;

                let rd = Register::from_u8(((encoded >> 7) & 0b11111) as u8);

                Instruction::CI { opcode, imm, rd }
            }

            Opcode::CLI | Opcode::CLUI => {
                let sign_bit = (encoded >> 12) & 0b1;

                let imm = (if sign_bit > 0 { (-1i64 as u64) << 5 } else { 0 })
                    | ((encoded as u64 >> 2) & 0b11111);
                let imm = imm as i16 as i64;

                let rd = Register::from_u8(((encoded >> 7) & 0b11111) as u8);

                Instruction::CI { opcode, imm, rd }
            }

            Opcode::CLWSP => {
                let offset_5 = ((encoded >> 12) & 0b1) << 5;
                let offset_4_2 = ((encoded >> 4) & 0b111) << 2;
                let offset_7_6 = ((encoded >> 2) & 0b11) << 6;

                let imm = offset_5 | offset_4_2 | offset_7_6;
                let imm = imm as u64 as i64;

                let rd = Register::from_u8(((encoded >> 7) & 0b11111) as u8);

                Instruction::CI { opcode, imm, rd }
            }

            Opcode::CLDSP => {
                let offset_5 = ((encoded >> 12) & 0b1) << 5;
                let offset_4_3 = ((encoded >> 5) & 0b11) << 3;
                let offset_8_6 = ((encoded >> 2) & 0b111) << 6;

                let imm = offset_5 | offset_4_3 | offset_8_6;
                let imm = imm as u64 as i64;

                let rd = Register::from_u8(((encoded >> 7) & 0b11111) as u8);

                Instruction::CI { opcode, imm, rd }
            }

            Opcode::CSRLI | Opcode::CSRAI | Opcode::CANDI => {
                let shamt_5 = ((encoded >> 12) & 0b1) << 5;
                let shamt_4_0 = (encoded >> 2) & 0b11111;

                let sign_bit = shamt_5 != 0;
                let imm = (if sign_bit { (-1i16 as u16) << 6 } else { 0 }) | shamt_5 | shamt_4_0;
                let imm = imm as i16;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);

                Instruction::CB { opcode, imm, rs1 }
            }

            Opcode::CBEQZ | Opcode::CBNEZ => {
                let offset_8 = ((encoded >> 12) & 0b1) << 8;
                let offset_4_3 = ((encoded >> 10) & 0b11) << 3;
                let offset_7_6 = ((encoded >> 5) & 0b11) << 6;
                let offset_2_1 = ((encoded >> 3) & 0b11) << 1;
                let offset_5 = ((encoded >> 2) & 0b1) << 5;

                let sign_bit = offset_8 != 0;
                let imm = (if sign_bit { (-1i16 as u16) << 9 } else { 0 })
                    | offset_8
                    | offset_4_3
                    | offset_7_6
                    | offset_2_1
                    | offset_5;
                let imm = imm as i16;

                let rs1 = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);

                Instruction::CB { opcode, imm, rs1 }
            }

            Opcode::CAND
            | Opcode::COR
            | Opcode::CXOR
            | Opcode::CSUB
            | Opcode::CADDW
            | Opcode::CSUBW => {
                let rd = Register::from_rd_prime(((encoded >> 7) & 0b111) as u8);
                let rs2 = Register::from_rd_prime(((encoded >> 2) & 0b111) as u8);

                Instruction::CA { opcode, rd, rs2 }
            }

            Opcode::CJ => {
                let offset_11 = ((encoded >> 12) & 0b1) << 11;
                let offset_4 = ((encoded >> 11) & 0b1) << 4;
                let offset_9_8 = ((encoded >> 9) & 0b11) << 8;
                let offset_10 = ((encoded >> 8) & 0b1) << 10;
                let offset_6 = ((encoded >> 7) & 0b1) << 6;
                let offset_7 = ((encoded >> 6) & 0b1) << 7;
                let offset_3_1 = ((encoded >> 3) & 0b111) << 1;
                let offset_5 = ((encoded >> 2) & 0b1) << 5;

                let sign_bit = offset_11 != 0;
                let imm = (if sign_bit { (-1i16 as u16) << 12 } else { 0 })
                    | offset_11
                    | offset_4
                    | offset_9_8
                    | offset_10
                    | offset_6
                    | offset_7
                    | offset_3_1
                    | offset_5;

                Instruction::CJ {
                    opcode,
                    imm: imm as i16,
                }
            }

            Opcode::CJR | Opcode::CJALR | Opcode::CMV | Opcode::CADD => {
                let rs1 = Register::from_u8(((encoded >> 7) & 0b11111) as u8);
                let rs2 = Register::from_u8(((encoded >> 2) & 0b11111) as u8);
                Instruction::CR { opcode, rs1, rs2 }
            }

            Opcode::CSWSP => {
                let rs2 = Register::from_u8(((encoded >> 2) & 0b11111) as u8);
                let offset_5_2 = ((encoded >> 9) & 0b1111) << 2;
                let offset_7_6 = ((encoded >> 7) & 0b11) << 6;
                let imm = offset_5_2 | offset_7_6;

                Instruction::CSS { opcode, imm, rs2 }
            }

            Opcode::CSDSP => {
                let rs2 = Register::from_u8(((encoded >> 2) & 0b11111) as u8);
                let offset_5_3 = ((encoded >> 10) & 0b111) << 3;
                let offset_8_6 = ((encoded >> 7) & 0b111) << 6;
                let imm = offset_5_3 | offset_8_6;

                Instruction::CSS { opcode, imm, rs2 }
            }

            _ => unimplemented!("compressed opcode {:#?}", opcode),
        }
    }

    pub fn from_32bits(encoded: u32) -> Instruction {
        let funct3 = (encoded >> 12) & 0b111;
        let funct5 = (encoded >> 27) & 0b11111;
        let funct7 = (encoded >> 25) & 0b1111111;
        let imm11_0 = (encoded >> 20) & 0b111111111111;

        let opcode = Opcode::new(
            encoded as u16,
            funct3 as u8,
            imm11_0 as u16,
            funct5 as u8,
            funct7 as u8,
        );

        let instruction = match opcode {
            Opcode::Lui | Opcode::AuiPc => {
                let imm = ((encoded >> 12) & 0b11111111111111111111) << 12;
                let rd = (encoded >> 7) & 0b11111;

                Instruction::U {
                    opcode,
                    rd: Register::from_u8(rd as u8),
                    imm: imm as i32,
                }
            }

            Opcode::JAl => {
                let imm20 = ((encoded >> 31) & 0b1) << 20;
                let imm19_12 = ((encoded >> 12) & 0b11111111) << 12;
                let imm11 = ((encoded >> 20) & 0b1) << 11;
                let imm10_1 = ((encoded >> 21) & 0b1111111111) << 1;

                let sign_bit = (encoded >> 31) != 0;
                let sign_extension = if sign_bit { (-1i32 as u32) << 21 } else { 0 };

                let imm = sign_extension | imm20 | imm19_12 | imm11 | imm10_1;

                let rd = (encoded >> 7) & 0b11111;
                Instruction::J {
                    opcode,
                    rd: Register::from_u8(rd as u8),
                    imm: imm as i32,
                }
            }

            Opcode::JAlr
            | Opcode::Load(_)
            | Opcode::OpImm(_)
            | Opcode::OpImm32(_)
            | Opcode::MiscMem(_) => {
                let imm11 = ((encoded >> 31) & 0b1) << 11;
                let imm10_0 = (encoded >> 20) & 0b111111111111;
                let sign_bit = imm11 != 0;

                let imm: u32 = (if sign_bit { (-1i32 as u32) << 12 } else { 0 }) | imm11 | imm10_0;

                let rd = (encoded >> 7) & 0b11111;
                let rs1 = (encoded >> 15) & 0b11111;

                Instruction::I {
                    opcode,
                    rd: Register::from_u8(rd as u8),
                    imm: imm as i32,
                    rs1: Register::from_u8(rs1 as u8),
                }
            }

            Opcode::Branch(_) => {
                let imm12 = ((encoded >> 31) & 0b1) << 12;
                let imm10_5 = ((encoded >> 25) & 0b111111) << 5;
                let imm4_1 = ((encoded >> 8) & 0b1111) << 1;
                let imm11 = ((encoded >> 7) & 0b1) << 11;

                let sign_bit = imm12 != 0;

                let imm = (if sign_bit { (-1i32 as u32) << 13 } else { 0 })
                    | imm12
                    | imm10_5
                    | imm4_1
                    | imm11;

                let rs1 = (encoded >> 15) & 0b11111;
                let rs2 = (encoded >> 20) & 0b11111;

                Instruction::B {
                    opcode,
                    rs1: Register::from_u8(rs1 as u8),
                    rs2: Register::from_u8(rs2 as u8),
                    imm: imm as i32,
                }
            }

            Opcode::Store(_) => {
                let imm11 = (encoded >> 31) << 11;
                let imm10_5 = ((encoded >> 25) & 0b111111) << 5;
                let imm4_0 = (encoded >> 7) & 0b11111;
                let sign_bit = imm11 != 0;

                let imm =
                    (if sign_bit { (-1i32 as u32) << 12 } else { 0 }) | imm11 | imm10_5 | imm4_0;

                let rs2 = (encoded >> 20) & 0b11111;
                let rs1 = (encoded >> 15) & 0b11111;

                Instruction::S {
                    opcode,
                    rs2: Register::from_u8(rs2 as u8),
                    rs1: Register::from_u8(rs1 as u8),
                    imm: imm as i32,
                }
            }

            Opcode::Op(_) | Opcode::Op32(_) => {
                let rs2 = (encoded >> 20) & 0b11111;
                let rs1 = (encoded >> 15) & 0b11111;
                let rd = (encoded >> 7) & 0b11111;

                Instruction::R {
                    opcode,
                    rs2: Register::from_u8(rs2 as u8),
                    rs1: Register::from_u8(rs1 as u8),
                    rd: Register::from_u8(rd as u8),
                }
            }

            Opcode::System(_) => {
                let imm = (encoded >> 20) & 0b111111111111;
                let rs1 = (encoded >> 15) & 0b11111;
                let rd = (encoded >> 7) & 0b11111;

                Instruction::IS {
                    opcode,
                    rd: Register::from_u8(rd as u8),
                    imm: imm as u32,
                    rs1: Register::from_u8(rs1 as u8),
                }
            }

            Opcode::Amo(_func, _width) => {
                let aq = (encoded >> 26) & 0b1;
                let rl = (encoded >> 25) & 0b1;
                let rs2 = (encoded >> 20) & 0b11111;
                let rs1 = (encoded >> 15) & 0b11111;
                let rd = (encoded >> 7) & 0b11111;

                let aq = AmoOrdering::from_bit(aq as u8);
                let rl = AmoOrdering::from_bit(rl as u8);
                let rs2 = Register::from_u8(rs2 as u8);
                let rs1 = Register::from_u8(rs1 as u8);
                let rd = Register::from_u8(rd as u8);

                Instruction::AR {
                    opcode,
                    aq,
                    rl,
                    rs2,
                    rs1,
                    rd,
                }
            }

            _ => unimplemented!("opcode {:?}", opcode),
        };

        instruction
    }

    pub fn bytes(&self) -> Vec<u8> {
        fn encode_16bits(instruction: u16) -> Vec<u8> {
            instruction.to_le_bytes().to_vec()
        }

        fn encode_32bits(instruction: i32) -> Vec<u8> {
            instruction.to_le_bytes().to_vec()
        }

        match self {
            Instruction::R {
                rs2,
                rs1,
                rd,
                opcode,
            } => encode_32bits(
                (opcode.funct7_field() << 25)
                    | (rs2.encode() << 20)
                    | (rs1.encode() << 15)
                    | (opcode.funct3_field() << 12)
                    | (rd.encode() << 7)
                    | opcode.opcode_field(),
            ),

            Instruction::I {
                imm,
                rs1,
                rd,
                opcode,
            } => {
                let funct3 = opcode.funct3_field();

                encode_32bits(
                    (imm << 20)
                        | (rs1.encode() << 15)
                        | (funct3 << 12)
                        | (rd.encode() << 7)
                        | opcode.opcode_field(),
                )
            }

            Instruction::IS {
                imm,
                rs1,
                rd,
                opcode,
            } => {
                let funct3 = opcode.funct3_field();

                encode_32bits(
                    (imm << 20) as i32
                        | (rs1.encode() << 15)
                        | (funct3 << 12)
                        | (rd.encode() << 7)
                        | (opcode.opcode_field()),
                )
            }

            Instruction::S {
                imm,
                rs2,
                rs1,
                opcode,
            } => encode_32bits(
                (((imm >> 5) & 0b1111111) << 25)
                    | (rs2.encode() << 20)
                    | (rs1.encode() << 15)
                    | (opcode.funct3_field() << 12)
                    | ((imm & 0b11111) << 7)
                    | opcode.opcode_field(),
            ),

            Instruction::B {
                imm,
                rs2,
                rs1,
                opcode,
            } => encode_32bits(
                (((imm >> 20) & 0b1) << 31)
                    | (((imm >> 5) & 0b111111) << 25)
                    | (rs2.encode() << 20)
                    | (rs1.encode() << 15)
                    | (opcode.funct3_field() << 12)
                    | (((imm >> 1) & 0b1111) << 8)
                    | (((imm >> 11) & 0b1) << 7)
                    | opcode.opcode_field(),
            ),

            Instruction::U { imm, rd, opcode } => encode_32bits(
                rd.encode() << 7 | opcode.opcode_field() | (imm & 0b11111111111111111111),
            ),

            Instruction::J { imm, rd, opcode } => encode_32bits(
                (((imm >> 20) & 0b1) << 31)
                    | (((imm >> 1) & 0b1111111111) << 21)
                    | (((imm >> 11) & 0b1) << 20)
                    | (((imm >> 12) & 0b11111111) << 12)
                    | (rd.encode() << 7)
                    | opcode.opcode_field(),
            ),

            Instruction::CIW { opcode, rd, imm } => encode_16bits(
                ((opcode.funct3_field() as u16) << 13)
                    | (((*imm as u16 >> 2) & 0b11111111) << 5)
                    | ((rd.encode_prime() as u16) << 2)
                    | opcode.opcode_field() as u16,
            ),

            Instruction::CNOP => encode_16bits(0x00000001),

            Instruction::CS {
                opcode,
                rs1,
                rs2,
                imm,
            } => encode_16bits(
                ((opcode.funct3_field() as u16) << 13)
                    | ((*imm >> 3 & 0b111) << 10)
                    | (rs1.encode_prime() << 7)
                    | ((*imm >> 2 & 0b1) << 6)
                    | ((*imm >> 3 & 0b1) << 5)
                    | (rs2.encode_prime() << 2)
                    | (opcode.opcode_field() as u16),
            ),

            Instruction::CL {
                opcode,
                rs1,
                rd,
                imm,
            } => encode_16bits(
                ((opcode.funct3_field() as u16) << 13)
                    | ((*imm >> 3 & 0b111) << 10)
                    | (rs1.encode_prime() << 7)
                    | ((*imm >> 2 & 0b1) << 6)
                    | ((*imm >> 3 & 0b1) << 5)
                    | (rd.encode_prime() << 2)
                    | (opcode.opcode_field() as u16),
            ),

            Instruction::CI { opcode, imm, rd } => encode_16bits(
                ((opcode.funct3_field() as u16) << 13)
                    | (((*imm as u16 >> 5) & 0b1) << 12)
                    | ((rd.encode_prime() as u16) << 7)
                    | ((*imm as u16 & 0b11111) << 2)
                    | opcode.opcode_field() as u16,
            ),

            Instruction::CB {
                opcode: _,
                imm: _,
                rs1: _,
            } => todo!("encoding for CB-type"),
            Instruction::CA {
                opcode: _,
                rd: _,
                rs2: _,
            } => todo!("encoding for CB-type"),
            Instruction::CJ { opcode: _, imm: _ } => todo!("encoding for CB-type"),
            Instruction::CR {
                opcode: _,
                rs1: _,
                rs2: _,
            } => todo!("encoding for CR-type"),
            Instruction::CSS {
                opcode: _,
                rs2: _,
                imm: _,
            } => todo!("encoding for CSS-type"),

            Instruction::AR {
                opcode: _,
                aq: _,
                rl: _,
                rd: _,
                rs1: _,
                rs2: _,
            } => todo!("encoding for CSS-type"),
        }
    }

    pub fn width_bytes(&self) -> u64 {
        match self {
            Instruction::CIW {
                opcode: _,
                rd: _,
                imm: _,
            }
            | Instruction::CS {
                opcode: _,
                rs1: _,
                rs2: _,
                imm: _,
            }
            | Instruction::CL {
                opcode: _,
                rs1: _,
                rd: _,
                imm: _,
            }
            | Instruction::CI {
                opcode: _,
                rd: _,
                imm: _,
            }
            | Instruction::CNOP
            | Instruction::CB {
                opcode: _,
                rs1: _,
                imm: _,
            }
            | Instruction::CA {
                opcode: _,
                rd: _,
                rs2: _,
            }
            | Instruction::CJ { opcode: _, imm: _ }
            | Instruction::CR {
                opcode: _,
                rs1: _,
                rs2: _,
            }
            | Instruction::CSS {
                opcode: _,
                imm: _,
                rs2: _,
            } => 2,

            Instruction::R {
                opcode: _,
                rd: _,
                rs1: _,
                rs2: _,
            }
            | Instruction::I {
                opcode: _,
                rd: _,
                rs1: _,
                imm: _,
            }
            | Instruction::IS {
                opcode: _,
                rd: _,
                rs1: _,
                imm: _,
            }
            | Instruction::S {
                opcode: _,
                rs1: _,
                rs2: _,
                imm: _,
            }
            | Instruction::B {
                opcode: _,
                rs1: _,
                rs2: _,
                imm: _,
            }
            | Instruction::U {
                opcode: _,
                rd: _,
                imm: _,
            }
            | Instruction::J {
                opcode: _,
                rd: _,
                imm: _,
            }
            | Instruction::AR {
                opcode: _,
                aq: _,
                rl: _,
                rd: _,
                rs1: _,
                rs2: _,
            } => 4,
        }
    }
}
