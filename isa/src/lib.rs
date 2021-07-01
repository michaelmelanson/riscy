use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;

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
    Amo(AmoFunction, AmoWidth),
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

    // "C" (Compressed) extension
    CADDI4SPN,
    CADDI,
    CNOP,
    CFLD,
    CLW,
    CLD,
    CSW,
    CSD,
    CADDIW,
    CLI,
    CADDI16SP,
    CLUI,
    CSRLI,
    CSRAI,
    CANDI,
    CSUB,
    CXOR,
    COR,
    CAND,
    CSUBW,
    CADDW,
    CJ,
    CBEQZ,
    CBNEZ,
    CSLLI,
    CLWSP,
    CLDSP,
    CJR,
    CMV,
    CBREAK,
    CJALR,
    CADD,
    CSWSP,
    CSDSP,
}

impl Opcode {
    fn new_compressed(base: u16) -> Opcode {
        let op_low = base & 0b11;
        let op_high = (base >> 13) & 0b111;

        match (op_high, op_low) {
            (0b000, 0b00) => Opcode::CADDI4SPN,
            (0b000, 0b01) => {
                if (base >> 7) & 0b11111 == 0 {
                    Opcode::CNOP
                } else {
                    Opcode::CADDI
                }
            }
            (0b001, 0b00) => Opcode::CFLD, // or is it C.LQ?
            (0b010, 0b00) => Opcode::CLW,
            (0b011, 0b00) => Opcode::CLD,
            (0b110, 0b00) => Opcode::CSW,
            (0b111, 0b00) => Opcode::CSD,
            (0b001, 0b01) => Opcode::CADDIW,
            (0b010, 0b01) => Opcode::CLI,
            (0b011, 0b01) => {
                if (base >> 7) & 0b11111 == 2 {
                    Opcode::CADDI16SP
                } else {
                    Opcode::CLUI
                }
            }
            (0b100, 0b01) => {
                let func3 = (base >> 10) & 0b111;
                let func2 = (base >> 5) & 0b11;
                match (func3, func2) {
                    (0b000, _) | (0b100, _) => Opcode::CSRLI,
                    (0b001, _) | (0b101, _) => Opcode::CSRAI,
                    (0b010, _) | (0b110, _) => Opcode::CANDI,
                    (0b011, 0b00) => Opcode::CSUB,
                    (0b011, 0b01) => Opcode::CXOR,
                    (0b011, 0b10) => Opcode::COR,
                    (0b011, 0b11) => Opcode::CAND,
                    (0b111, 0b00) => Opcode::CSUBW,
                    (0b111, 0b01) => Opcode::CADDW,
                    _ => todo!(
                        "compressed arithmetic function with func=({:#03b}, {:#02b})",
                        func3,
                        func2
                    ),
                }
            }
            (0b101, 0b01) => Opcode::CJ,
            (0b110, 0b01) => Opcode::CBEQZ,
            (0b111, 0b01) => Opcode::CBNEZ,
            (0b000, 0b10) => Opcode::CSLLI,
            (0b010, 0b10) => Opcode::CLWSP,
            (0b011, 0b10) => Opcode::CLDSP,
            (0b100, 0b10) => {
                let func12 = (base >> 12) & 0b1;
                let func11_7 = (base >> 7) & 0b11111;
                let func6_2 = (base >> 2) & 0b11111;

                match (func12, func11_7, func6_2) {
                    (0b0, _, 0b00000) => Opcode::CJR,
                    (0b0, _, _) => Opcode::CMV,
                    (0b1, 0b0, 0b00000) => unimplemented!("C.EBREAK"),
                    (0b1, _, 0b00000) => Opcode::CJALR,
                    (0b1, _, _) => Opcode::CADD,
                    spec => unimplemented!("compressed opcode 100...10 with func ({:?})", spec),
                }
            }
            (0b110, 0b10) => Opcode::CSWSP,
            (0b111, 0b10) => Opcode::CSDSP,

            _ => todo!(
                "compressed instruction with op={:03b}...{:02b} from base={:016b} ({:#04x})",
                op_high,
                op_low,
                base,
                base
            ),
        }
    }

    fn new(base: u16, func3: u8, imm11_0: u16, func5: u8, func7: u8) -> Opcode {
        let opcode = base & 0b1111111;
        match opcode {
            0b0000011 => Opcode::Load(LoadWidth::from_func3(func3)),
            0b0000111 => Opcode::LoadFp,
            0b0001011 => unimplemented!("Custom 0"),
            0b0001111 => Opcode::MiscMem(MiscMemFunction::from_func3(func3)),
            0b0010011 => Opcode::OpImm(OpImmFunction::from_imm11_0_func3(imm11_0, func3)),
            0b0010111 => Opcode::AuiPc,
            0b0011011 => Opcode::OpImm32(OpImm32Function::from_func3_imm(func3, imm11_0)),

            0b0100011 => Opcode::Store(StoreWidth::from_func3(func3)),
            0b0100111 => Opcode::StoreFp,
            0b0101011 => unimplemented!("Custom 1"),
            0b0101111 => Opcode::Amo(AmoFunction::from_func5(func5), AmoWidth::from_func3(func3)),
            0b0110011 => Opcode::Op(OpFunction::from_func3_func7(func3, func7)),
            0b0110111 => Opcode::Lui,
            0b0111011 => Opcode::Op32(Op32Function::from_func3_func7(func3, func7)),

            0b1000011 => Opcode::MAdd,
            0b1000111 => Opcode::MSub,
            0b1001011 => Opcode::NMSub,
            0b1001111 => Opcode::NMAdd,
            0b1010011 => Opcode::OpFp,
            0b1010111 => unimplemented!("Reserved 0"),
            0b1011011 => unimplemented!("Custom 2"),

            0b1100011 => Opcode::Branch(BranchOperation::from_func3(func3)),
            0b1100111 => Opcode::JAlr,
            0b1101011 => unimplemented!("Reserved 1"),
            0b1101111 => Opcode::JAl,
            0b1110011 => Opcode::System(SystemFunction::from_func3_imm11_0(func3, imm11_0)),
            0b1110111 => unimplemented!("Reserved 2"),
            0b1111011 => unimplemented!("Custom 3"),

            _ => panic!("Unknown opcode"),
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
            Opcode::Amo(_, _) => 0b0101111,
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
            Opcode::Custom3 => 0b1111011,

            Opcode::CADDI4SPN => 0b00,
            Opcode::CADDI => 0b01,
            Opcode::CNOP => 0b01,
            Opcode::CFLD => 0b00,
            Opcode::CLW => 0b00,
            Opcode::CLD => 0b00,
            Opcode::CSW => 0b00,
            Opcode::CSD => 0b00,
            Opcode::CADDIW => 0b01,
            Opcode::CLI => 0b01,
            Opcode::CADDI16SP => 0b01,
            Opcode::CLUI => 0b01,
            Opcode::CSRLI => 0b01,
            Opcode::CSRAI => 0b01,
            Opcode::CANDI => 0b01,
            Opcode::CSUB => 0b01,
            Opcode::CXOR => 0b01,
            Opcode::COR => 0b01,
            Opcode::CAND => 0b01,
            Opcode::CSUBW => 0b01,
            Opcode::CADDW => 0b01,
            Opcode::CJ => 0b01,
            Opcode::CBEQZ => 0b01,
            Opcode::CBNEZ => 0b01,
            Opcode::CSLLI => 0b10,
            Opcode::CLWSP => 0b10,
            Opcode::CLDSP => 0b10,
            Opcode::CJR => 0b10,
            Opcode::CMV => 0b10,
            Opcode::CBREAK => 0b10,
            Opcode::CJALR => 0b10,
            Opcode::CADD => 0b10,
            Opcode::CSWSP => 0b10,
            Opcode::CSDSP => 0b10,
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

            _ => unimplemented!("opcode {:?} does not have funct3 field", self),
        }
    }

    pub fn funct7_field(&self) -> i32 {
        match self {
            Opcode::Op(function) => function.to_func7(),

            _ => unimplemented!("opcode {:?} does not have funct7 field", self),
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
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl Register {
    // used in compressed data formats
    pub fn from_rd_prime(value: u8) -> Register {
        Register::from_u8(value + 8)
    }

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

            _ => unimplemented!("register {}", value),
        }
    }

    pub fn encode_prime(&self) -> u16 {
        self.encode() as u16 - 8
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
    FS0,
    FS1,
    FS2,
    FS3,
    FS4,
    FS5, // f0-15 (FP saved registers)
    FS6,
    FS7,
    FS8,
    FS9,
    FS10,
    FS11,
    FS12,
    FS13,
    FS14,
    FS15,
    FV0,
    FV1, // f16-17 (FP return values)
    FA0,
    FA1,
    FA2,
    FA3,
    FA4,
    FA5, // f18-25 (FP arguments)
    FA6,
    FA7,
    FT0,
    FT1,
    FT2,
    FT3,
    FT4,
    FT5, // ft0-5 (FP temporaries)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MiscMemFunction {
    FENCE,
    FENCEI,
}

impl MiscMemFunction {
    pub fn from_func3(func3: u8) -> Self {
        match func3 {
            0b000 => MiscMemFunction::FENCE,
            0b001 => MiscMemFunction::FENCEI,

            _ => unimplemented!("MISC-MEM for func3={:#03b}", func3),
        }
    }

    pub fn to_func3(&self) -> i32 {
        match self {
            MiscMemFunction::FENCE => 0b000,
            MiscMemFunction::FENCEI => 0b001,
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

            _ => unimplemented!("OP-IMM for func3={:#03b}", func3),
        }
    }

    pub fn to_func3(&self) -> i32 {
        match self {
            OpImmFunction::ADDI => 0b000,
            OpImmFunction::SLTI => 0b010,
            OpImmFunction::SLTIU => 0b011,
            OpImmFunction::XORI => 0b100,
            OpImmFunction::ORI => 0b110,
            OpImmFunction::ANDI => 0b111,

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

            _ => unimplemented!("OP-IMM with func3={:#03b}, imm11_0={:#08x}", func3, imm11_0),
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
pub enum AmoWidth {
    Word,
    DoubleWord,
}

impl AmoWidth {
    pub fn from_func3(func3: u8) -> Self {
        match func3 {
            0b010 => AmoWidth::Word,
            0b011 => AmoWidth::DoubleWord,
            _ => unimplemented!("AMO width {:#03b}", func3),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AmoFunction {
    LR,
    SC,
    SWAP,
    ADD,
    AND,
    OR,
    XOR,
    MAX,
    MIN,
    MAXU,
    MINU,
}

impl AmoFunction {
    pub fn from_func5(func5: u8) -> Self {
        match func5 {
            0b00010 => AmoFunction::LR,
            0b00011 => AmoFunction::SC,
            0b00001 => AmoFunction::SWAP,
            0b00000 => AmoFunction::ADD,
            0b00100 => AmoFunction::XOR,
            0b01100 => AmoFunction::AND,
            0b01000 => AmoFunction::OR,
            0b10000 => AmoFunction::MIN,
            0b10100 => AmoFunction::MAX,
            0b11100 => AmoFunction::MAXU,
            0b11000 => AmoFunction::MINU,
            _ => unimplemented!("AMO function {:#05b}", func5),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AmoOrdering {
    Ordered,
    Unordered,
}

impl AmoOrdering {
    fn from_bit(bit: u8) -> Self {
        match bit {
            0 => AmoOrdering::Unordered,
            1 => AmoOrdering::Ordered,
            _ => unimplemented!("AMO ordering {}", bit),
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
    REMU,
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

            _ => unimplemented!("Op func7={:07b} func3={:03b}", func7, func3),
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
            Self::REMU => 0b111,
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
            Self::REMU => 0b0000001,
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

    DIVW,
    DIVUW,
    MULW,
    REMW,
    REMUW,
}

impl Op32Function {
    pub fn from_func3_func7(func3: u8, func7: u8) -> Self {
        match (func7, func3) {
            (0b0000000, 0b000) => Op32Function::ADDW,
            (0b0100000, 0b000) => Op32Function::SUBW,
            (0b0000000, 0b001) => Op32Function::SLLW,
            (0b0000000, 0b101) => Op32Function::SRLW,
            (0b0100000, 0b101) => Op32Function::SRAW,

            (0b0000001, 0b000) => Op32Function::MULW,
            (0b0000001, 0b100) => Op32Function::DIVW,
            (0b0000001, 0b101) => Op32Function::DIVUW,
            (0b0000001, 0b110) => Op32Function::REMW,
            (0b0000001, 0b111) => Op32Function::REMUW,

            _ => unimplemented!("OP-32 with func7={:#07b}, func3={:#03b}", func7, func3),
        }
    }

    pub fn to_func3(&self) -> i32 {
        match self {
            Op32Function::ADDW => 0b000,
            Op32Function::SUBW => 0b000,
            Op32Function::SLLW => 0b001,
            Op32Function::SRLW => 0b101,
            Op32Function::SRAW => 0b101,

            Op32Function::MULW => 0b000,
            Op32Function::DIVW => 0b100,
            Op32Function::DIVUW => 0b101,
            Op32Function::REMW => 0b110,
            Op32Function::REMUW => 0b111,
        }
    }

    pub fn to_func7(&self) -> i32 {
        match self {
            Op32Function::ADDW => 0b0000000,
            Op32Function::SUBW => 0b0100000,
            Op32Function::SLLW => 0b0000000,
            Op32Function::SRLW => 0b0000000,
            Op32Function::SRAW => 0b0100000,

            Op32Function::MULW => 0b0000001,
            Op32Function::DIVW => 0b0000001,
            Op32Function::DIVUW => 0b0000001,
            Op32Function::REMW => 0b0000001,
            Op32Function::REMUW => 0b0000001,
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
            0b001 | 0b010 | 0b011 | 0b101 | 0b110 | 0b111 => {
                SystemFunction::CSR(CSRFunction::from_func3(func3))
            }

            _ => unimplemented!("system function {:#04x}", func3),
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

            _ => unimplemented!("environment function {:#012b}", imm11_0),
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
    CSRRCI,
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

            _ => unimplemented!("csr function {:#03b}", func3),
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
    GreaterOrEqualUnsigned,
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

            _ => unimplemented!(),
        }
    }

    pub fn to_func3(&self) -> i32 {
        match self {
            BranchOperation::Equal => 0b000,
            BranchOperation::NotEqual => 0b001,
            BranchOperation::LessThan => 0b100,
            BranchOperation::GreaterOrEqual => 0b101,
            BranchOperation::LessThanUnsigned => 0b110,
            BranchOperation::GreaterOrEqualUnsigned => 0b111,
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

            _ => unimplemented!("Load width {:03b}", func3),
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
            LoadWidth::WordUnsigned => 0b110,
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

            _ => unimplemented!("Store width {:03b}", func3),
        }
    }

    pub fn to_func3(&self) -> i32 {
        match self {
            Self::Byte => 0b000,
            Self::HalfWord => 0b001,
            Self::Word => 0b010,
            Self::DoubleWord => 0b011,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
    // Base instruction set
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
                let imm10_1 = ((encoded >> 21) & 0b11111111111) << 1;
                let imm11 = ((encoded >> 20) & 0b1) << 11;
                let imm19_12 = ((encoded >> 12) & 0b11111111) << 12;

                let sign_bit = imm20 != 0;
                let imm = (if sign_bit { (-1i32 as u32) << 21 } else { 0 })
                    | imm20
                    | imm10_1
                    | imm11
                    | imm19_12;

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
            } => {
                (encode_32bits(
                    (((imm >> 5) & 0b1111111) << 25)
                        | (rs2.encode() << 20)
                        | (rs1.encode() << 15)
                        | (opcode.funct3_field() << 12)
                        | ((imm & 0b11111) << 7)
                        | opcode.opcode_field(),
                ))
            }

            Instruction::B {
                imm,
                rs2,
                rs1,
                opcode,
            } => {
                (encode_32bits(
                    (((imm >> 20) & 0b1) << 31)
                        | (((imm >> 5) & 0b111111) << 25)
                        | (rs2.encode() << 20)
                        | (rs1.encode() << 15)
                        | (opcode.funct3_field() << 12)
                        | (((imm >> 1) & 0b1111) << 8)
                        | (((imm >> 11) & 0b1) << 7)
                        | opcode.opcode_field(),
                ))
            }

            Instruction::U { imm, rd, opcode } => {
                (encode_32bits(
                    rd.encode() << 7 | opcode.opcode_field() | (imm & 0b11111111111111111111),
                ))
            }

            Instruction::J { imm, rd, opcode } => {
                (encode_32bits(
                    (((imm >> 20) & 0b1) << 31)
                        | (((imm >> 1) & 0b1111111111) << 21)
                        | (((imm >> 11) & 0b1) << 20)
                        | (((imm >> 12) & 0b11111111) << 12)
                        | (rd.encode() << 7)
                        | opcode.opcode_field(),
                ))
            }

            Instruction::CIW { opcode, rd, imm } => {
                (encode_16bits(
                    ((opcode.funct3_field() as u16) << 13)
                        | (((*imm as u16 >> 2) & 0b11111111) << 5)
                        | ((rd.encode_prime() as u16) << 2)
                        | opcode.opcode_field() as u16,
                ))
            }

            Instruction::CNOP => encode_16bits(0x00000001),

            Instruction::CS {
                opcode,
                rs1,
                rs2,
                imm,
            } => {
                (encode_16bits(
                    ((opcode.funct3_field() as u16) << 13)
                        | ((*imm >> 3 & 0b111) << 10)
                        | (rs1.encode_prime() << 7)
                        | ((*imm >> 2 & 0b1) << 6)
                        | ((*imm >> 3 & 0b1) << 5)
                        | (rs2.encode_prime() << 2)
                        | (opcode.opcode_field() as u16),
                ))
            }

            Instruction::CL {
                opcode,
                rs1,
                rd,
                imm,
            } => {
                (encode_16bits(
                    ((opcode.funct3_field() as u16) << 13)
                        | ((*imm >> 3 & 0b111) << 10)
                        | (rs1.encode_prime() << 7)
                        | ((*imm >> 2 & 0b1) << 6)
                        | ((*imm >> 3 & 0b1) << 5)
                        | (rd.encode_prime() << 2)
                        | (opcode.opcode_field() as u16),
                ))
            }

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

#[derive(Debug)]
pub enum ReadError {
    InvalidInstruction,
}

pub struct DecodingStream<'a> {
    cursor: Cursor<&'a [u8]>,
}

impl<'a> DecodingStream<'a> {
    pub fn new(bytes: &'a [u8]) -> DecodingStream {
        DecodingStream {
            cursor: Cursor::new(bytes),
        }
    }
}

impl<'a> Iterator for DecodingStream<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let base: u16 = match self.cursor.read_u16::<LittleEndian>() {
            Ok(base) => base,
            Err(_) => {
                return None;
            }
        };

        if base == 0 {
            return None;
        }

        let ones = base.trailing_ones();

        match ones {
            0..=1 => Some(Instruction::from_16bits(base)),

            2..=4 => {
                let second: u32 = self.cursor.read_u16::<LittleEndian>().unwrap() as u32;
                let encoded: u32 = (second << 16) | (base as u32);

                Some(Instruction::from_32bits(encoded))
            }

            5 => {
                let parcels = [
                    base,
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                ];

                unimplemented!("48-bit instruction: {:?}", parcels);
            }

            6 => {
                let parcels = [
                    base,
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                ];

                unimplemented!("64-bit instruction: {:?}", parcels);
            }

            7..=11 => unimplemented!("Large instruction ({} trailing ones): {:#018b}", ones, base),
            _ => unimplemented!(
                "Invalid instruction ({} trailing ones): {:#018b}",
                ones,
                base
            ),
        }
    }
}

#[test]
pub fn test_stream_decoding_add() {
    let input = [
        183, 2, 1, 0, 147, 130, 2, 24, 147, 129, 2, 0, 19, 5, 0, 0, 147, 8, 96, 13, 115, 0, 0, 0,
        19, 5, 117, 0, 147, 2, 128, 0, 179, 114, 85, 2, 51, 5, 85, 64, 147, 8, 96, 13, 115, 0, 0,
        0, 35, 188, 161, 254, 19, 5, 0, 0, 147, 2, 129, 0, 19, 1, 129, 255, 35, 48, 81, 0, 239, 0,
        64, 15, 19, 1, 129, 255, 35, 48, 161, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 208, 5, 115,
        0, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129,
        0, 147, 8, 240, 3, 115, 0, 0, 0, 103, 128, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0,
        19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 0, 4, 115, 0, 0, 0, 103, 128, 0, 0, 131,
        54, 1, 0, 19, 1, 129, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 19, 5,
        192, 249, 147, 8, 128, 3, 115, 0, 0, 0, 103, 128, 0, 0, 131, 50, 1, 0, 19, 1, 129, 0, 147,
        130, 114, 0, 19, 3, 128, 0, 51, 243, 98, 2, 179, 130, 98, 64, 3, 179, 129, 255, 51, 5, 83,
        0, 147, 8, 96, 13, 115, 0, 0, 0, 99, 4, 101, 0, 99, 8, 0, 0, 99, 6, 80, 0, 19, 5, 0, 0, 99,
        6, 0, 0, 35, 188, 161, 254, 19, 5, 3, 0, 103, 128, 0, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3,
        53, 1, 0, 19, 1, 129, 0, 147, 8, 16, 25, 115, 0, 0, 0, 19, 5, 8, 0, 103, 128, 0, 0, 19, 1,
        129, 255, 35, 48, 17, 0, 19, 1, 129, 255, 35, 48, 129, 0, 19, 4, 1, 0, 147, 2, 112, 3, 19,
        3, 160, 2, 179, 130, 98, 0, 19, 133, 2, 0, 111, 0, 64, 0, 19, 1, 4, 0, 3, 52, 1, 0, 19, 1,
        129, 0, 131, 48, 1, 0, 19, 1, 129, 1, 103, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
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

    decode_test(
        &[183, 2, 1, 0],
        Instruction::U {
            opcode: Opcode::Lui,
            imm: 0x10000,
            rd: Register::T0,
        },
    );
    decode_test(
        &[147, 130, 2, 24],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::T0,
            rs1: Register::T0,
            imm: 384,
        },
    );
    decode_test(
        &[19, 5, 0, 0],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::A0,
            rs1: Register::Zero,
            imm: 0,
        },
    );
    decode_test(
        &[147, 8, 96, 13],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::A7,
            rs1: Register::Zero,
            imm: 214,
        },
    );
    decode_test(
        &[0x93, 0x87, 0xe0, 0xFC],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::A5,
            rs1: Register::ReturnAddress,
            imm: -50,
        },
    );
    decode_test(
        &[115, 0, 0, 0],
        Instruction::IS {
            opcode: Opcode::System(SystemFunction::Environment(EnvironmentFunction::ECALL)),
            rd: Register::Zero,
            rs1: Register::Zero,
            imm: 0,
        },
    );
    decode_test(
        &[239, 0, 64, 15],
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::ReturnAddress,
            imm: 244,
        },
    );
    decode_test(
        &[0x6f, 0x00, 0x80, 0x04],
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::Zero,
            imm: 72,
        },
    );
    decode_test(
        &[0x6f, 0x10, 0xf0, 0x67],
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::Zero,
            imm: 7806,
        },
    );
    decode_test(
        &[99, 4, 101, 0],
        Instruction::B { opcode: Opcode::Branch(BranchOperation::Equal), rs1: Register::A0, rs2: Register::T1, imm: /*2?*/ 8},
    );
    decode_test(
        &[0x83, 0x21, 0x72, 0x03],
        Instruction::I {
            opcode: Opcode::Load(LoadWidth::Word),
            imm: 55,
            rs1: Register::ThreadPointer,
            rd: Register::GlobalPointer,
        },
    );
    decode_test(
        &[35, 48, 81, 0],
        Instruction::S {
            opcode: Opcode::Store(StoreWidth::DoubleWord),
            rs2: Register::T0,
            rs1: Register::StackPointer,
            imm: 0,
        },
    );
    decode_test(
        &[179, 130, 98, 64],
        Instruction::R {
            opcode: Opcode::Op(OpFunction::SUB),
            rs1: Register::T0,
            rs2: Register::T1,
            rd: Register::T0,
        },
    );
}

pub struct EncodingStream {
    bytes: Vec<u8>,
}

impl EncodingStream {
    pub fn new() -> Self {
        Self { bytes: Vec::new() }
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
