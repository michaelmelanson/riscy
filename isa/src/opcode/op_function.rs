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
