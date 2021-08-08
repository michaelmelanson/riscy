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
