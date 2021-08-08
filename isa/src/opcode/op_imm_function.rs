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
