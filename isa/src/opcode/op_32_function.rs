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
