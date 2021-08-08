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
