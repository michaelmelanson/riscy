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
