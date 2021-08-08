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
