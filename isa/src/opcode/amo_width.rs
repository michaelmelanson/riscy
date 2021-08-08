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
