#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AmoOrdering {
    Ordered,
    Unordered,
}

impl AmoOrdering {
    pub fn from_bit(bit: u8) -> Self {
        match bit {
            0 => AmoOrdering::Unordered,
            1 => AmoOrdering::Ordered,
            _ => unimplemented!("AMO ordering {}", bit),
        }
    }
}
