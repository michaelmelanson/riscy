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
