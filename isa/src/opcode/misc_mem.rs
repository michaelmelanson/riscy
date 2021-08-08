#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MiscMemFunction {
    FENCE,
    FENCEI,
}

impl MiscMemFunction {
    pub fn from_func3(func3: u8) -> Self {
        match func3 {
            0b000 => MiscMemFunction::FENCE,
            0b001 => MiscMemFunction::FENCEI,

            _ => unimplemented!("MISC-MEM for func3={:#03b}", func3),
        }
    }

    pub fn to_func3(&self) -> i32 {
        match self {
            MiscMemFunction::FENCE => 0b000,
            MiscMemFunction::FENCEI => 0b001,
        }
    }
}
