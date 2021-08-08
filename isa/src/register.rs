#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Register {
    Zero,
    ReturnAddress,
    StackPointer,
    GlobalPointer,
    ThreadPointer,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl Register {
    // used in compressed data formats
    pub fn from_rd_prime(value: u8) -> Register {
        Register::from_u8(value + 8)
    }

    pub fn from_u8(value: u8) -> Register {
        match value {
            0 => Register::Zero,

            1 => Register::ReturnAddress,
            2 => Register::StackPointer,
            3 => Register::GlobalPointer,
            4 => Register::ThreadPointer,

            5 => Register::T0,
            6 => Register::T1,
            7 => Register::T2,

            8 => Register::S0,
            9 => Register::S1,

            10 => Register::A0,
            11 => Register::A1,
            12 => Register::A2,
            13 => Register::A3,
            14 => Register::A4,
            15 => Register::A5,
            16 => Register::A6,
            17 => Register::A7,

            18 => Register::S2,
            19 => Register::S3,
            20 => Register::S4,
            21 => Register::S5,
            22 => Register::S6,
            23 => Register::S7,
            24 => Register::S8,
            25 => Register::S9,
            26 => Register::S10,
            27 => Register::S11,

            28 => Register::T3,
            29 => Register::T4,
            30 => Register::T5,
            31 => Register::T6,

            _ => unimplemented!("register {}", value),
        }
    }

    pub fn encode_prime(&self) -> u16 {
        self.encode() as u16 - 8
    }

    pub fn encode(&self) -> i32 {
        match self {
            Register::Zero => 0,

            Register::ReturnAddress => 1,
            Register::StackPointer => 2,
            Register::GlobalPointer => 3,
            Register::ThreadPointer => 4,

            Register::T0 => 5,
            Register::T1 => 6,
            Register::T2 => 7,

            Register::S0 => 8,
            Register::S1 => 9,

            Register::A0 => 10,
            Register::A1 => 11,
            Register::A2 => 12,
            Register::A3 => 13,
            Register::A4 => 14,
            Register::A5 => 15,
            Register::A6 => 16,
            Register::A7 => 17,

            Register::S2 => 18,
            Register::S3 => 19,
            Register::S4 => 20,
            Register::S5 => 21,
            Register::S6 => 22,
            Register::S7 => 23,
            Register::S8 => 24,
            Register::S9 => 25,
            Register::S10 => 26,
            Register::S11 => 27,

            Register::T3 => 28,
            Register::T4 => 29,
            Register::T5 => 30,
            Register::T6 => 32,
        }
    }
}
