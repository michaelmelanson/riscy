#[derive(Copy, Clone, Debug, PartialEq)]
pub enum EnvironmentFunction {
    ECALL,
    EBREAK,

    URET,
    SRET,
    MRET,
}

impl EnvironmentFunction {
    pub fn from_imm11_0(imm11_0: u16) -> Self {
        match imm11_0 {
            0b000000000000 => EnvironmentFunction::ECALL,
            0b000000000001 => EnvironmentFunction::EBREAK,

            0b000000000010 => EnvironmentFunction::URET,
            0b000100000010 => EnvironmentFunction::SRET,
            0b001100000010 => EnvironmentFunction::MRET,

            _ => unimplemented!("environment function {:#012b}", imm11_0),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CSRFunction {
    CSRRW,
    CSRRS,
    CSRRC,
    CSRRWI,
    CSRRSI,
    CSRRCI,
}

impl CSRFunction {
    pub fn from_func3(func3: u8) -> Self {
        match func3 {
            0b001 => CSRFunction::CSRRW,
            0b010 => CSRFunction::CSRRS,
            0b011 => CSRFunction::CSRRC,
            0b101 => CSRFunction::CSRRWI,
            0b110 => CSRFunction::CSRRSI,
            0b111 => CSRFunction::CSRRCI,

            _ => unimplemented!("csr function {:#03b}", func3),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SystemFunction {
    // base
    Environment(EnvironmentFunction),

    //Zicsr
    CSR(CSRFunction),
}

impl SystemFunction {
    pub fn from_func3_imm11_0(func3: u8, imm11_0: u16) -> Self {
        match func3 {
            0 => SystemFunction::Environment(EnvironmentFunction::from_imm11_0(imm11_0)),
            0b001 | 0b010 | 0b011 | 0b101 | 0b110 | 0b111 => {
                SystemFunction::CSR(CSRFunction::from_func3(func3))
            }

            _ => unimplemented!("system function {:#04x}", func3),
        }
    }
}
