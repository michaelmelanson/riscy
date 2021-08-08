mod amo_function;
mod amo_ordering;
mod amo_width;
mod branch_operation;
mod load_width;
mod misc_mem;
mod op_32_function;
mod op_function;
mod op_imm_32_function;
mod op_imm_function;
mod store_width;
mod system_function;

pub use self::{
    amo_function::AmoFunction,
    amo_ordering::AmoOrdering,
    amo_width::AmoWidth,
    branch_operation::BranchOperation,
    load_width::LoadWidth,
    misc_mem::MiscMemFunction,
    op_32_function::Op32Function,
    op_function::OpFunction,
    op_imm_32_function::OpImm32Function,
    op_imm_function::OpImmFunction,
    store_width::StoreWidth,
    system_function::{CSRFunction, EnvironmentFunction, SystemFunction},
};

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    Load(LoadWidth),
    LoadFp,
    Custom0,
    MiscMem(MiscMemFunction),
    OpImm(OpImmFunction),
    AuiPc,
    OpImm32(OpImm32Function),

    Store(StoreWidth),
    StoreFp,
    Custom1,
    Amo(AmoFunction, AmoWidth),
    Op(OpFunction),
    Lui,
    Op32(Op32Function),

    MAdd,
    MSub,
    NMSub,
    NMAdd,
    OpFp,
    Reserved0,
    Custom2,

    Branch(BranchOperation),
    JAlr,
    Reserved1,
    JAl,
    System(SystemFunction),
    Reserved2,
    Custom3,

    // "C" (Compressed) extension
    CADDI4SPN,
    CADDI,
    CNOP,
    CFLD,
    CLW,
    CLD,
    CSW,
    CSD,
    CADDIW,
    CLI,
    CADDI16SP,
    CLUI,
    CSRLI,
    CSRAI,
    CANDI,
    CSUB,
    CXOR,
    COR,
    CAND,
    CSUBW,
    CADDW,
    CJ,
    CBEQZ,
    CBNEZ,
    CSLLI,
    CLWSP,
    CLDSP,
    CJR,
    CMV,
    CBREAK,
    CJALR,
    CADD,
    CSWSP,
    CSDSP,
}

impl Opcode {
    pub fn new_compressed(base: u16) -> Opcode {
        let op_low = base & 0b11;
        let op_high = (base >> 13) & 0b111;

        match (op_high, op_low) {
            (0b000, 0b00) => Opcode::CADDI4SPN,
            (0b000, 0b01) => {
                if (base >> 7) & 0b11111 == 0 {
                    Opcode::CNOP
                } else {
                    Opcode::CADDI
                }
            }
            (0b001, 0b00) => Opcode::CFLD, // or is it C.LQ?
            (0b010, 0b00) => Opcode::CLW,
            (0b011, 0b00) => Opcode::CLD,
            (0b110, 0b00) => Opcode::CSW,
            (0b111, 0b00) => Opcode::CSD,
            (0b001, 0b01) => Opcode::CADDIW,
            (0b010, 0b01) => Opcode::CLI,
            (0b011, 0b01) => {
                if (base >> 7) & 0b11111 == 2 {
                    Opcode::CADDI16SP
                } else {
                    Opcode::CLUI
                }
            }
            (0b100, 0b01) => {
                let func3 = (base >> 10) & 0b111;
                let func2 = (base >> 5) & 0b11;
                match (func3, func2) {
                    (0b000, _) | (0b100, _) => Opcode::CSRLI,
                    (0b001, _) | (0b101, _) => Opcode::CSRAI,
                    (0b010, _) | (0b110, _) => Opcode::CANDI,
                    (0b011, 0b00) => Opcode::CSUB,
                    (0b011, 0b01) => Opcode::CXOR,
                    (0b011, 0b10) => Opcode::COR,
                    (0b011, 0b11) => Opcode::CAND,
                    (0b111, 0b00) => Opcode::CSUBW,
                    (0b111, 0b01) => Opcode::CADDW,
                    _ => todo!(
                        "compressed arithmetic function with func=({:#03b}, {:#02b})",
                        func3,
                        func2
                    ),
                }
            }
            (0b101, 0b01) => Opcode::CJ,
            (0b110, 0b01) => Opcode::CBEQZ,
            (0b111, 0b01) => Opcode::CBNEZ,
            (0b000, 0b10) => Opcode::CSLLI,
            (0b010, 0b10) => Opcode::CLWSP,
            (0b011, 0b10) => Opcode::CLDSP,
            (0b100, 0b10) => {
                let func12 = (base >> 12) & 0b1;
                let func11_7 = (base >> 7) & 0b11111;
                let func6_2 = (base >> 2) & 0b11111;

                match (func12, func11_7, func6_2) {
                    (0b0, _, 0b00000) => Opcode::CJR,
                    (0b0, _, _) => Opcode::CMV,
                    (0b1, 0b0, 0b00000) => unimplemented!("C.EBREAK"),
                    (0b1, _, 0b00000) => Opcode::CJALR,
                    (0b1, _, _) => Opcode::CADD,
                    spec => unimplemented!("compressed opcode 100...10 with func ({:?})", spec),
                }
            }
            (0b110, 0b10) => Opcode::CSWSP,
            (0b111, 0b10) => Opcode::CSDSP,

            _ => todo!(
                "compressed instruction with op={:03b}...{:02b} from base={:016b} ({:#04x})",
                op_high,
                op_low,
                base,
                base
            ),
        }
    }

    pub fn new(base: u16, func3: u8, imm11_0: u16, func5: u8, func7: u8) -> Opcode {
        let opcode = base & 0b1111111;
        match opcode {
            0b0000011 => Opcode::Load(LoadWidth::from_func3(func3)),
            0b0000111 => Opcode::LoadFp,
            0b0001011 => unimplemented!("Custom 0"),
            0b0001111 => Opcode::MiscMem(MiscMemFunction::from_func3(func3)),
            0b0010011 => Opcode::OpImm(OpImmFunction::from_imm11_0_func3(imm11_0, func3)),
            0b0010111 => Opcode::AuiPc,
            0b0011011 => Opcode::OpImm32(OpImm32Function::from_func3_imm(func3, imm11_0)),

            0b0100011 => Opcode::Store(StoreWidth::from_func3(func3)),
            0b0100111 => Opcode::StoreFp,
            0b0101011 => unimplemented!("Custom 1"),
            0b0101111 => Opcode::Amo(AmoFunction::from_func5(func5), AmoWidth::from_func3(func3)),
            0b0110011 => Opcode::Op(OpFunction::from_func3_func7(func3, func7)),
            0b0110111 => Opcode::Lui,
            0b0111011 => Opcode::Op32(Op32Function::from_func3_func7(func3, func7)),

            0b1000011 => Opcode::MAdd,
            0b1000111 => Opcode::MSub,
            0b1001011 => Opcode::NMSub,
            0b1001111 => Opcode::NMAdd,
            0b1010011 => Opcode::OpFp,
            0b1010111 => unimplemented!("Reserved 0"),
            0b1011011 => unimplemented!("Custom 2"),

            0b1100011 => Opcode::Branch(BranchOperation::from_func3(func3)),
            0b1100111 => Opcode::JAlr,
            0b1101011 => unimplemented!("Reserved 1"),
            0b1101111 => Opcode::JAl,
            0b1110011 => Opcode::System(SystemFunction::from_func3_imm11_0(func3, imm11_0)),
            0b1110111 => unimplemented!("Reserved 2"),
            0b1111011 => unimplemented!("Custom 3"),

            _ => panic!("Unknown opcode"),
        }
    }

    pub fn opcode_field(&self) -> i32 {
        match self {
            Opcode::Load(_) => 0b0000011,
            Opcode::LoadFp => 0b0000111,
            Opcode::Custom0 => 0b0001011,
            Opcode::MiscMem(_) => 0b0001111,
            Opcode::OpImm(_) => 0b0010011,
            Opcode::AuiPc => 0b0010111,
            Opcode::OpImm32(_) => 0b0011011,

            Opcode::Store(_) => 0b0100011,
            Opcode::StoreFp => 0b0100111,
            Opcode::Custom1 => 0b0101011,
            Opcode::Amo(_, _) => 0b0101111,
            Opcode::Op(_) => 0b0110011,
            Opcode::Lui => 0b0110111,
            Opcode::Op32(_) => 0b0111011,

            Opcode::MAdd => 0b1000011,
            Opcode::MSub => 0b1000111,
            Opcode::NMSub => 0b1001011,
            Opcode::NMAdd => 0b1001111,
            Opcode::OpFp => 0b1010011,
            Opcode::Reserved0 => 0b1010111,
            Opcode::Custom2 => 0b1011011,

            Opcode::Branch(_) => 0b1100011,
            Opcode::JAlr => 0b1100111,
            Opcode::Reserved1 => 0b1101011,
            Opcode::JAl => 0b1101111,
            Opcode::System(_) => 0b1110011,
            Opcode::Reserved2 => 0b1110111,
            Opcode::Custom3 => 0b1111011,

            Opcode::CADDI4SPN => 0b00,
            Opcode::CADDI => 0b01,
            Opcode::CNOP => 0b01,
            Opcode::CFLD => 0b00,
            Opcode::CLW => 0b00,
            Opcode::CLD => 0b00,
            Opcode::CSW => 0b00,
            Opcode::CSD => 0b00,
            Opcode::CADDIW => 0b01,
            Opcode::CLI => 0b01,
            Opcode::CADDI16SP => 0b01,
            Opcode::CLUI => 0b01,
            Opcode::CSRLI => 0b01,
            Opcode::CSRAI => 0b01,
            Opcode::CANDI => 0b01,
            Opcode::CSUB => 0b01,
            Opcode::CXOR => 0b01,
            Opcode::COR => 0b01,
            Opcode::CAND => 0b01,
            Opcode::CSUBW => 0b01,
            Opcode::CADDW => 0b01,
            Opcode::CJ => 0b01,
            Opcode::CBEQZ => 0b01,
            Opcode::CBNEZ => 0b01,
            Opcode::CSLLI => 0b10,
            Opcode::CLWSP => 0b10,
            Opcode::CLDSP => 0b10,
            Opcode::CJR => 0b10,
            Opcode::CMV => 0b10,
            Opcode::CBREAK => 0b10,
            Opcode::CJALR => 0b10,
            Opcode::CADD => 0b10,
            Opcode::CSWSP => 0b10,
            Opcode::CSDSP => 0b10,
        }
    }

    pub fn funct3_field(&self) -> i32 {
        match self {
            Opcode::Load(function) => function.to_func3(),
            Opcode::OpImm(function) => function.to_func3(),
            Opcode::Store(function) => function.to_func3(),
            Opcode::Op(function) => function.to_func3(),
            Opcode::Branch(function) => function.to_func3(),
            Opcode::System(_) => 0,

            _ => unimplemented!("opcode {:?} does not have funct3 field", self),
        }
    }

    pub fn funct7_field(&self) -> i32 {
        match self {
            Opcode::Op(function) => function.to_func7(),

            _ => unimplemented!("opcode {:?} does not have funct7 field", self),
        }
    }
}
