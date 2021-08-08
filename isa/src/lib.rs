mod decoding_stream;
mod encoding_stream;
mod fp_register;
mod instruction;
mod opcode;
mod register;

pub use self::{
    decoding_stream::DecodingStream, encoding_stream::EncodingStream, instruction::Instruction,
    opcode::*, register::Register,
};

#[derive(Debug)]
pub enum ReadError {
    InvalidInstruction,
}

#[test]
pub fn test_stream_decoding_add() {
    let input = [
        183, 2, 1, 0, 147, 130, 2, 24, 147, 129, 2, 0, 19, 5, 0, 0, 147, 8, 96, 13, 115, 0, 0, 0,
        19, 5, 117, 0, 147, 2, 128, 0, 179, 114, 85, 2, 51, 5, 85, 64, 147, 8, 96, 13, 115, 0, 0,
        0, 35, 188, 161, 254, 19, 5, 0, 0, 147, 2, 129, 0, 19, 1, 129, 255, 35, 48, 81, 0, 239, 0,
        64, 15, 19, 1, 129, 255, 35, 48, 161, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 208, 5, 115,
        0, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129,
        0, 147, 8, 240, 3, 115, 0, 0, 0, 103, 128, 0, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0,
        19, 1, 129, 0, 3, 53, 1, 0, 19, 1, 129, 0, 147, 8, 0, 4, 115, 0, 0, 0, 103, 128, 0, 0, 131,
        54, 1, 0, 19, 1, 129, 0, 3, 54, 1, 0, 19, 1, 129, 0, 131, 53, 1, 0, 19, 1, 129, 0, 19, 5,
        192, 249, 147, 8, 128, 3, 115, 0, 0, 0, 103, 128, 0, 0, 131, 50, 1, 0, 19, 1, 129, 0, 147,
        130, 114, 0, 19, 3, 128, 0, 51, 243, 98, 2, 179, 130, 98, 64, 3, 179, 129, 255, 51, 5, 83,
        0, 147, 8, 96, 13, 115, 0, 0, 0, 99, 4, 101, 0, 99, 8, 0, 0, 99, 6, 80, 0, 19, 5, 0, 0, 99,
        6, 0, 0, 35, 188, 161, 254, 19, 5, 3, 0, 103, 128, 0, 0, 131, 53, 1, 0, 19, 1, 129, 0, 3,
        53, 1, 0, 19, 1, 129, 0, 147, 8, 16, 25, 115, 0, 0, 0, 19, 5, 8, 0, 103, 128, 0, 0, 19, 1,
        129, 255, 35, 48, 17, 0, 19, 1, 129, 255, 35, 48, 129, 0, 19, 4, 1, 0, 147, 2, 112, 3, 19,
        3, 160, 2, 179, 130, 98, 0, 19, 133, 2, 0, 111, 0, 64, 0, 19, 1, 4, 0, 3, 52, 1, 0, 19, 1,
        129, 0, 131, 48, 1, 0, 19, 1, 129, 1, 103, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    let stream = DecodingStream::new(&input);

    let instructions = stream.collect::<Vec<Instruction>>();

    assert_eq!(instructions.len(), 94);
}

#[test]
pub fn test_instruction_encoding() {
    fn encoding_test(bytes: &[u8], instruction: Instruction) {
        let mut stream = DecodingStream::new(bytes);
        let actual = stream.next().unwrap();
        assert_eq!(actual, instruction);

        let mut stream = EncodingStream::new();
        stream.push(&instruction);
        let actual = stream.bytes();
        assert_eq!(actual, bytes);
    }

    /*
      183, 2, 1, 0,     // lui t0,0x10
      147, 130, 2, 24,  // addi t0,t0,384
      147, 129, 2, 0,   // addi gp,t0,0
      19, 5, 0, 0,      // addi a0,zero,0
      147, 8, 96, 13,   // addi a7,zero,214
      115, 0, 0, 0,     // ecall
      19, 5, 117, 0,    // addi a0,a0,7
      147, 2, 128, 0,   // addi t0,zero,8
      179, 114, 85, 2,  // remu t0,a0,t0
      51, 5, 85, 64,    // sub a0,a0,t0
      147, 8, 96, 13,   // addi a7,zero,214
      115, 0, 0, 0,     // ecall
      35, 188, 161, 254,// sd a0,-8(gp)
      19, 5, 0, 0,      // addi a0,zero,0
      147, 2, 129, 0,   // addi t0,sp,8
      19, 1, 129, 255,  // addi sp,sp,-8
      35, 48, 81, 0,    // sd t0,0(sp)
      239, 0, 64, 15,   // jal ra,61
      19, 1, 129, 255,  // addi sp,sp,-8
      35, 48, 161, 0,   // sd a0,0(sp)
      3, 53, 1, 0,      // ld a0,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      147, 8, 208, 5,   // addi a7,zero,93
      115, 0, 0, 0,     // ecall
      3, 54, 1, 0,      // ld a2,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      131, 53, 1, 0,    // ld a1,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      3, 53, 1, 0,      // ld a0,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      147, 8, 240, 3,   // addi a7,zero,63
      115, 0, 0, 0,     // ecall
      103, 128, 0, 0,   // jalr zero,0(ra)
      3, 54, 1, 0,      // ld a2,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      131, 53, 1, 0,    // ld a1,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      3, 53, 1, 0,      // ld a0,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      147, 8, 0, 4,     // addi a7,zero,64
      115, 0, 0, 0,     // ecall
      103, 128, 0, 0,   // jalr zero,0(ra)
      131, 54, 1, 0,    // ld a3,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      3, 54, 1, 0,      // ld a2,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      131, 53, 1, 0,    // ld a1,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      19, 5, 192, 249,  // addi a0,zero,-100
      147, 8, 128, 3,   // addi a7,zero,56
      115, 0, 0, 0,     // ecall
      103, 128, 0, 0,   // jalr zero,0(ra)
      131, 50, 1, 0,    // ld t0,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      147, 130, 114, 0, // addi t0,t0,7
      19, 3, 128, 0,    // addi t1,zero,8
      51, 243, 98, 2,   // remu t1,t0,t1
      179, 130, 98, 64, // sub t0,t0,t1
      3, 179, 129, 255, // ld t1,-8(gp)
      51, 5, 83, 0,     // add a0,t1,t0
      147, 8, 96, 13,   // addi a7,zero,214
      115, 0, 0, 0,     // ecall
      99, 4, 101, 0,    // beq a0,t1,2
      99, 8, 0, 0,      // beq zero,zero,4
      99, 6, 80, 0,     // beq zero,t0,3
      19, 5, 0, 0,      // addi a0,zero,0
      99, 6, 0, 0,      // beq zero,zero,3
      35, 188, 161, 254,// sd a0,-8(gp)
      19, 5, 3, 0,      // addi a0,t1,0
      103, 128, 0, 0,   // jalr zero,0(ra)
      131, 53, 1, 0,    // ld a1,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      3, 53, 1, 0,      // ld a0,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      147, 8, 16, 25,   // addi a7,zero,401
      115, 0, 0, 0,     // ecall
      19, 5, 8, 0,      // addi a0,a6,0
      103, 128, 0, 0,   // jalr zero,0(ra)
      19, 1, 129, 255,  // addi sp,sp,-8
      35, 48, 17, 0,    // sd ra,0(sp)
      19, 1, 129, 255,  // addi sp,sp,-8
      35, 48, 129, 0,   // sd s0,0(sp)
      19, 4, 1, 0,      // addi s0,sp,0
      147, 2, 112, 3,   // addi t0,zero,55
      19, 3, 160, 2,    // addi t1,zero,42
      179, 130, 98, 0,  // add t0,t0,t1
      19, 133, 2, 0,    // addi a0,t0,0
      111, 0, 64, 0,    // jal zero,1
      19, 1, 4, 0,      // addi sp,s0,0
      3, 52, 1, 0,      // ld s0,0(sp)
      19, 1, 129, 0,    // addi sp,sp,8
      131, 48, 1, 0,    // ld ra,0(sp)
      19, 1, 129, 1,    // addi sp,sp,24
      103, 128, 0, 0,   // jalr zero,0(ra)
    */

    encoding_test(
        &[183, 2, 1, 0],
        Instruction::U {
            opcode: Opcode::Lui,
            imm: 0x10000,
            rd: Register::T0,
        },
    );
    encoding_test(
        &[147, 130, 2, 24],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::T0,
            rs1: Register::T0,
            imm: 384,
        },
    );
    encoding_test(
        &[19, 5, 0, 0],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::A0,
            rs1: Register::Zero,
            imm: 0,
        },
    );
    encoding_test(
        &[147, 8, 96, 13],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::A7,
            rs1: Register::Zero,
            imm: 214,
        },
    );
    encoding_test(
        &[0x93, 0x87, 0xe0, 0xFC],
        Instruction::I {
            opcode: Opcode::OpImm(OpImmFunction::ADDI),
            rd: Register::A5,
            rs1: Register::ReturnAddress,
            imm: -50,
        },
    );
    encoding_test(
        &[115, 0, 0, 0],
        Instruction::IS {
            opcode: Opcode::System(opcode::SystemFunction::Environment(
                opcode::EnvironmentFunction::ECALL,
            )),
            rd: Register::Zero,
            rs1: Register::Zero,
            imm: 0,
        },
    );
    encoding_test(
        &[239, 0, 64, 15],
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::ReturnAddress,
            imm: 244,
        },
    );
    encoding_test(
        &[0x6f, 0x00, 0x80, 0x04],
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::Zero,
            imm: 72,
        },
    );
    encoding_test(
        &[0x6f, 0x10, 0xf0, 0x67],
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::Zero,
            imm: 7806,
        },
    );
    encoding_test(
        &0x946ff0efu32.to_le_bytes(),
        Instruction::J {
            opcode: Opcode::JAl,
            rd: Register::ReturnAddress,
            imm: -0xeba, // 0xeba,
        },
    );

    encoding_test(
        &[99, 4, 101, 0],
        Instruction::B { opcode: Opcode::Branch(BranchOperation::Equal), rs1: Register::A0, rs2: Register::T1, imm: /*2?*/ 8},
    );
    encoding_test(
        &[0x83, 0x21, 0x72, 0x03],
        Instruction::I {
            opcode: Opcode::Load(LoadWidth::Word),
            imm: 55,
            rs1: Register::ThreadPointer,
            rd: Register::GlobalPointer,
        },
    );
    encoding_test(
        &[35, 48, 81, 0],
        Instruction::S {
            opcode: Opcode::Store(StoreWidth::DoubleWord),
            rs2: Register::T0,
            rs1: Register::StackPointer,
            imm: 0,
        },
    );
    encoding_test(
        &[179, 130, 98, 64],
        Instruction::R {
            opcode: Opcode::Op(OpFunction::SUB),
            rs1: Register::T0,
            rs2: Register::T1,
            rd: Register::T0,
        },
    );
}

#[test]
pub fn test_instruction_roundtrip() {
    fn roundtrip_test(input: &[u8]) {
        let mut stream = DecodingStream::new(input);
        let instruction = stream.next().unwrap();

        let mut stream = EncodingStream::new();
        stream.push(&instruction);

        let output = stream.bytes();
        assert_eq!(output, input);
    }

    roundtrip_test(&[183, 2, 1, 0]);
    roundtrip_test(&[147, 130, 2, 24]);
    roundtrip_test(&[19, 5, 0, 0]);
    roundtrip_test(&[147, 8, 96, 13]);
    roundtrip_test(&[0x93, 0x87, 0xe0, 0xFC]);
    roundtrip_test(&[115, 0, 0, 0]);
    roundtrip_test(&[239, 0, 64, 15]);
    roundtrip_test(&[99, 4, 101, 0]);
    roundtrip_test(&[0x83, 0x21, 0x72, 0x03]);
    roundtrip_test(&[35, 48, 81, 0]);
    roundtrip_test(&[179, 130, 98, 64]);
}
