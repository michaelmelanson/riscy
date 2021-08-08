use crate::Instruction;

/// Encodes a sequence of RISC-V [instructions](Instruction), turning them into bytes.
///
/// # Example
/// To use this you'll want to create a stream using `EncodingStream::new()`,
/// then push each instruction in sequence using `push()`. When you're done, you
/// can get the encoded bytes using `bytes()`.
///
/// ```
/// use riscy_isa::{Instruction, Opcode, OpImmFunction, EncodingStream, Register};
/// let mut stream = EncodingStream::new();
/// stream.push(&Instruction::I {
///     opcode: Opcode::OpImm(OpImmFunction::ADDI),
///     rd: Register::A0,
///     rs1: Register::Zero,
///     imm: 0,
/// });
/// assert_eq!(stream.bytes(), &[19, 5, 0, 0]);
/// ```
pub struct EncodingStream {
    bytes: Vec<u8>,
}

impl EncodingStream {
    pub fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    pub fn push(&mut self, instruction: &Instruction) {
        self.bytes.append(&mut instruction.bytes());
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}
