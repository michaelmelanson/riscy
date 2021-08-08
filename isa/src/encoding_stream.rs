use crate::Instruction;

pub struct EncodingStream {
    bytes: Vec<u8>,
}

impl EncodingStream {
    pub fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.bytes.append(&mut instruction.bytes());
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}
