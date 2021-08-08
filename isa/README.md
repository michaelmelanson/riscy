# RISC-V instruction encoding and decoding.

This crate allows you to encode and decode streams of RISC-V instructions to and from Rust structs.

To get the instruction streams out of an ELF binary, which is where you'll normally find them, I recommend the [`goblin`](https://crates.io/crates/goblin) crate.

# Example
```rust
use riscy_isa::{Opcode, DecodingStream, Instruction, Register, OpImmFunction};
let bytes: [u8; 4] = [19, 5, 0, 0];
let mut stream = DecodingStream::new(&bytes);
// Decodes to an `addi a0, x0, 0` instruction
assert_eq!(stream.next(), Some(Instruction::I {
    opcode: Opcode::OpImm(OpImmFunction::ADDI),
    rd: Register::A0,
    rs1: Register::Zero,
    imm: 0,
}));
// There's only one instruction in the byte array so any further calls to
// `next` return `None`.
assert_eq!(stream.next(), None);
```

# Compatibility

This crate partially or fully supports the following RISC-V extensions:

* RV64I Base instruction set.
* "M" Integer multiplication & division (exception: `MULHSU` is not implemented)
* "A" Atomic operations (exception: Load-Reserved and Store-Condition are not implemented)
* "Zicsr" Control & Status Register (partial)
* "C" Compressed instructions (exception: instruction encoding is not implemented)
