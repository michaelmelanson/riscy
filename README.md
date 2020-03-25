# riscy  
![Build](https://github.com/michaelmelanson/riscy/workflows/Build/badge.svg)

A software implementation of a RISC-V computer.

## Usage

### Emulator

You can run the emulator using `cargo` and give it a file to run:

```
cargo run -p emulator -- path/to/binary
```

The binary must be a RISC-V executable in ELF format. Binaries produced by [Selfie](https://github.com/cksystemsteaching/selfie) should work, as long as they are very simple. Only a couple system calls are supported: "break" (which doesn't actually have an effect) and "exit" (which terminates with a status code).

The emulator will only print out a couple lines by default, but you can turn on more verbose logging with the `-d` flag:

```
cargo run -p emulator -- -d debug path/to/binary
```



## Status

### RISC-V compliance

_tl;dr_ it supports the basic instruction set, but it's hopelessly inadequate in every other way.

This repository contains binary versions of the `riscv-tests` test suite from
https://github.com/riscv/riscv-tests and uses these to test compliance with the 
RISC-V standard:

| Instruction Set | Test suites passing | Notes |
|-|-|-|
| RV64I Base instruction set              | **100%** 🎉 | |
| RV64M Integer multiplication & division | N/A  | Partial untested support |
| Zicsr Control & Status Register         | N/A  | Minimal support for MRET instruction |

### Selfie compatibility

_tl;dr_ super rudimentary

The emulator can execute simple programs compiled with [Selfie](https://github.com/cksystemsteaching/selfie). Notably it does 
not yet support output, so you're limited to using the status code to output data.
