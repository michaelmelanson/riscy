# riscy  
![Build](https://github.com/michaelmelanson/riscy/workflows/Build/badge.svg)

A software implementation of a RISC-V computer.

## Status

### RISC-V compliance

_tl;dr_ it only wishes it were RISC-V compliant.

This repository contains the `riscv-tests` compliance test suite from https://github.com/riscv/riscv-tests and passes a subset
of them. See [emulator/tests/test_rv64ui-p.rs](https://github.com/michaelmelanson/riscy/blob/master/emulator/tests/test_rv64ui-p.rs#L30)
for details about which ones are currently supported.

| Instruction Set | Test suites passing | Notes |
|-|-|-|
| RV64I Base instruction set              | 30 / 50 test suites  **60%** | |
| RV64M Integer multiplication & division | N/A  | Partial untested support |
| Zicsr Control & Status Register         | N/A  | Minimal support for MRET |

### Selfie compatibility

_tl;dr_ super rudimentary

The emulator can execute simple programs compiled with [Selfie](https://github.com/cksystemsteaching/selfie). Notably it does 
not yet support output, so you're limited to using the status code to output data.
