# riscy  
![Build](https://github.com/michaelmelanson/riscy/workflows/Build/badge.svg)

A software implementation of a RISC-V computer.

## Usage

### Emulator

You can run the emulator using `cargo` and give it a file to run:

```
cargo run --bin riscy-emulator -- path/to/binary
```

The binary must be a RISC-V executable in ELF format. Binaries produced by [Selfie](https://github.com/cksystemsteaching/selfie) should work, as long as they are very simple. There is one subsystem supported which supplies a very limited set of POSIX-compliant system calls. Currently only a couple system calls are supported: 

|#   | System call | Current support |
|--|--|--|
|57  | close | No effect. Always returns success. |
|64  | write | Prints the buffer as a string to standard output (regardless of which file descriptor is given). |
|80  | fstat | If file descriptor 1 (standard output) is requested, then it returns a refcount of 0. Other descriptors are not supported. |
|93  | exit  | Terminates with a status code. |
|214 | program break | No effect. |

Any other system calls will fail with an "unknown system call" error.

The emulator will only print out a couple lines by default, but you can turn on more verbose logging with the `-d` flag:

```
cargo run --bin riscy-emulator -- -d debug path/to/binary
```



## Status

### RISC-V compliance

_tl;dr_ it might run your program, if you're super generous with your definition of "run".

This repository contains binary versions of the `riscv-tests` test suite from
https://github.com/riscv/riscv-tests and uses these to test compliance with the 
RISC-V standard:

| Instruction Set | Test suites passing | Notes |
|-|-|-|
| RV64I Base instruction set            | **100%** 🎉 | |
| "M" Integer multiplication & division | 12 / 13 test suites (**92%**)  | Exception is MULHSU which is a pain in the ass |
| "A" Atomic operations                 | 18 / 19 test suites (**94%**) | Load-Reserved and Store-Condition are not implemented |
| "Zicsr" Control & Status Register     | 2 / 11 test suites (**18%**)| Some CSRs work |
| "C" Compressed instructions           | **100%** 🎉 | Not including re-encoding |

### Selfie compatibility

_tl;dr_ super rudimentary

The emulator can execute simple programs compiled with [Selfie](https://github.com/cksystemsteaching/selfie). Notably it does 
not yet support output, so you're limited to using the status code to output data.
