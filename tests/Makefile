
all: add.s add.m fib.s fib.m print.s print.m
clean:
	rm selfie selfie.c
	rm add.s add.m fib.s fib.m
.PHONY: all clean

selfie.c: 
	curl https://raw.githubusercontent.com/cksystemsteaching/selfie/master/selfie.c > selfie.c

selfie: selfie.c
	cc -Wall -Wextra -O3 -m64 -Duint64_t='unsigned long long' selfie.c -o selfie

%.m: %.c link.ld defines.h entry.S Makefile
	/opt/riscv/bin/riscv64-unknown-elf-gcc -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -std=gnu99 -O2 -T link.ld entry.S $< -o $@

%.s: %.m
	/opt/riscv/bin/riscv64-unknown-elf-objdump -d $< > $@
