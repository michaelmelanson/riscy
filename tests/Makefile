
all: add.s add.m fib.s fib.m
clean:
	rm selfie selfie.c
	rm add.s add.m fib.s fib.m
.PHONY: all clean

selfie.c: 
	curl https://raw.githubusercontent.com/cksystemsteaching/selfie/master/selfie.c > selfie.c

selfie: selfie.c
	cc -Wall -Wextra -O3 -m64 -Duint64_t='unsigned long long' selfie.c -o selfie

%.s: %.c selfie Makefile
	./selfie -c $< -S $@

%.m: %.c selfie Makefile
	./selfie -c $< -o $@