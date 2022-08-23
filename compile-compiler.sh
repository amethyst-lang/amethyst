#!/bin/sh
nasm -f elf64 _start.s -o _start.o
gcc -c gcc_jit_wrapper.s -o gcc_jit_wrapper.o
./target/release/amethyst self-hosted-compiler/compiler.amy amethystc
gcc _start.o gcc_jit_wrapper.o amethystc.o -lgccjit -nostdlib -o amethystc
