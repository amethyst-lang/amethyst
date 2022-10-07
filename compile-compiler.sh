#!/bin/sh
nasm -f elf64 _start.s -o _start.o
gcc -c llvm_wrapper.s -o llvm_wrapper.o
./target/release/amethyst self-hosted-compiler/compiler.amy amethystc
gcc _start.o amethystc.o llvm_wrapper.o `llvm-config --ldflags --system-libs --libs all` -nostartfiles -o amethystc
