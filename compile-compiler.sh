#!/bin/sh
nasm -f elf64 _start.s -o _start.o
./target/release/amethyst self-hosted-compiler/compiler.amy amethystc
gcc _start.o amethystc.o `llvm-config --ldflags --system-libs --libs all` -nostartfiles -o amethystc
