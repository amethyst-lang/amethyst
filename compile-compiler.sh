#!/bin/sh
./target/release/amethyst self-hosted-compiler/compiler.amy amethystc
gcc amethystc.o `llvm-config --ldflags --system-libs --libs all` -o amethystc
