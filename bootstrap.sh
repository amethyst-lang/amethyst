#!/bin/sh
./amethystc self-hosted-compiler/compiler.amy
gcc a.out.o `llvm-config --ldflags --system-libs --libs all`
