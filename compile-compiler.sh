#!/bin/sh
./target/release/amethyst self-hosted-compiler/compiler.amy amethystc
gcc _start.o amethystc.o -lgccjit -nostdlib -o amethystc
