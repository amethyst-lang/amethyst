# amethyst
Amethyst is a systems language aimed at being simple, small, portable, and safe.

## What is this language?
From the r/ProgLangs discord server:
 - simple: implementing this language should be super simple. the design of this language should be super simple. it should be super simple to learn this language if youre familiar with other system langs
 - small: the base implementation should be small and with few external dependencies. the stdlib should be small but expandable, with no external dependencies besides libc
 - portable: nothing in the language should be specific to one architecture. arch specific external libs developed for the language are allowed, but if theres a backend for it, any code written in amethyst should work on it. this also means no inline assembly because *dear god how the heck do you port that*
 - safe: ~~damn how did you know i love rust~~ code should be as safe as possible without sacrificing any of the above points. whilst safety is a goal of amethyst, it is not the main goal (simplicity is)

## Cool, what does it look like?
Check out the `examples/` folder for examples, or the `self-hosted-compiler/` folder for the self hosted compiler.

