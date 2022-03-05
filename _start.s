global _start
extern main

_start:
    call main
    mov rdi, rax
    mov rax, 60
    syscall
