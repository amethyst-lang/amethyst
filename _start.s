global _start
global syscall_
extern main

_start:
    pop rsi
    mov rdi, rsp
    call main
    mov rdi, rax
    mov rax, 60
    syscall

syscall_:
    mov rax, rdi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    mov r10, r8
    mov r8, r9
    mov r9, [rsp + 8]
    syscall
    ret
