global _start
global syscall_
extern main

_start:
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
    ; TODO: 6th syscall argument
    mov r9, [rsp - 16]
    syscall
    ret
