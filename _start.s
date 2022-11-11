global _start
global syscall_
extern main

%ifdef NetBSD
section .note.netbsd.ident
	dd	7, 4, 1
	db	"NetBSD", 0, 0
	dd	200000000
%endif

%ifdef OpenBSD
section .note.openbsd.ident
	align	2
	dd	8, 4, 1
	db	"OpenBSD", 0
	dd	0
	align 2
%endif

%ifdef UNIX
	; Unix / Unix-like
	%define SYS_exit	1
%else
	%define SYS_exit	60
%endif


_start:
    pop rsi
    mov rdi, rsp
	push rsi
	and rsp, -16
    call main
    mov rdi, rax
    mov rax, SYS_exit
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
