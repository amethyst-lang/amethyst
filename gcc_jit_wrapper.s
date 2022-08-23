.global gcc_jit_context_compile_to_file_raw
.extern gcc_jit_context_compile_to_file

.intel_syntax

gcc_jit_context_compile_to_file_raw:
    mov %rax, 0x20
    sub %rsp, %rax
    call gcc_jit_context_compile_to_file
    mov %rax, 0x20
    add %rsp, %rax
    ret
