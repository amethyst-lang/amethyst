.global gcc_jit_context_compile_to_file_raw
.extern gcc_jit_context_compile_to_file
.global gcc_jit_block_end_with_conditional_wrapper
.extern gcc_jit_block_end_with_conditional
.global gcc_jit_block_end_with_void_return_wrapper
.extern gcc_jit_block_end_with_void_return

.intel_syntax

gcc_jit_context_compile_to_file_raw:
    mov %rax, 0x20
    sub %rsp, %rax
    call gcc_jit_context_compile_to_file
    mov %rax, 0x20
    add %rsp, %rax
    ret

gcc_jit_block_end_with_conditional_wrapper:
    mov %rax, 0x40
    sub %rsp, %rax
    call gcc_jit_block_end_with_conditional
    mov %rax, 0x40
    add %rsp, %rax
    ret

gcc_jit_block_end_with_void_return_wrapper:
    mov %rax, 0x30
    sub %rsp, %rax
    call gcc_jit_block_end_with_void_return
    mov %rax, 0x30
    add %rsp, %rax
    ret
