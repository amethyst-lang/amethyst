.global gcc_jit_context_compile_to_file_raw
.extern gcc_jit_context_compile_to_file
.global gcc_jit_block_end_with_conditional_wrapper
.extern gcc_jit_block_end_with_conditional
.global gcc_jit_block_end_with_void_return_wrapper
.extern gcc_jit_block_end_with_void_return
.global gcc_jit_lvalue_as_rvalue_wrapper
.extern gcc_jit_lvalue_as_rvalue

.intel_syntax

gcc_jit_context_compile_to_file_raw:
    mov %r11, 0x20
    sub %rsp, %r11
    call gcc_jit_context_compile_to_file
    mov %r11, 0x20
    add %rsp, %r11
    ret

gcc_jit_block_end_with_conditional_wrapper:
    mov %r11, 0x40
    sub %rsp, %r11
    call gcc_jit_block_end_with_conditional
    mov %r11, 0x40
    add %rsp, %r11
    ret

gcc_jit_block_end_with_void_return_wrapper:
    mov %r11, 0x30
    sub %rsp, %r11
    call gcc_jit_block_end_with_void_return
    mov %r11, 0x30
    add %rsp, %r11
    ret

gcc_jit_lvalue_as_rvalue_wrapper:
    mov %r11, 0x30
    sub %rsp, %r11
    call gcc_jit_lvalue_as_rvalue
    mov %r11, 0x30
    add %rsp, %r11
    ret
