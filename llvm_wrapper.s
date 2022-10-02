.global LLVMCreateBuilderWrapper
.extern LLVMCreateBuilder

.intel_syntax

LLVMCreateBuilderWrapper:
    call LLVMCreateBuilder
    ret

