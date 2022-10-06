.global LLVMCreateBuilderInContextWrapper
.extern LLVMCreateBuilderInContext
.global LLVMDumpModuleWrapper
.extern LLVMDumpModule
.global LLVMFunctionTypeWrapper
.extern LLVMFunctionType

.intel_syntax

LLVMCreateBuilderInContextWrapper:
    call LLVMCreateBuilderInContext
    ret

LLVMDumpModuleWrapper:
    call LLVMDumpModule
    ret

LLVMFunctionTypeWrapper:
    call LLVMFunctionType
    ret
