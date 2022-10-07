.global LLVMCreateBuilderInContextWrapper
.extern LLVMCreateBuilderInContext
.global LLVMDumpModuleWrapper
.extern LLVMDumpModule
.global LLVMFunctionTypeWrapper
.extern LLVMFunctionType
.global LLVMConstIntWrapper
.extern LLVMConstInt
.global LLVMInitializeX86TargetMCWrapper
.extern LLVMInitializeX86TargetMC
.global LLVMCreateTargetMachineWrapper
.extern LLVMCreateTargetMachine
.global LLVMTargetMachineEmitToFileWrapper
.extern LLVMTargetMachineEmitToFile

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

LLVMConstIntWrapper:
    call LLVMConstInt
    ret

LLVMInitializeX86TargetMCWrapper:
    call LLVMInitializeX86TargetMC
    ret

LLVMCreateTargetMachineWrapper:
    mov %r11, 0
    push %r11
    mov %r11, [%rsp + 0x10]
    push %r11
    call LLVMCreateTargetMachine
    pop %r11
    pop %r11
    ret

LLVMTargetMachineEmitToFileWrapper:
    call LLVMTargetMachineEmitToFile
    ret
