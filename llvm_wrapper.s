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
.global LLVMBuildMulWrapper
.extern LLVMBuildMul
.global LLVMBuildUDivWrapper
.extern LLVMBuildUDiv
.global LLVMBuildSDivWrapper
.extern LLVMBuildSDiv
.global LLVMBuildURemWrapper
.extern LLVMBuildURem
.global LLVMBuildSRemWrapper
.extern LLVMBuildSRem
.global LLVMBuildAddWrapper
.extern LLVMBuildAdd
.global LLVMBuildSubWrapper
.extern LLVMBuildSub
.global LLVMBuildShlWrapper
.extern LLVMBuildShl
.global LLVMBuildLShrWrapper
.extern LLVMBuildLShr
.global LLVMBuildAndWrapper
.extern LLVMBuildAnd
.global LLVMBuildOrWrapper
.extern LLVMBuildOr
.global LLVMBuildXorWrapper
.extern LLVMBuildXor
.global LLVMBuildICmpWrapper
.extern LLVMBuildICmp
.global LLVMAddFunctionWrapper
.extern LLVMAddFunction

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

LLVMBuildMulWrapper:
    call LLVMBuildMul
    ret

LLVMBuildUDivWrapper:
    call LLVMBuildUDiv
    ret

LLVMBuildSDivWrapper:
    call LLVMBuildSDiv
    ret

LLVMBuildURemWrapper:
    call LLVMBuildURem
    ret

LLVMBuildSRemWrapper:
    call LLVMBuildSRem
    ret

LLVMBuildAddWrapper:
    call LLVMBuildAdd
    ret

LLVMBuildSubWrapper:
    call LLVMBuildSub
    ret

LLVMBuildShlWrapper:
    call LLVMBuildShl
    ret

LLVMBuildLShrWrapper:
    call LLVMBuildLShr
    ret

LLVMBuildAndWrapper:
    call LLVMBuildAnd
    ret

LLVMBuildOrWrapper:
    call LLVMBuildOr
    ret

LLVMBuildXorWrapper:
    call LLVMBuildXor
    ret

LLVMBuildICmpWrapper:
    call LLVMBuildICmp
    ret

LLVMAddFunctionWrapper:
    call LLVMAddFunction
    ret
