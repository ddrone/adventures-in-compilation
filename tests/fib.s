    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    callq read_int
    movq %rax, %rsi
    movq $1, %rdi
    movq $0, %rcx
    movq $0, %rdx
    cmpq %rsi, %rdx
    jl block_3
    jmp block_1
block_1:
    movq %rcx, %rdi
    callq print_int
    movq $0, %rax
    jmp conclusion
block_3:
    movq %rdi, %r8
    addq %rcx, %rdi
    movq %r8, %rcx
    addq $1, %rdx
    cmpq %rsi, %rdx
    jl block_3
    jmp block_1
conclusion:
    addq $0, %rsp
    popq %rbp
    retq
