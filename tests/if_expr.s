    .globl main
main:
    pushq %rbp
    pushq %r12
    movq %rsp, %rbp
    subq $8, %rsp
    callq read_int
    movq %rax, %r12
    callq read_int
    movq %rax, %rcx
    cmpq $1, %r12
    jl block_5
    jmp block_6
block_1:
    movq %rcx, %rdi
    callq print_int
    movq $0, %rax
    jmp conclusion
block_2:
    addq $2, %rcx
    jmp block_1
block_3:
    addq $10, %rcx
    jmp block_1
block_5:
    movq $0, %rax
    cmpq %r12, %rax
    sete %al
    movzbq %al, %rdx
    cmpq $0, %r12
    je block_2
    jmp block_3
block_6:
    movq $2, %rax
    cmpq %r12, %rax
    sete %al
    movzbq %al, %rdx
    cmpq $2, %r12
    je block_2
    jmp block_3
conclusion:
    addq $8, %rsp
    popq %r12
    popq %rbp
    retq
