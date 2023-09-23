    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    cmpq $0, %rcx
    jg block_3
    jmp block_1
block_1:
    movq %rdx, %rdi
    callq print_int
    movq $0, %rax
    jmp conclusion
block_3:
    addq %rcx, %rdx
    subq $1, %rcx
    cmpq $0, %rcx
    jg block_3
    jmp block_1
conclusion:
    addq $0, %rsp
    popq %rbp
    retq
