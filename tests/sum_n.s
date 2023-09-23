    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    callq read_int
    movq %rax, %rdx
    movq $0, %rsi
    movq $0, %rcx
    cmpq %rdx, %rcx
    jle block_3
    jmp block_1
block_1:
    movq %rsi, %rdi
    callq print_int
    movq $0, %rax
    jmp conclusion
block_3:
    addq %rcx, %rsi
    addq $1, %rcx
    cmpq %rdx, %rcx
    jle block_3
    jmp block_1
conclusion:
    addq $0, %rsp
    popq %rbp
    retq
