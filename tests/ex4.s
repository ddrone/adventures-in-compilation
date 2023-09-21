    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movq $0, %rax
    jmp conclusion
conclusion:
    addq $0, %rsp
    popq %rbp
    retq
