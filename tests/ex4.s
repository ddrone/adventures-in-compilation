    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movq $42, %rax
    addq $0, %rsp
    popq %rbp
    retq
