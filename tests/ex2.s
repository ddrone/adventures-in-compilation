    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movq $-10, %rdi
    callq print_int
    movq $0, %rax
    addq $0, %rsp
    popq %rbp
    retq
