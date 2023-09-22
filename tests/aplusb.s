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
    addq %rcx, %r12
    movq %r12, %rdi
    callq print_int
    movq $0, %rax
    addq $8, %rsp
    popq %r12
    popq %rbp
    retq
