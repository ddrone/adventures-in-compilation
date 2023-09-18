    .globl main
main:
    pushq %rbp
    pushq %r13
    movq %rsp, %rbp
    subq $8, %rsp
    callq read_int
    movq %rax, %r13
    callq read_int
    movq %rax, %rcx
    movq %r13, %rdx
    addq %rcx, %rdx
    movq %rdx, %rdi
    callq print_int
    addq $8, %rsp
    popq %r13
    popq %rbp
    retq
