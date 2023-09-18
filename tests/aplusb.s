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
    addq %rcx, %r13
    movq %r13, %rdi
    callq print_int
    addq $8, %rsp
    popq %r13
    popq %rbp
    retq
