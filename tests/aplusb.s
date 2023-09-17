    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    callq read_int
    movq %rax, %r13
    callq read_int
    movq %rax, %rcx
    movq %r13, %rdx
    addq %rcx, %rdx
    movq %rdx, %rdi
    callq print_int
    addq $0, %rsp
    popq %rbp
    retq
