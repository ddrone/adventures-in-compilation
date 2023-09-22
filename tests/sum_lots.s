    .globl main
main:
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    movq %rsp, %rbp
    subq $72, %rsp
    callq read_int
    movq %rax, %r12
    callq read_int
    movq %rax, -16(%rbp)
    callq read_int
    movq %rax, -24(%rbp)
    callq read_int
    movq %rax, -32(%rbp)
    callq read_int
    movq %rax, -40(%rbp)
    callq read_int
    movq %rax, -48(%rbp)
    callq read_int
    movq %rax, -56(%rbp)
    callq read_int
    movq %rax, -64(%rbp)
    callq read_int
    movq %rax, -72(%rbp)
    callq read_int
    movq %rax, %r13
    callq read_int
    movq %rax, %r14
    callq read_int
    movq %rax, -8(%rbp)
    callq read_int
    movq %rax, %rcx
    addq -16(%rbp), %r12
    addq -24(%rbp), %r12
    addq -32(%rbp), %r12
    addq -40(%rbp), %r12
    addq -48(%rbp), %r12
    addq -56(%rbp), %r12
    addq -64(%rbp), %r12
    addq -72(%rbp), %r12
    addq %r13, %r12
    addq %r14, %r12
    addq -8(%rbp), %r12
    addq %rcx, %r12
    movq %r12, %rdi
    callq print_int
    movq $0, %rax
    addq $72, %rsp
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    retq
