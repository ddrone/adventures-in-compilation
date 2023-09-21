    .globl main
main:
    pushq %rbp
    pushq %r13
    pushq %r14
    movq %rsp, %rbp
    subq $80, %rsp
    callq read_int
    movq %rax, %r13
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
    movq %rax, -80(%rbp)
    callq read_int
    movq %rax, %r14
    callq read_int
    movq %rax, -8(%rbp)
    callq read_int
    movq %rax, -16(%rbp)
    callq read_int
    movq %rax, %rcx
    addq -24(%rbp), %r13
    addq -32(%rbp), %r13
    addq -40(%rbp), %r13
    addq -48(%rbp), %r13
    addq -56(%rbp), %r13
    addq -64(%rbp), %r13
    addq -72(%rbp), %r13
    addq -80(%rbp), %r13
    addq %r14, %r13
    addq -8(%rbp), %r13
    addq -16(%rbp), %r13
    addq %rcx, %r13
    movq %r13, %rdi
    callq print_int
    movq $0, %rax
    jmp conclusion
conclusion:
    addq $80, %rsp
    popq %r14
    popq %r13
    popq %rbp
    retq
