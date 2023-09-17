    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
    callq read_int
    movq %rax, %r13
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
    movq %rax, -88(%rbp)
    callq read_int
    movq %rax, %r14
    callq read_int
    movq %rax, -8(%rbp)
    callq read_int
    movq %rax, -16(%rbp)
    callq read_int
    movq %rax, -24(%rbp)
    callq read_int
    movq %rax, %rcx
    movq %r13, %rdx
    addq -32(%rbp), %rdx
    addq -40(%rbp), %rdx
    addq -48(%rbp), %rdx
    addq -56(%rbp), %rdx
    addq -64(%rbp), %rdx
    addq -72(%rbp), %rdx
    addq -80(%rbp), %rdx
    addq -88(%rbp), %rdx
    addq %r14, %rdx
    addq -8(%rbp), %rdx
    addq -16(%rbp), %rdx
    addq -24(%rbp), %rdx
    addq %rcx, %rdx
    movq %rdx, %rdi
    callq print_int
    addq $80, %rsp
    popq %rbp
    retq
