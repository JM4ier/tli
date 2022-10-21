.intel_syntax noprefix
.globl stack_search

stack_search:

    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15

    call stack_search_impl

    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax

    ret
