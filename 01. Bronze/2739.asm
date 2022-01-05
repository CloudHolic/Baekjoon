section .bss
    n: resb 8
    res: resb 8
    cur: resb 8


section .data
    format: db '%d', 0
    equal: db ' = ', 0
    mult: db ' * ', 0
    termi: db 10, 0


section .text
    global main
    extern printf
    extern scanf


print:
    push rbp
    mov rbp, rsp

    mov rsi, [n]
    mov rdi, format
    xor rax, rax
    call printf

    mov rdi, mult
    xor rax, rax
    call printf

    mov rsi, [cur]
    mov rdi, format
    xor rax, rax
    call printf

    mov rdi, equal
    xor rax, rax
    call printf

    mov rsi, [res]
    mov rdi, format
    xor rax, rax
    call printf

    mov rdi, termi
    xor rax, rax
    call printf

    leave
    ret


multiply:
    push rbp
    mov rbp, rsp

    xor rax, rax
    xor rbx, rbx
    inc rax
    inc rbx
_loop:
    mov rcx, 9
    cmp rbx, rcx
    jg _end
    mov rax, rbx
    mov rdi, [n]
    mul rdi
    mov [cur], rbx
    mov [res], rax
    call print
    inc rbx
    jmp _loop
_end:
    leave
    ret


main:
    push rbp
    mov rbp, rsp

    mov rsi, n
    mov rdi, format
    xor rax, rax
    call scanf
    call multiply

    pop rbp
    xor rax, rax
    ret