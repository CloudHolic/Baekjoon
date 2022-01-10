section .bss
    n: resq 1
    count: resq 1
    result: resq 1


section .data
    format: db '%d', 0


section .text
    global main
    extern printf
    extern scanf


solve:
    push rbp
    mov rbp, rsp

    mov rax, [n]
    cmp rax, 100
    jl _solve_less_100
    xor rbx, rbx
    add rbx, 100
_solve_loop:
    mov rcx, [n]
    cmp rbx, rcx
    jg _solve_greater_100
    mov rax, rbx
    mov r8, 100
    xor rdx, rdx
    div r8
    mov r8, rax
    mov rax, rdx
    xor rdx, rdx
    mov r9, 10
    div r9
    mov r9, rax
    mov r10, rdx
    sub r8, r9
    sub r9, r10
    inc rbx
    cmp r8, r9
    jne _solve_loop
    inc qword [count]
    jmp _solve_loop
_solve_less_100:
    mov rax, [n]
    mov qword [result], rax
    jmp _solve_end
_solve_greater_100:
    mov rax, [count]
    add rax, 99
    mov qword [result], rax
_solve_end:
    leave
    ret


main:
    push rbp
    mov rbp, rsp

    mov rsi, n
    mov rdi, format
    xor rax, rax
    call scanf
    call solve
    mov rsi, [result]
    mov rdi, format
    xor rax, rax
    call printf

    pop rbp
    xor rax, rax
    ret