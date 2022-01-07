section .bss
    n: resq 1
    total: resq 1
    mid: resq 1
    temp: resq 1


section .data
    format: db '%d', 0
    m: db 'm', 0
    o: db 'o', 0


section .text
    global main
    extern printf
    extern scanf


solve:
    mov rax, [total]
    sub rax, [mid]
    mov rbx, 2
    xor rdx, rdx
    div rbx
    mov [temp], rax
    cmp [n], rax
    jg _solve_mid
    dec qword [mid]
    mov rax, [temp]
    mov [total], rax
    jmp solve
_solve_mid:
    mov rax, [temp]
    add rax, [mid]
    cmp [n], rax
    jle _solve_end
    mov rbx, [n]
    sub rbx, rax
    mov [n], rbx
    dec qword [mid]
    mov rax, [temp]
    mov [total], rax
    jmp solve
_solve_end:
    mov rax, [n]
    mov rbx, [temp]
    sub rax, rbx
    cmp rax, 1
    je _print_m
    jne _print_o


_print_m:
    mov rdi, m
    xor rax, rax
    call printf
    jmp _main_end
_print_o:
    mov rdi, o
    xor rax, rax
    call printf
    jmp _main_end


main:
    push rbp
    mov rbp, rsp

    mov rsi, n
    mov rdi, format
    xor rax, rax
    call scanf

    mov rax, 3
    mov rbx, 3
_main_loop:
    mov rcx, 2
    mul rcx
    inc rbx
    add rax, rbx
    mov [total], rax
    mov [mid], rbx
    cmp rax, [n]
    jl _main_loop
    jmp solve
_main_end:
    pop rbp
    xor rax, rax
    ret