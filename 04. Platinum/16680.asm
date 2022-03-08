section .bss
    t: resq 1
    n: resq 1
    count: resq 1


section .data
    format_count: db '%d', 0
    format_in: db '%lld', 0
    format_out: db '%lld', 10, 0


section .text
    global main
    extern printf
    extern scanf


main:
    push rbp
    mov rbp, rsp

    mov rsi, t
    mov rdi, format_count
    xor rax, rax
    call scanf

    mov qword [count], 0
_main_loop:
    mov rsi, n
    mov rdi, format_in
    xor rax, rax
    call scanf

    mov rax, [n]
    mov rbx, 999999999
    mul rbx

    mov rsi, rax
    mov rdi, format_out
    xor rax, rax
    call printf

    inc qword [count]
    mov rax, [t]
    cmp rax, qword [count]
    je _main_end
    jmp _main_loop
_main_end:
    pop rbp
    xor rax, rax
    ret