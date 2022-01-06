section .bss
    n: resq 1
    len: resq 1
    cur: resq 1


section .data
    format: db '%d', 10, 0


section .text
    global main
    extern printf
    extern scanf


calc:
    push rbp
    mov rbp, rsp
    mov qword [len], 1
    mov qword [cur], 1
_calc_loop:
    xor rdx, rdx
    mov rax, [cur]
    div qword [n]
    test rdx, rdx
    je _calc_end

    mov rax, 10
    mul rdx
    inc rax
    mov [cur], rax
    inc qword [len]
    jmp _calc_loop
_calc_end:
    mov rsi, [len]
    mov rdi, format
    xor rax, rax
    call printf

    leave
    ret


main:
    push rbp
    mov rbp, rsp
_main_loop:
    mov rsi, n
    mov rdi, format
    xor rax, rax
    call scanf
    cmp rax, 1
    jne _main_end
    call calc
    jmp _main_loop
_main_end:
    pop rbp
    xor rax, rax
    ret