section .bss
    max: resq 1
    min: resq 1
    ans: resq 1
    sieve: resb 1000001


section .data
    format_in: dq '%lld %lld', 0
    format_out: dq '%d', 0


section .text
    global main
    extern printf
    extern scanf


main:
    push rbp
    mov rbp, rsp

    mov rdx, max
    mov rsi, min
    mov rdi, format_in
    xor rax, rax
    call scanf

    mov rax, [max]
    sub rax, [min]
    inc rax
    mov [ans], rax
    mov rcx, 2
_main_loop:
    mov rax, rcx
    mul rcx
    mov rbx, [max]
    cmp rax, rbx
    jg _main_end
    mov r8, rax
    mov rax, [min]
    xor rdx, rdx
    div r8
    mov r9, rax
    cmp rdx, 0
    je _inner_loop
    add r9, 1
_inner_loop:
    mov rax, r9
    mul r8
    mov rbx, [max]
    cmp rax, rbx
    jg _inner_end
    sub rax, [min]
    mov r10b, byte [sieve + rax]
    cmp r10, 0
    jne _pass_sieve
    mov byte [sieve + rax], 1
    mov rax, [ans]
    dec rax
    mov [ans], rax
_pass_sieve:
    inc r9
    jmp _inner_loop
_inner_end:
    inc rcx
    jmp _main_loop
_main_end:
    mov rsi, [ans]
    mov rdi, format_out
    xor rax, rax
    call printf

    pop rbp
    xor rax, rax
    ret