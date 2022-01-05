section .data
	msg: db 'Hello World!', 10
	

section .text
	global main
	extern printf
	

main:
	push rbp
	mov rbp, rsp

	mov rdi, msg
	xor rax, rax
	call printf

	pop rbp
	xor rax, rax
	ret