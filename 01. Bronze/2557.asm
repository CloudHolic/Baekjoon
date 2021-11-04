section .data
	msg: db "Hello World!", 10
	
section .text
	global main
	extern printf
	
main:
	push rbp
	mov rdi, msg
	xor rax, rax
	call printf
	xor rax, rax
	ret