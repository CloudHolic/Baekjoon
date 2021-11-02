section .data
	msg: db "Hello World!", 0
	
section .data
	global main
	extern printf
	
main:
	push rbp
	mov rdi, msg
	xor rax, rax
	call printf
	xor rax, rax
	ret