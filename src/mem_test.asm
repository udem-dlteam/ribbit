bits 16

clump_size    equ 3
max_nb_clumps equ 10
heap_bot  equ 0x8000
heap_mid  equ heap_bot+(clump_size*max_nb_clumps*2)
heap_top  equ heap_mid+(clump_size*max_nb_clumps*2)

	EXTERN getchar
	EXTERN putchar
	EXTERN eq
	EXTERN add
	EXTERN sub
	EXTERN mul
	EXTERN div
	EXTERN field0
	EXTERN field1
	EXTERN field2
	EXTERN set_field0
	EXTERN set_field1
	EXTERN set_field2
	EXTERN init_heap
	EXTERN push_clump
	EXTERN pop_clump

	EXTERN print_i
	EXTERN write

	GLOBAL gc_test

	;; -------------------

print_stack:
	;;  di alloc
	;;  si stack
	mov bp, si

print_stack_next:
	cmp bp, 1
	je  print_stack_end

	mov ax, [bp]; get_field(0, probe)
	shr ax, 1

	pusha
	push ax
	push 0
	call print_i
	add  sp, 2
	popa

	mov  al, ' '
	call write

	mov bp, [bp + 2 * (clump_size - 1)]
	jmp print_stack_next

print_stack_end:
	ret
	;; -------------------

gc_test:
	a equ 24
	b equ 12

	call init_heap

	call push_clump
	mov  ax, 199
	call set_field0

	call push_clump
	mov  ax, 198
	call set_field0

	call eq

	call print_stack

	mov  al, `\r`
	call write

	mov  al, `\n`
	call write

gc_test_end:
	jmp gc_test_end
	ret
