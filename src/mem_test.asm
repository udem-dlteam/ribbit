bits 16

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
	call init_heap
	mov  cx, 1; i = 1
	;    i < 1001

gc_test_for:
	cmp cx, 200
	je  gc_test_end

	call push_clump

	;;  check if (cx & 7) == 7
	mov ax, cx
	and ax, 7
	cmp ax, 7
	jne gc_pop

	;;  if(...) {
	;;  put cx into ax and covn
	mov ax, cx
	shl ax, 1
	inc ax
	mov word [si], ax; set_field(0, stack, fixnum(i))
	mov word [si + 2], 1; set_field(1, stack, nil)
	;;  }
	jmp gc_pop_end

	;; else {

gc_pop:
	call pop_clump
	;;   }

gc_pop_end:

	pusha
	;;   check if cx % 50 == 0
	xor  dx, dx
	mov  ax, cx
	mov  cx, 5
	div  cx
	test dx, dx
	popa

	jnz  gc_next_iter
	call print_stack

	mov  al, `\r`
	call write

	mov  al, `\n`
	call write

gc_next_iter:
	inc cx
	jmp gc_test_for

gc_test_end:
	ret
