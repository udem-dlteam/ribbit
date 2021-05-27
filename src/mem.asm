bits 16

clump_size    equ 3
max_nb_clumps equ 10
heap_bot  equ 0x8000
heap_mid  equ heap_bot+(clump_size*max_nb_clumps*2)
heap_top  equ heap_mid+(clump_size*max_nb_clumps*2)

	EXTERN print_i
	EXTERN write
	GLOBAL gc_test

init_heap:
	;;  "alloc" variable is assigned to register di
	;;  "stack" variable is assigned to register si
	;;  register bx always points to bx_base
	mov bx, bx_base
	mov di, heap_bot;; alloc = heap_bot
	mov ax, heap_mid;; alloc_limit = heap_mid
	mov [bx+(alloc_limit-bx_base)], ax
	mov si, 1;; stack = nil
	ret

pop_clump:
	;;   "alloc" variable is assigned to register di
	;;   "stack" variable is assigned to register si
	xchg bp, si
	mov  si, [bp + 2 * (clump_size - 1)]
	ret

push_clump:
	;;   "alloc" variable is assigned to register di
	;;   "stack" variable is assigned to register si
	xchg ax, si
	mov  si, di
	stosw
	stosw
	stosw
	cmp  di, [bx+(alloc_limit-bx_base)]
	jne  return
	;;   fallthrough garbage collector

gc:
	pusha ;; save "stack" variable (and other registers)

	;;  "broken_heart" variable is assigned to register cx
	mov cx, si;; use the newly allocated clump as "broken heart"

	;;  "scan" variable is assigned to register si
	mov si, sp;; point "scan" to "stack" variable
	inc si
	inc si

	;;  setup "alloc" variable to start of tospace
	mov di, heap_mid
	cmp di, [bx+(alloc_limit-bx_base)]
	je  alloc_set
	mov di, heap_bot

alloc_set:

	push di
	mov  ax, clump_size*max_nb_clumps*2
	add  ax, di
	mov  [bx+(alloc_limit-bx_base)], ax

	call copy;; copy "stack" (the only root)

	pop si;; setup "scan" variable to start of tospace

copy_loop:
	call copy;; copy reachable object
	cmp  si, di
	jne  copy_loop

	popa ;; restore "stack" variable (and other registers)
	ret

copy:
	;;   "scan" variable is assigned to register si
	lodsw
	test al, 1
	jnz  return

	push si;; save "scan"
	xchg ax, si
	lodsw
	mov  dx, [si];; speculatively get forwarding pointer
	cmp  ax, cx;; is it a "broken heart"?
	je   update

	mov dx, di;; remember where copied

	stosw ;; copy field 0
	lodsw

	mov [si-4], cx;; store "broken heart"
	mov [si-2], dx

	stosw ;; copy field 1
	movsw ;; copy field 2

update:
	pop si;; restore "scan"
	mov [si-2], dx

return:
	ret

gc_test:
	mov  bp, sp
	call init_heap
	mov  cx, 1; i = 1
	;    i < 1001

gc_test_for:
	cmp cx, 1001
	je  gc_test_end

	call push_clump

	mov bx, cx
	and bx, 7
	cmp bx, 7
	mov bx, bx_base
	jne gc_pop

	mov cx, ax
	shl ax, 1
	inc ax
	mov word [si], ax; set_field(0, stack, fixnum(i))
	mov word [si + 2], 0; set_field(1, stack, nil)
	jmp gc_after_pop

gc_pop:
	call pop_clump

gc_after_pop:

	pusha
	xor  dx, dx
	mov  ax, cx
	mov  cx, 50
	div  cx
	test dx, dx
	popa
	jnz  gc_next_iter
	mov  al, '>'
	call write

	;;  stack print
	mov bp, si; probe = bp
	mov [si + 2], si; set_field(1, stack, stack)

gc_print_while:
	test bp, bp
	jz   gc_print_while_end

	mov ax, [bp]; get_field(, probe)
	shr ax, 1

	push ax
	push 0
	call print_i
	add  sp, 2

	mov bp, [bp + 2]
	jmp gc_print_while

gc_print_while_end:
	;;   print("\n")
	mov  al, `\r`
	call write

	mov  al, `\n`
	call write

gc_next_iter:
	;   ++i
	inc cx
	jmp gc_test_for

gc_test_end:
	mov  al, '>'
	call write
	ret

bx_base: ;; bx will point here at all times
alloc_limit: dw 0
