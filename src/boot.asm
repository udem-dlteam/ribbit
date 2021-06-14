STACK_TOP equ 0x7C00
CLUMP_SIZE    equ 3
max_nb_clumps equ 10
heap_bot  equ 0x8000
heap_mid  equ heap_bot+(CLUMP_SIZE*max_nb_clumps*2)
heap_top  equ heap_mid+(CLUMP_SIZE*max_nb_clumps*2)

bits 16
org  0x7C00

	;; Boot is the entry point, no code allowed before the
	;; boot label

boot:
	cli
	xor cx, cx
	mov ds, cx
	mov ss, cx
	mov sp, STACK_TOP

load_second_sector:
	mov si, $DAP
	mov ah, 0x42
	int 0x13

	jmp second_sector

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
nop

pop_clump:
	;;   "alloc" variable is assigned to register di
	;;   "stack" variable is assigned to register si
	;;   'old stack' ends up in bp
	xchg bp, si
	mov  si, [bp + 2 * (CLUMP_SIZE - 1)]
	ret
nop

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
	;;    "broken_heart" variable is assigned to register cx
	mov   cx, si;; use the newly allocated clump as "broken heart"

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
	mov  ax, CLUMP_SIZE*max_nb_clumps*2
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
nop

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
nop

	;; get the address of the 'nth' 'ret' instruction where
	;; nth is in cx. The result is in bp. Smashes al.

dispatch:
	mov bp, 0x7C00 - 1

dispatch_loop:
	inc bp
	mov al, [bp]
	cmp al, 0xC3
	je  dispatch_loop_end
	jmp dispatch_loop

dispatch_loop_end:
	loop dispatch_loop
	ret

add:
	lodsw
	mov si, [si+2*(1-1)]
	dec ax
	add [si], ax
	ret
nop

sub:
	lodsw
	mov si, [si+2*(1-1)]
	dec ax
	sub [si], ax
	ret
nop

mul:
	lodsw
	mov  si, [si+2*(1-1)]
	shr  ax, 1
	dec  word [si]
	imul word [si]
	inc  ax
	mov  [si], ax
	ret
nop

div:
	lodsw
	mov  si, [si+2*(1-1)]
	dec  ax
	xchg ax, cx
	mov  ax, [si]
	dec  ax
	cwd
	idiv cx
	shl  ax, 1
	inc  ax
	mov  [si], ax
	ret
nop

first:
	call pop_clump
	ret
nop

lt:
	mov  al, 0x8
	mov  ax, [si + 0*(CLUMP_SIZE - 1)]
	call pop_clump
	cmp  ax, [si + 0*(CLUMP_SIZE - 1)]
	mov  ax, 1
	jge  eq_e
	or   al, 2
	jmp  eq_e

eq:
	mov  ax, [si + 0*(CLUMP_SIZE - 1)]
	call pop_clump
	cmp  ax, [si + 0*(CLUMP_SIZE - 1)]
	mov  ax, 1
	jne  eq_e
	or   al, 2

eq_e:
	call sf0_of_tos
	ret
nop

field0_set:
	xor cx, cx
	jmp fieldX_set

field1_set:
	mov cx, 2
	jmp fieldX_set

field2_set:
	mov cx, 4

fieldX_set:
	call gf0_of_tos
	call pop_clump;; remove top
	mov  bp, [si + 2 * (CLUMP_SIZE - 3)];; get addr. of top cell
	add  bp, cx;; add offset
	mov  [bp], ax;; write at clump
	ret
nop

field0:
	xor cx, cx
	jmp fieldX

field1:
	mov cx, 2
	jmp fieldX

field2:
	mov cx, 4

fieldX:
	mov  bp, [si + 2 * (CLUMP_SIZE - 3)]
	add  bp, cx
	mov  cx, [bp]
	call push_clump
	mov  [si + 2 * (CLUMP_SIZE - 3)], cx
	ret
nop

clump:
	mov  ax, [si + 2 * (CLUMP_SIZE - 1)];; ax has z
	call pop_clump

	mov  cx, [si + 2 * (CLUMP_SIZE - 2)];; cx has y
	call pop_clump
	;;   last clump is on stack, w. x already there
	mov  [si + 2 * (CLUMP_SIZE - 1)], ax
	mov  [si + 2 * (CLUMP_SIZE - 2)], cx
	ret
nop

putchar:
	call gf0_of_tos
	shr  ax, 1
	call write
	ret
nop

getchar:
	call push_clump
	call read
	xor  ah, ah
	shl  ax, 1
	inc  ax
	;;   fallthrough intentional so we can reuse
	;;   set_field0

sf0_of_tos:
	mov [si + 0 * (CLUMP_SIZE - 1)], ax
	;;  fallthrough intentional so we can reuse
	;;  the ret

gf0_of_tos:
	mov ax, [si + 0 * (CLUMP_SIZE - 1)]
	ret

	;; skip n clumps, starting at di
	;; goes forward n clump where n is in cx
	;; di will be the nth clump
	;; si will be the nth-1 clump
	;; smashes cx, bp, si (changed iff cx != 0)

skip_1:
	xor cx, cx
	inc cx

skip_n:
skip_n_loop:
	test cx, cx;; while cx != 0
	jz   skip_end
	dec  cx
	mov  si, di;; previous is now current
	mov  di, [di + 2 * 1];; move forward
	jmp  skip_n_loop

skip_end:
	ret

	;; find the current environment clump

env:
	;; si is back
	;; di is front
	;; when [di + 2 * 2] == 1, si is the current env. clump
	;; address is returned in si
	;; smashes si, bp

	mov di, si

env_loop:
	cmp word [di + 2 * 2], 1
	je  env_found

	call skip_1
	jmp  env_loop

env_found:
	ret

jump:
	xor dx, dx;; smaller than mov dx, 1
	inc dx
	jmp do_call

call:
	xor dx, dx

	;; call / jump to a function
	;; assumes the following:
	;; ax contains the address of g in the env
	;; si is the top of the stack

do_call:
	pusha
	;; STACK AT THIS POINT
	;; 1 AX       sp + 14
	;; 2 CX       sp + 12
	;; 3 DX       sp + 10
	;; 4 BX       sp + 8
	;; 5 Temp     sp + 6
	;; 6 BP       sp + 4
	;; 7 SI       sp + 2
	;; 8 DI       sp

	;; bp contains the current clump (the program counter)

	;;   ax contains the address of the callee's clump
	xchg bp, ax
	mov  bp, [bp];; bp contains the value of the first cell of the callee's clump
	test bp, 1;; test parity
	jpo  call_prim;; if that value is odd, it's a primitive function

	xchg si, bp

	lodsw; mov  cx, [bp];; otherwise, it's a clump address. cx contains the number of params
	xchg   cx, ax;; cx contains the number of params

	lodsw; mov  ax, [bp + 2 * 1];; ax contains the new PC

	mov bp, sp
	mov [bp + 14], ax;; store the new PC into the old ax

	xchg di, si;; di = address of the first clump of the environment
	xor  si, si;; si = address of the last clump of the arguments (null at first)

	;;   cx is set to the number of args to pop off
	call skip_n

	push si
	push di
	;;   STACK AT THIS POINT
	;;   01 AX       sp + 18
	;;   02 CX       sp + 16
	;;   03 DX       sp + 14
	;;   04 BX       sp + 12
	;;   05 Temp     sp + 10
	;;   06 BP       sp + 8
	;;   07 SI       sp + 6
	;;   08 DI       sp + 4
	;;   09 si       sp + 2    (addr. of last arg clump / null)
	;;   10 di       sp        (addr. of first clump of env)

	mov  si, [bp + 6];; si points to the TOS
	mov  di, [bp + 4];; di points to alloc
	call push_clump;; allocate the continuation clump
	mov  [bp + 4], di

	; Modify the current clump to be eq. to the previous' env clump

	push si
	call env;; si contains the addr. of the environment's clump
	pop  di

	movsw
	movsw
	movsw
	;; new clump is now a copy of the previous environment:
	;; previous env == (env, ??, code)
	;; this is correct for a jump but for a call we need to
	;; update things. We also copied the cdr, we need to fix
	;; it back (at label call_env_ok)

	;   mov bp, sp;; bp can be used to read the stack
	mov si, [bp + 6];; si points to the old TOS
	add di, -6;; di points to the new clump again

	;;   if call
	test dx, dx
	jnz  call_env_ok

	;;  When doing a call as opposed to a load, we need to change env + code
	mov ax, [bp];; ax contains the address of the environment's first clump
	mov [di + 2 * 0], ax

	mov ax, [bp + 8];; load the return value (bx + 8) = bp
	mov [di + 2 * 2], ax;; point to the return value

call_env_ok:

	mov ax, [bp + 18];; load the addr. of g (bx + 18 = ax)
	mov [di + 2 * 1], ax

	pop si;; si = first clump of the environment
	pop si;; si = last argument clump address

	;; STACK AT THIS POINT
	;; 01 AX       sp + 14
	;; 02 CX       sp + 12
	;; 03 DX       sp + 10
	;; 04 BX       sp + 8
	;; 05 Temp     sp + 6
	;; 06 BP       sp + 4
	;; 07 SI       sp + 2
	;; 08 DI       sp

	;;   here, di contains the ptr to the new allocated clump
	;;   and si contains the last argument pointer (maybe null)
	test si, si
	jz   call_after_relink
	mov  [si + 2 * 1], di

call_after_relink:
	mov si, [bp + 2];; si points to the old TOS
	mov bp, [bp + 14];; bp (PC) is now set to where to run the code
	jmp call_done

call_prim:
	call bp;; call the primitive. Result is now at TOS.

	mov bp, sp
	mov [bp], di

	test  dx, dx;; is the current function call or jump?
	jz    call_done
	;;    it's a tail-call primitive, rewind the stack
	push  si
	call  env
	;;    si contains the current continuation's clump
	lodsw ;; mov  ax, [si + 2 * 0];; ax contains the new stack
	mov   bp, [si];; bp is the new PC (si was moved to the next field by loadsw)
	pop   si
	mov   [si + 2 * 1], ax;; set the front of the stack to be the result of the primitive call

call_done:
	pop di;; restore di. Since at top of machine stack, we can use pop
	ret 14

write:
	push bx
	mov  ah, 0x0E
	int  0x10
	pop  bx
	ret

DAP:
	db 0x10
	db 0
	dw 1
	dw 0x7E00
	dw 0
	dd 1
	dd 0

first_sector_end:
	times 510-($first_sector_end-$boot) db 0
	dw    0xAA55

second_sector:
	mov  al, ':'
	call write

	mov  al, ')'
	call write

r:
	jmp r

read:
	xor ax, ax
	int 0x16
	ret

bx_base: ;; bx will point here at all times
alloc_limit: dw 0

second_sector_end:
	times 512-($second_sector_end-second_sector) db 0
