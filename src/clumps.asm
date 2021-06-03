bits 16

CLUMP_NB_FIELDS equ 3

	EXTERN read
	EXTERN write
	EXTERN push_clump
	EXTERN pop_clump
	GLOBAL first
	GLOBAL getchar
	GLOBAL putchar
	GLOBAL add
	GLOBAL sub
	GLOBAL mul
	GLOBAL div
	GLOBAL eq
	GLOBAL lt
	GLOBAL sf0_of_tos
	GLOBAL gf0_of_tos

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
	mov  ax, [si + 0*(CLUMP_NB_FIELDS - 1)]
	call pop_clump
	cmp  ax, [si + 0*(CLUMP_NB_FIELDS - 1)]
	mov  ax, 1
	jge  eq_e
	or   al, 2
	jmp  eq_e

eq:
	mov  ax, [si + 0*(CLUMP_NB_FIELDS - 1)]
	call pop_clump
	cmp  ax, [si + 0*(CLUMP_NB_FIELDS - 1)]
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
	mov  bp, [si + 2 * (CLUMP_NB_FIELDS - 3)];; get addr. of top cell
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
	mov  bp, [si + 2 * (CLUMP_NB_FIELDS - 3)]
	add  bp, cx
	mov  cx, [bp]
	call push_clump
	mov  [si + 2 * (CLUMP_NB_FIELDS - 3)], cx
	ret
nop

clump:
	mov  ax, [si + 2 * (CLUMP_NB_FIELDS - 1)];; ax has z
	call pop_clump

	mov  cx, [si + 2 * (CLUMP_NB_FIELDS - 2)];; cx has y
	call pop_clump
	;;   last clump is on stack, w. x already there
	mov  [si + 2 * (CLUMP_NB_FIELDS - 1)], ax
	mov  [si + 2 * (CLUMP_NB_FIELDS - 2)], cx
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
	mov [si + 0 * (CLUMP_NB_FIELDS - 1)], ax
	;;  fallthrough intentional so we can reuse
	;;  the ret

gf0_of_tos:
	mov ax, [si + 0 * (CLUMP_NB_FIELDS - 1)]
	ret
nop
