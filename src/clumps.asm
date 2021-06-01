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

sub:
	lodsw
	mov si, [si+2*(1-1)]
	dec ax
	sub [si], ax
	ret

mul:
	lodsw
	mov  si, [si+2*(1-1)]
	shr  ax, 1
	dec  word [si]
	imul word [si]
	inc  ax
	mov  [si], ax
	ret

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

first:
	call pop_clump
	ret

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

	;; (clump x y z)
	;; make a clump (utiliser x comme clump)
	;; x = field0, y = field1, z=field3
	;; smashes ax, cx

	;; (field0 clump)
	;; field0 of clump in clump

field2:
	mov bp, [si + 2 * (CLUMP_NB_FIELDS - 3)]
	mov cx, [bp + 2 * (CLUMP_NB_FIELDS - 1)]
	jmp field_done

field1:
	mov bp, [si + 2 * (CLUMP_NB_FIELDS - 3)]
	mov cx, [bp + 2 * (CLUMP_NB_FIELDS - 2)]
	jmp field_done

field0:
	mov bp, [si + 2 * (CLUMP_NB_FIELDS - 3)]
	mov cx, [bp + 2 * (CLUMP_NB_FIELDS - 3)]

field_done:
	call push_clump
	mov  [si + 2 * (CLUMP_NB_FIELDS - 3)], cx
	ret

clump:
	mov  ax, [si + 2 * (CLUMP_NB_FIELDS - 1)];; ax has z
	call pop_clump

	mov  cx, [si + 2 * (CLUMP_NB_FIELDS - 2)];; cx has y
	call pop_clump
	;;   last clump is on stack, w. x already there
	mov  [si + 2 * (CLUMP_NB_FIELDS - 1)], ax
	mov  [si + 2 * (CLUMP_NB_FIELDS - 2)], cx
	ret

putchar:
	call gf0_of_tos
	shr  ax, 1
	call write
	ret

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
