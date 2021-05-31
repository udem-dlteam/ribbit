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
	GLOBAL field0
	GLOBAL field1
	GLOBAL field2
	GLOBAL set_field0
	GLOBAL set_field1
	GLOBAL set_field2
	GLOBAL eq

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
	mov al, 0x8

eq:
	;    add  al, 0x75
	;    mov  byte  [jmp_i + 1], al
	mov  ax, [si + 0*(CLUMP_NB_FIELDS - 1)]
	call pop_clump
	cmp  ax, [si + 0*(CLUMP_NB_FIELDS - 1)]
	mov  ax, 1
	;;   TODO
	jmp_i: jge  eq_e
	or   al, 2

eq_e:
	call set_field0
	ret

	; lahf
	; and  word 16384
	; shr  ax, 14
	; inc  ax
	; mov  [si + 0*(CLUMP_NB_FIELDS - 1)], ax
	; ret

putchar:
	call field0
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

set_field0:
	mov [si + 0 * (CLUMP_NB_FIELDS - 1)], ax
	;;  fallthrough intentional so we can reuse
	;;  the ret

field0:
	mov ax, [si + 0 * (CLUMP_NB_FIELDS - 1)]
	ret

set_field1:
	mov [si + 1 * (CLUMP_NB_FIELDS - 1)], ax
	;;  fallthrough intentional so we can reuse
	;;  the ret

field1:
	mov ax, [si + 1 * (CLUMP_NB_FIELDS - 1)]
	ret

set_field2:
	mov [si + 2 * (CLUMP_NB_FIELDS - 1)], ax
	;;  fallthrough intentional so we can reuse
	;;  the ret

field2:
	mov ax, [si + 2 * (CLUMP_NB_FIELDS - 1)]
	ret
