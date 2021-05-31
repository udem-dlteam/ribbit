bits 16

CLUMP_NB_FIELDS equ 3

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
