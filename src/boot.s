# Sector Scheme
# Boot routine + setup
# AT&T syntax

	NIL = 0
	STACK_TOP = 0x7C00             # location of stack top

	.code16
	jmp boot

.globl boot
.globl write
.globl read

.extern gc_test
.extern dispatch

boot:
	cli
	xorw %cx, %cx
	movw %cx, %ds
	movw %cx, %ss
	movw $(STACK_TOP), %sp

load_second_sector:
	mov $DAP, %si
	mov $0x42, %ah
	int $0x13

repl:
	mov  $5, %cx
	call dispatch
	hlt

write:
	pushw %bx
	mov   $0x0E, %ah  # service code 14
	int   $0x10     # print service
	popw  %bx
	ret
nop

read:
	xor %ax, %ax # service 0
	int $0x16    # keyboard service
	ret
nop

DAP:
	.byte 0x10 # 16 bytes (1)
	.byte 0x0  # unused   (1)
	.word 0x1  # count    (2)
	.word 0x7E00 # seg    (2)
	.word 0      # off    (2)
	.long 1      # countlo(4)
	.long 0      # counthi(4)

# ----------------------- 16
