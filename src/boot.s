# Sector Scheme
# Boot routine + setup
# AT&T syntax

	NIL = 0
	STACK_TOP = 0x7C00             # location of stack top

	.code16
	jmp boot

.globl boot
.globl c_write
.globl c_read

.extern main

alloc_p:
	.word 0

allocl_p:
	.word 0

scan_p:
	.word 0

latest:
	.word 0

stack:
	.word 0

boot:
	cli
	std

	xorw %cx, %cx
	movw %cx, %ds
	movw %cx, %ss
	movw $(STACK_TOP), %sp

load_second_sector:
	mov $DAP, %si
	mov $0x42, %ah
	int $0x13

repl:
	mov  $'>, %al
	call write
	call main
	hlt

c_write:
	mov 4(%esp), %al

# Write an ASCII char to the screen, assumes the char is inside al
write:
	mov $0x0E, %ah  # service code 14
	int $0x10     # print service
	ret

c_read:
# Read a keystroke, returns %ax = (scan code . ascii char)
read:
	xor %ax, %ax # service 0
	int $0x16    # keyboard service
	ret

# cx = byte cursor
# dh = half byte tracker
# dl = free operator
# ah = data at ptr / being decoded
# Function stack:
# --------------------------------
# len
# -------------------------------- bp + 6
# addr
# -------------------------------- bp + 4
# ret addr
# -------------------------------- bp + 2
# old bp
# -------------------------------- bp
# decoded lo
# -------------------------------- bp - 2
# decoded hi
# -------------------------------- bp - 4
# multiple
# -------------------------------- sp ⇔ bp - 6
decode:
	pushw %bp
	movw  %sp, %bp
	xor   %cx, %cx
	movb  $1, %dh
	addw  $(-6), %sp # decoded (u32) + multiple (u16)

decode_remaining:
	movw 6(%bp), %bx
	cmpw %bx, %cx # while bt_c < len
	jge  decode_end
	decode_not_over: # do ... while
	xorl %eax, %eax
	movb %dh, %dl
	shl  $2, %dl # dl = shift_factor
	movw 4(%bp), %bx # address of the buffer in bx
	addw %cx, %bx
	movb (%bx), %al # al = buff[cx]

	xchg %dl, %cl
	shr  %cl, %al # al = buff[cx] >> shift_factor
	xchg %cl, %dl
	andb $0xF, %al # al = half_byte

	movw %cx, %bx
	movw -6(%bp), %cx # cx = multiple
	shl  $2, %cx # bl = multiple << 2 = mshift
	xorb %ah, %ah
	shl  %cl, %eax # eax = partial decoded = half_byte << mshift
	orl  %eax, -4(%bp) # decoded |= half_byte << mshift
	movw %bx, %cx
	incw -6(%bp) # multiple++
	dec  %dh # decrement halfw.
	jz   decode_check_loop # if underflow occurs, move
	inc  %cx # bt_c++
	movb $1, %dh # half_c = 1
	decode_check_loop: # cx, dh are kept, rest is trash
	xchg %bx, %cx
	movw -6(%bp), %cx # multiple in cx
	shl  $2, %cx # multiple * 4 in cx
	movl $1, %eax
	shl  %cl, %eax
	dec  %eax
	xchg %cx, %bx
	cmpl -4(%bp), %eax
	je   decode_not_over

# here would be a call to dispatch()

xor  %eax, %eax # reset decode + multiple
movl %eax, -4(%bp)
movw %ax, -6(%bp)

	jmp decode_remaining

decode_end:
	addw $6, %sp
	popw %bp
	ret

DAP:
	.byte 0x10 # 16 bytes (1)
	.byte 0x0  # unused   (1)
	.word 0x1  # count    (2)
	.word 0x7E00 # seg    (2)
	.word 0      # off    (2)
	.long 1      # countlo(4)
	.long 0      # counthi(4)

# ----------------------- 16
