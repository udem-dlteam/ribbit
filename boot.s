# Sector Scheme
# Boot routine + setup
# AT&T syntax

.globl boot
.globl write
.globl read

.code16
.section .boot
boot:
	cli

	mov $'>, %al
	call write

loop:
	call read
	call write

	jmp loop


	hlt

# Write an ASCII char to the screen, assumes the char is inside al
write:
	mov $0x0E, %ah  # service code 14
	int $0x10	    # print service
	ret

# Read a keystroke, returns %ax = (scan code . ascii char)
read:
	xor %ax, %ax # service 0
	int $0x16    # keyboard service
	ret 

