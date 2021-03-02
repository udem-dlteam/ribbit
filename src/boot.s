# Sector Scheme
# Boot routine + setup
# AT&T syntax

.globl boot
.globl c_write
.globl c_read

.extern main

.code16

boot:
	cli

	mov $'>, %al
	call write

	call main

	hlt

c_write:
	mov 4(%esp), %al
# Write an ASCII char to the screen, assumes the char is inside al
write:
	mov $0x0E, %ah  # service code 14
	int $0x10	    # print service
	ret

c_read:
# Read a keystroke, returns %ax = (scan code . ascii char)
read:
	xor %ax, %ax # service 0
	int $0x16    # keyboard service
	ret 

