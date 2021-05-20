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

# disk number is already loaded-in by BIOS
mov  $DAP, %si
mov  $0x42, %ah
int  $0x13
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

DAP:
	.byte 0x10 # 16 bytes (1)
	.byte 0x0  # unused   (1)
	.word 0x1  # count    (2)
	.word 0x7E00 # seg    (2)
	.word 0      # off    (2)
	.long 1      # countlo(4)
	.long 0      # counthi(4)

# ----------------------- 16
