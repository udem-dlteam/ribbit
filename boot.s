# Sector Scheme
# Boot routine + setup
# AT&T syntax

.globl boot
.globl write

.code16

boot:
	cli
	mov $'S, %al   
	call write

	hlt


# Write a char to the screen, assumes the char is inside
# AH
write:
	mov $0x0E, %ah
	int $0x10	
	ret
	
end:
	.space 512-2-(end-boot)
	.byte 0x55
	.byte 0xAA
