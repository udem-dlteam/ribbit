# Sector Scheme
# Boot routine + setup
# AT&T syntax

.globl boot

boot:
	.code16

	mov $0x0e, %ah   # function number = 0Eh : Display Character
	mov $33, %al     # AL = code of character to display
	int $0x10        # call INT 10h, BIOS video service

	jmp boot


