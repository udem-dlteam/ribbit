bits 16

	EXTERN print_i
	GLOBAL dispatch

	;; get the address of the 'nth' 'ret' instruction where
	;; nth is in cx. The result is in bp. Smashes al.

dispatch:
	mov bp, 0x7C00 - 1

dispatch_loop:
	inc bp
	mov al, [bp]
	cmp al, 0xC3
	je  dispatch_loop_end
	jmp dispatch_loop

dispatch_loop_end:
	loop dispatch_loop
	ret
