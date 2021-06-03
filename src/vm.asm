bits 16

	EXTERN push_clump

	;; Pseudo impl. of function call, not primitives
	;; assumes the following:
	;; ax contains the address of g in the env
	;; bp contains the current clump (the program counter)

call:
	pusha

	;;   ax contains the address of the callee's clump
	xchg bp, ax
	mov  bp, [bp];; bp contains the value of the first cell of the callee's clump
	test bp, 1;; if that value is odd, it's a primitive
	jpo  call_prim
	mov  cx, [bp];; otherwise, it's a clump address. cx contains the number of params
	mov  ax, [bp + 2];; ax contains the new PC

	xchg bp, si;; bp = first_env_clump
	xor  si, si;; si = last_arg_clump (null at first)

	;; cx is set to the number of args

call_search:
	test cx, cx;; while cx-- != 0
	jz   call_search_done
	dec  cx
	mov  si, bp;; last_arg_clump = first_env_clump
	mov  bp, [bp + 2];; first_env_clump = next_clump(first_env_clump)
	jmp  call_search

call_search_done:
	;;   bp = first_env_clump
	;;   si = last_arg_clump (maybe 0 == nil)
	push si
	push bx

	mov bx, sp
	mov si, [bx + 6];; si contains the first stack obj (bx + 6 = si)

	call push_clump;; allocate the continuation clump
	mov  [si], bp;; point to the environment

	mov bp, [bx + 18];; load the addr. of g (bx + 18 = ax)
	mov [si + 2 * 1], bp

	mov bp, [bx + 8];; load the return value (bx + 8) = bp
	mov [si + 2 * 2], bp;; point to the return value

	pop  bx
	xchg bp, si;; bp = new clump addr
	pop  si;; si = last arg ptr

	;;   here, bp contains the ptr to the new allocated clump
	;;   and si contains the last argument pointer (maybe null)
	test si, si
	jz   call_after_relink
	mov  [si + 2], bp

call_after_relink:
	mov bp, ax;; bp (PC) is now set to where to run the code
	jmp call_done

call_prim:
	call bp

call_done:
	add sp, 16;; clean stack, do not restore because we changed stuff around
	ret
