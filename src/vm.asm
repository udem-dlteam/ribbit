bits 16

	EXTERN push_clump

	;; skip n clumps, starting at di
	;; goes forward n clump where n is in cx
	;; di will be the nth clump
	;; si will be the nth-1 clump
	;; smashes cx, bp, si (changed iff cx != 0)

skip_1:
	xor cx, cx
	inc cx

skip_n:
skip_n_loop:
	test cx, cx;; while cx != 0
	jz   skip_end
	dec  cx
	mov  si, di;; previous is now current
	mov  di, [di + 2 * 1];; move forward
	jmp  skip_n_loop

skip_end:
	ret

	;; find the current environment clump

env:
	;; si is back
	;; di is front
	;; when [di + 2 * 2] == 1, si is the current env. clump
	;; address is returned in si
	;; smashes si, bp

	mov di, si

env_loop:
	cmp word [di + 2 * 2], 1
	je  env_found

	call skip_1
	jmp  env_loop

env_found:
	ret

jump:
	xor dx, dx;; smaller than mov dx, 1
	inc dx
	jmp do_call

call:
	xor dx, dx

	;; call / jump to a function
	;; assumes the following:
	;; ax contains the address of g in the env
	;; si is the top of the stack

do_call:
	pusha
	;; STACK AT THIS POINT
	;; 1 AX       sp + 14
	;; 2 CX       sp + 12
	;; 3 DX       sp + 10
	;; 4 BX       sp + 8
	;; 5 Temp     sp + 6
	;; 6 BP       sp + 4
	;; 7 SI       sp + 2
	;; 8 DI       sp

	;; bp contains the current clump (the program counter)

	;;   ax contains the address of the callee's clump
	xchg bp, ax
	mov  bp, [bp];; bp contains the value of the first cell of the callee's clump
	test bp, 1;; test parity
	jpo  call_prim;; if that value is odd, it's a primitive function

	xchg si, bp

	lodsw; mov  cx, [bp];; otherwise, it's a clump address. cx contains the number of params
	xchg   cx, ax;; cx contains the number of params

	lodsw; mov  ax, [bp + 2 * 1];; ax contains the new PC

	mov bp, sp
	mov [bp + 14], ax;; store the new PC into the old ax

	xchg di, si;; di = address of the first clump of the environment
	xor  si, si;; si = address of the last clump of the arguments (null at first)

	;;   cx is set to the number of args to pop off
	call skip_n

	push si
	push di
	;;   STACK AT THIS POINT
	;;   01 AX       sp + 18
	;;   02 CX       sp + 16
	;;   03 DX       sp + 14
	;;   04 BX       sp + 12
	;;   05 Temp     sp + 10
	;;   06 BP       sp + 8
	;;   07 SI       sp + 6
	;;   08 DI       sp + 4
	;;   09 si       sp + 2    (addr. of last arg clump / null)
	;;   10 di       sp        (addr. of first clump of env)

	mov  si, [bp + 6];; si points to the TOS
	mov  di, [bp + 4];; di points to alloc
	call push_clump;; allocate the continuation clump
	mov  [bp + 4], di

	; Modify the current clump to be eq. to the previous' env clump

	push si
	call env;; si contains the addr. of the environment's clump
	pop  di

	movsw
	movsw
	movsw
	;; new clump is now a copy of the previous environment:
	;; previous env == (env, ??, code)
	;; this is correct for a jump but for a call we need to
	;; update things. We also copied the cdr, we need to fix
	;; it back (at label call_env_ok)

	;   mov bp, sp;; bp can be used to read the stack
	mov si, [bp + 6];; si points to the old TOS
	add di, -6;; di points to the new clump again

	;;   if call
	test dx, dx
	jnz  call_env_ok

	;;  When doing a call as opposed to a load, we need to change env + code
	mov ax, [bp];; ax contains the address of the environment's first clump
	mov [di + 2 * 0], ax

	mov ax, [bp + 8];; load the return value (bx + 8) = bp
	mov [di + 2 * 2], ax;; point to the return value

call_env_ok:

	mov ax, [bp + 18];; load the addr. of g (bx + 18 = ax)
	mov [di + 2 * 1], ax

	pop si;; si = first clump of the environment
	pop si;; si = last argument clump address

	;; STACK AT THIS POINT
	;; 01 AX       sp + 14
	;; 02 CX       sp + 12
	;; 03 DX       sp + 10
	;; 04 BX       sp + 8
	;; 05 Temp     sp + 6
	;; 06 BP       sp + 4
	;; 07 SI       sp + 2
	;; 08 DI       sp

	;;   here, di contains the ptr to the new allocated clump
	;;   and si contains the last argument pointer (maybe null)
	test si, si
	jz   call_after_relink
	mov  [si + 2 * 1], di

call_after_relink:
	mov si, [bp + 2];; si points to the old TOS
	mov bp, [bp + 14];; bp (PC) is now set to where to run the code
	jmp call_done

call_prim:
	call bp;; call the primitive. Result is now at TOS.

	mov bp, sp
	mov [bp], di

	test  dx, dx;; is the current function call or jump?
	jz    call_done
	;;    it's a tail-call primitive, rewind the stack
	push  si
	call  env
	;;    si contains the current continuation's clump
	lodsw ;; mov  ax, [si + 2 * 0];; ax contains the new stack
	mov   bp, [si];; bp is the new PC (si was moved to the next field by loadsw)
	pop   si
	mov   [si + 2 * 1], ax;; set the front of the stack to be the result of the primitive call

call_done:
	pop di;; restore di. Since at top of machine stack, we can use pop
	ret 14
