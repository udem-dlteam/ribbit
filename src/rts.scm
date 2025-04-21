;;;============================================================================

;;; File: "rts.scm"

;;; Copyright (c) 1994-2022 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; This module contains the X86-64 runtime system routines for doing
;;; portable I/O (macOS and linux supported).

;;;============================================================================

(define (gen-putchar cgc putchar)

  (define write_syscall_macos (asm-make-label cgc 'write_syscall_macos))
  (define write_syscall_linux (asm-make-label cgc 'write_syscall_linux))
  (define write_syscall       (asm-make-label cgc 'write_syscall))

;; The putchar function sends a single character to the
;; standard output (stdout).  The calling convention is
;; to push to the stack a word (64 bits) containing the
;; character to write, and then to execute a "call putchar".
;; When control returns from the function, the stack will
;; be back to the state it had before the character was
;; pushed.

(x86-label cgc putchar)
  (x86-push cgc (x86-r11)) ;; push the 7 registers modified
  (x86-push cgc (x86-rbp))
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-rbx))

  ;; determine if OS is linux or macOS

  (x86-mov  cgc (x86-rdi) (x86-imm-int -1 0)) ;; parameter of system call is -1
  (x86-mov  cgc (x86-rax) (x86-imm-int 13 0)) ;; system call 13 is "rt_sigaction" on linux and a noop on macOS
  (x86-syscall cgc)                           ;; perform system call to OS

  (x86-cmp  cgc (x86-rax) (x86-imm-int 0 0))  ;; negative means error which means linux
  (x86-js   cgc write_syscall_linux)

(x86-label cgc write_syscall_macos)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x2000004 0)) ;; "write" system call is 0x2000004
  (x86-jmp  cgc write_syscall)

(x86-label cgc write_syscall_linux)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x0000001 0)) ;; "write" system call is 0x0000001

(x86-label cgc write_syscall)
  (x86-lea  cgc (x86-rsi) (x86-mem (* 8 8) (x86-rsp))) ;; get address of character to write
  (x86-mov  cgc (x86-rdx) (x86-imm-int 1 0))           ;; number of bytes to write = 1
  (x86-mov  cgc (x86-rdi) (x86-imm-int 1 0))           ;; file descriptor 1 = stdout
  (x86-syscall cgc)                                    ;; perform system call to OS

  (x86-pop  cgc (x86-rbx)) ;; pop the 7 registers modified
  (x86-pop  cgc (x86-rcx))
  (x86-pop  cgc (x86-rdx))
  (x86-pop  cgc (x86-rdi))
  (x86-pop  cgc (x86-rsi))
  (x86-pop  cgc (x86-rbp))
  (x86-pop  cgc (x86-r11))

  (x86-ret  cgc (* 8 1)) ;;  return and pop parameter
)

;;;============================================================================

(define (gen-getchar cgc getchar)

  (define read_syscall_macos (asm-make-label cgc 'read_syscall_macos))
  (define read_syscall_linux (asm-make-label cgc 'read_syscall_linux))
  (define read_syscall       (asm-make-label cgc 'read_syscall))
  (define getchar1           (asm-make-label cgc 'getchar1))

;; The getchar function reads a single character from the
;; standard input (stdin).  The calling convention is
;; to execute a "call getchar".  When control
;; returns from the function, the register %rax will
;; contain the character (byte) that was read.  The
;; value -1 is returned when end-of-file is reached.

(x86-label cgc getchar)
  (x86-push cgc (x86-r11)) ;; push the 7 registers modified
  (x86-push cgc (x86-rbp))
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-rbx))

  ;; determine if OS is linux or macOS

  (x86-mov  cgc (x86-rdi) (x86-imm-int -1 0)) ;; parameter of system call is -1
  (x86-mov  cgc (x86-rax) (x86-imm-int 13 0)) ;; system call 13 is "rt_sigaction" on linux and a noop on macOS
  (x86-syscall cgc)                           ;; perform system call to OS

  (x86-cmp  cgc (x86-rax) (x86-imm-int 0 0))  ;; negative means error which means linux
  (x86-js   cgc read_syscall_linux)

(x86-label cgc read_syscall_macos)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x2000003 0)) ;; "read" system call is 0x2000003
  (x86-jmp  cgc read_syscall)

(x86-label cgc read_syscall_linux)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x0000000 0)) ;; "read" system call is 0x0000000

(x86-label cgc read_syscall)
  (x86-push cgc (x86-imm-int 0 0))               ;; buffer
  (x86-lea  cgc (x86-rsi) (x86-mem 0 (x86-rsp))) ;; get address of buffer
  (x86-mov  cgc (x86-rdx) (x86-imm-int 1 0))     ;; number of bytes to read = 1
  (x86-mov  cgc (x86-rdi) (x86-imm-int 0 0))     ;; file descriptor 0 = stdin
  (x86-syscall cgc)                              ;; perform system call to OS

  (x86-cmp  cgc (x86-rax) (x86-imm-int 1 0))     ;; did we read 1 byte?
  (x86-pop  cgc (x86-rax))
  (x86-je   cgc getchar1)
  (x86-mov  cgc (x86-rax) (x86-imm-int -1 0))    ;; indicate end-of-file
(x86-label cgc getchar1)

  (x86-pop  cgc (x86-rbx)) ;; pop the 7 registers modified
  (x86-pop  cgc (x86-rcx))
  (x86-pop  cgc (x86-rdx))
  (x86-pop  cgc (x86-rdi))
  (x86-pop  cgc (x86-rsi))
  (x86-pop  cgc (x86-rbp))
  (x86-pop  cgc (x86-r11))

  (x86-ret  cgc 0) ;;  return
)

;;;============================================================================

(define (gen-print_string cgc print_string putchar)

  (define print_string_loop      (asm-make-label cgc 'print_string_loop))
  (define print_string_loop_done (asm-make-label cgc 'print_string_loop_done))

;; The print_string function sends a null-terminated string
;; of characters to the standard output (stdout).  The calling
;; convention is to push to the stack a word (64 bits) containing
;; the address of the string to write, and then to execute a
;; "call print_string".  When control returns from the function,
;; the stack and all the registers will be back to the state they
;; had before the string was pushed.

(x86-label cgc print_string)
  (x86-pushf cgc)          ;; push flags register
  (x86-push cgc (x86-rax)) ;; push 15 registers (all registers except rsp)
  (x86-push cgc (x86-rbx))
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rbp))
  (x86-push cgc (x86-r8))
  (x86-push cgc (x86-r9))
  (x86-push cgc (x86-r10))
  (x86-push cgc (x86-r11))
  (x86-push cgc (x86-r12))
  (x86-push cgc (x86-r13))
  (x86-push cgc (x86-r14))
  (x86-push cgc (x86-r15))

  (x86-mov  cgc (x86-rbx) (x86-mem (* 8 17) (x86-rsp))) ;; get address of string
  (x86-mov  cgc (x86-rax) (x86-imm-int 0 0))

(x86-label cgc print_string_loop)
  (x86-mov  cgc (x86-al) (x86-mem 0 (x86-rbx))) ;; get next character
  (x86-cmp  cgc (x86-rax) (x86-imm-int 0 0))    ;; null byte?
  (x86-je   cgc print_string_loop_done)
  (x86-push cgc (x86-rax))
  (x86-call cgc putchar)
  (x86-inc  cgc (x86-rbx))
  (x86-jmp  cgc print_string_loop)

(x86-label cgc print_string_loop_done)
  (x86-pop  cgc (x86-r15)) ;; pop 15 registers (all registers except rsp)
  (x86-pop  cgc (x86-r14))
  (x86-pop  cgc (x86-r13))
  (x86-pop  cgc (x86-r12))
  (x86-pop  cgc (x86-r11))
  (x86-pop  cgc (x86-r10))
  (x86-pop  cgc (x86-r9))
  (x86-pop  cgc (x86-r8))
  (x86-pop  cgc (x86-rbp))
  (x86-pop  cgc (x86-rdi))
  (x86-pop  cgc (x86-rsi))
  (x86-pop  cgc (x86-rdx))
  (x86-pop  cgc (x86-rcx))
  (x86-pop  cgc (x86-rbx))
  (x86-pop  cgc (x86-rax))
  (x86-popf cgc) ;; pop flags register

  (x86-ret  cgc (* 8 1)) ;;  return and pop parameter
)

(define (x86-push-string cgc str)

  (define lbl (asm-make-label* cgc))

  (x86-call cgc lbl)
  (asm-string cgc str)
  (x86-label cgc lbl))

(define (x86-push-string-and-call cgc lbl str)
  (x86-push-string cgc str)
  (x86-call cgc lbl))

;;;============================================================================

(define (gen-print_word_hex cgc print_word_hex putchar print_string)

  (define print_word_hex_loop (asm-make-label cgc 'print_word_hex_loop))
  (define print_hex1          (asm-make-label cgc 'print_hex1))
  (define print_hex1_putchar  (asm-make-label cgc 'print_hex1_putchar))

;; The print_word_hex function sends a hexadecimal integer
;; representation of a word to the standard output (stdout).
;; The calling convention is to push to the stack the word (64 bits)
;; to write, and then to execute a "call print_word_hex".  When
;; control returns from the function, the stack and all the registers
;; will be back to the state they had before the word was pushed.

(x86-label cgc print_word_hex)
  (x86-pushf cgc)          ;; push flags register
  (x86-push cgc (x86-rax)) ;; push 15 registers (all registers except rsp)
  (x86-push cgc (x86-rbx))
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rbp))
  (x86-push cgc (x86-r8))
  (x86-push cgc (x86-r9))
  (x86-push cgc (x86-r10))
  (x86-push cgc (x86-r11))
  (x86-push cgc (x86-r12))
  (x86-push cgc (x86-r13))
  (x86-push cgc (x86-r14))
  (x86-push cgc (x86-r15))

  (x86-push-string-and-call cgc print_string "0x")

  (x86-mov  cgc (x86-rax) (x86-mem (* 8 17) (x86-rsp))) ;; get number to print
  (x86-mov  cgc (x86-rbx) (x86-imm-int 16 0))

(x86-label cgc print_word_hex_loop)
  (x86-rol  cgc (x86-rax) (x86-imm-int 4 0))
  (x86-push cgc (x86-rax))
  (x86-call cgc print_hex1)
  (x86-dec  cgc (x86-rbx))
  (x86-jne  cgc print_word_hex_loop)

  (x86-pop  cgc (x86-r15)) ;; pop 15 registers (all registers except rsp)
  (x86-pop  cgc (x86-r14))
  (x86-pop  cgc (x86-r13))
  (x86-pop  cgc (x86-r12))
  (x86-pop  cgc (x86-r11))
  (x86-pop  cgc (x86-r10))
  (x86-pop  cgc (x86-r9))
  (x86-pop  cgc (x86-r8))
  (x86-pop  cgc (x86-rbp))
  (x86-pop  cgc (x86-rdi))
  (x86-pop  cgc (x86-rsi))
  (x86-pop  cgc (x86-rdx))
  (x86-pop  cgc (x86-rcx))
  (x86-pop  cgc (x86-rbx))
  (x86-pop  cgc (x86-rax))
  (x86-popf cgc) ;; pop flags register

  (x86-ret  cgc (* 8 1)) ;;  return and pop parameter

(x86-label cgc print_hex1)
  (x86-push cgc (x86-rax))

  (x86-mov  cgc (x86-rax) (x86-mem (* 8 2) (x86-rsp))) ;; get hex digit to print
  (x86-and  cgc (x86-rax) (x86-imm-int #x0f 0))
  (x86-cmp  cgc (x86-rax) (x86-imm-int 10 0))
  (x86-jb   cgc print_hex1_putchar)
  (x86-add  cgc (x86-rax) (x86-imm-int 39 0)) ;; handle A-F
(x86-label cgc print_hex1_putchar)
  (x86-add  cgc (x86-rax) (x86-imm-int 48 0)) ;; digit -> ASCII
  (x86-push cgc (x86-rax))
  (x86-call cgc putchar)

  (x86-pop  cgc (x86-rax))
  (x86-ret  cgc (* 8 1)) ;;  return and pop parameter
)

;;;============================================================================

(define (gen-print_regs cgc print_regs print_string print_word_hex)

  (define flag_o  (asm-make-label cgc 'flag_o))
  (define flag_no (asm-make-label cgc 'flag_no))
  (define flag_b  (asm-make-label cgc 'flag_b))
  (define flag_ae (asm-make-label cgc 'flag_ae))
  (define flag_e  (asm-make-label cgc 'flag_e))
  (define flag_ne (asm-make-label cgc 'flag_ne))
  (define flag_be (asm-make-label cgc 'flag_be))
  (define flag_a  (asm-make-label cgc 'flag_a))
  (define flag_s  (asm-make-label cgc 'flag_s))
  (define flag_ns (asm-make-label cgc 'flag_ns))
  (define flag_p  (asm-make-label cgc 'flag_p))
  (define flag_np (asm-make-label cgc 'flag_np))
  (define flag_l  (asm-make-label cgc 'flag_l))
  (define flag_ge (asm-make-label cgc 'flag_ge))
  (define flag_le (asm-make-label cgc 'flag_le))
  (define flag_g  (asm-make-label cgc 'flag_g))

;; The print_regs function sends a hexadecimal integer representation
;; of the content of all the registers to the standard output (stdout).
;; The calling convention is to execute a "call print_regs".
;; When control returns from the function, the stack and all the registers
;; will be back to the state they had before the call was executed.

(x86-label cgc print_regs)

  (x86-pushf cgc)

  (x86-push-string-and-call cgc print_string "\n# flags=")

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-jo   cgc flag_o)
  (x86-push-string-and-call cgc print_string " no")
  (x86-jmp  cgc flag_no)
(x86-label cgc flag_o)
  (x86-push-string-and-call cgc print_string " o")
(x86-label cgc flag_no)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-jb   cgc flag_b)
  (x86-push-string-and-call cgc print_string " ae")
  (x86-jmp  cgc flag_ae)
(x86-label cgc flag_b)
  (x86-push-string-and-call cgc print_string " b")
(x86-label cgc flag_ae)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-je   cgc flag_e)
  (x86-push-string-and-call cgc print_string " ne")
  (x86-jmp  cgc flag_ne)
(x86-label cgc flag_e)
  (x86-push-string-and-call cgc print_string " e")
(x86-label cgc flag_ne)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-jbe  cgc flag_be)
  (x86-push-string-and-call cgc print_string " a")
  (x86-jmp  cgc flag_a)
(x86-label cgc flag_be)
  (x86-push-string-and-call cgc print_string " be")
(x86-label cgc flag_a)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-js   cgc flag_s)
  (x86-push-string-and-call cgc print_string " ns")
  (x86-jmp  cgc flag_ns)
(x86-label cgc flag_s)
  (x86-push-string-and-call cgc print_string " s")
(x86-label cgc flag_ns)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-jp   cgc flag_p)
  (x86-push-string-and-call cgc print_string " np")
  (x86-jmp  cgc flag_np)
(x86-label cgc flag_p)
  (x86-push-string-and-call cgc print_string " p")
(x86-label cgc flag_np)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-jl   cgc flag_l)
  (x86-push-string-and-call cgc print_string " ge")
  (x86-jmp  cgc flag_ge)
(x86-label cgc flag_l)
  (x86-push-string-and-call cgc print_string " l")
(x86-label cgc flag_ge)

  (x86-popf cgc)
  (x86-pushf cgc)
  (x86-jle   cgc flag_le)
  (x86-push-string-and-call cgc print_string " g")
  (x86-jmp  cgc flag_g)
(x86-label cgc flag_le)
  (x86-push-string-and-call cgc print_string " le")
(x86-label cgc flag_g)

  (x86-push-string-and-call cgc print_string "\n# rax=")
  (x86-push cgc (x86-rax))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rbx=")
  (x86-push cgc (x86-rbx))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rcx=")
  (x86-push cgc (x86-rcx))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rdx=")
  (x86-push cgc (x86-rdx))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rsi=")
  (x86-push cgc (x86-rsi))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rdi=")
  (x86-push cgc (x86-rdi))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rbp=")
  (x86-push cgc (x86-rbp))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rsp=")
  (x86-push cgc (x86-rax))
  (x86-mov  cgc (x86-rax) (x86-rsp))
  (x86-add  cgc (x86-rax) (x86-imm-int (* 8 3) 0))
  (x86-push cgc (x86-rax))
  (x86-call cgc print_word_hex)
  (x86-pop  cgc (x86-rax))

  (x86-push-string-and-call cgc print_string "\n# r8 =")
  (x86-push cgc (x86-r8))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r9 =")
  (x86-push cgc (x86-r9))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r10=")
  (x86-push cgc (x86-r10))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r11=")
  (x86-push cgc (x86-r11))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r12=")
  (x86-push cgc (x86-r12))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r13=")
  (x86-push cgc (x86-r13))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r14=")
  (x86-push cgc (x86-r14))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# r15=")
  (x86-push cgc (x86-r15))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# rip=")
  (x86-push cgc (x86-mem (* 8 1) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# [rsp+8*0]=")
  (x86-push cgc (x86-mem (* 8 2) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# [rsp+8*1]=")
  (x86-push cgc (x86-mem (* 8 3) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# [rsp+8*2]=")
  (x86-push cgc (x86-mem (* 8 4) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# [rsp+8*3]=")
  (x86-push cgc (x86-mem (* 8 5) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# [rsp+8*4]=")
  (x86-push cgc (x86-mem (* 8 6) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n# [rsp+8*5]=")
  (x86-push cgc (x86-mem (* 8 7) (x86-rsp)))
  (x86-call cgc print_word_hex)

  (x86-push-string-and-call cgc print_string "\n")

  (x86-popf cgc)
  (x86-ret  cgc 0)
)

;;;============================================================================

(define (gen-mmap cgc mmap)

  (define mmap_syscall_macos (asm-make-label cgc 'mmap_syscall_macos))
  (define mmap_syscall_linux (asm-make-label cgc 'mmap_syscall_linux))
  (define mmap_syscall       (asm-make-label cgc 'mmap_syscall))

;; The mmap function calls the OS to allocate a block of virtual
;; memory.  The block of memory will be readable, writable and
;; executable.  The calling convention is to push to the stack a word
;; (64 bits) containing the length of the block in bytes, and then to
;; execute a "call mmap".  When control returns from the function, the
;; stack will be back to the state it had before the character was
;; pushed and the register %rax will contain the address of the block.
;; The value -1 is returned when the block could not be allocated.

(x86-label cgc mmap)
  (x86-push cgc (x86-r11)) ;; push the 10 registers modified
  (x86-push cgc (x86-r10))
  (x86-push cgc (x86-r9))
  (x86-push cgc (x86-r8))
  (x86-push cgc (x86-rbp))
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-rbx))

  ;; determine if OS is linux or macOS

  (x86-mov  cgc (x86-rdi) (x86-imm-int -1 0)) ;; parameter of system call is -1
  (x86-mov  cgc (x86-rax) (x86-imm-int 13 0)) ;; system call 13 is "rt_sigaction" on linux and a noop on macOS
  (x86-syscall cgc)                           ;; perform system call to OS

  (x86-cmp  cgc (x86-rax) (x86-imm-int 0 0))  ;; negative means error which means linux
  (x86-js   cgc mmap_syscall_linux)

(x86-label cgc mmap_syscall_macos)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x20000c5 0)) ;; "mmap" system call is 0x20000c5
  (x86-mov  cgc (x86-rdx) (x86-imm-int 7 0))         ;; prot = PROT_READ | PROT_WRITE | PROT_EXEC
  (x86-mov  cgc (x86-r10) (x86-imm-int 4098 0))      ;; flags = MAP_PRIVATE | MAP_ANON
  (x86-jmp  cgc mmap_syscall)

(x86-label cgc mmap_syscall_linux)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x0000009 0)) ;; "mmap" system call is 0x0000009
  (x86-mov  cgc (x86-rdx) (x86-imm-int 7 0))         ;; prot = PROT_READ | PROT_WRITE | PROT_EXEC
  (x86-mov  cgc (x86-r10) (x86-imm-int 34 0))        ;; flags = MAP_PRIVATE | MAP_ANON

(x86-label cgc mmap_syscall)
  (x86-mov  cgc (x86-rdi) (x86-imm-int 0 0))            ;; address (0 means OS chooses)
  (x86-mov  cgc (x86-rsi) (x86-mem (* 8 11) (x86-rsp))) ;; number of bytes to allocate
  (x86-mov  cgc (x86-r8) (x86-imm-int -1 0))            ;; fd
  (x86-mov  cgc (x86-r9) (x86-imm-int 0 0))             ;; offset
  (x86-syscall cgc)                                     ;; perform system call to OS

  (x86-pop  cgc (x86-rbx)) ;; pop the 10 registers modified
  (x86-pop  cgc (x86-rcx))
  (x86-pop  cgc (x86-rdx))
  (x86-pop  cgc (x86-rdi))
  (x86-pop  cgc (x86-rsi))
  (x86-pop  cgc (x86-rbp))
  (x86-pop  cgc (x86-r8))
  (x86-pop  cgc (x86-r9))
  (x86-pop  cgc (x86-r10))
  (x86-pop  cgc (x86-r11))

  (x86-ret  cgc (* 8 1)) ;;  return and pop parameter
)

;;;============================================================================

(define (gen-exit cgc exit)

  (define exit_syscall_macos (asm-make-label cgc 'exit_syscall_macos))
  (define exit_syscall_linux (asm-make-label cgc 'exit_syscall_linux))
  (define exit_syscall       (asm-make-label cgc 'exit_syscall))

;; The exit function calls the OS to terminate the process with an
;; exit code.  The calling convention is to push to the stack a word
;; (64 bits) containing the exit code, and then to execute a "call
;; exit".  This function does not return.

(x86-label cgc exit)

  ;; determine if OS is linux or macOS

  (x86-mov  cgc (x86-rdi) (x86-imm-int -1 0)) ;; parameter of system call is -1
  (x86-mov  cgc (x86-rax) (x86-imm-int 13 0)) ;; system call 13 is "rt_sigaction" on linux and a noop on macOS
  (x86-syscall cgc)                           ;; perform system call to OS

  (x86-cmp  cgc (x86-rax) (x86-imm-int 0 0))  ;; negative means error which means linux
  (x86-js   cgc exit_syscall_linux)

(x86-label cgc exit_syscall_macos)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x2000001 0)) ;; "exit" system call is 0x2000001
  (x86-jmp  cgc exit_syscall)

(x86-label cgc exit_syscall_linux)
  (x86-mov  cgc (x86-rax) (x86-imm-int #x000003c 0)) ;; "exit" system call is 0x000003c

(x86-label cgc exit_syscall)
  (x86-mov  cgc (x86-rdi) (x86-mem (* 8 1) (x86-rsp))) ;; exit code
  (x86-syscall cgc)                                    ;; perform system call to OS

  (x86-jmp  cgc exit) ;; the syscall should not return but if it does just loop
)

;;;============================================================================

(define (gen-rts cgc)

  (define putchar        (x86-global-label cgc 'putchar))
  (define getchar        (x86-global-label cgc 'getchar))
  (define print_string   (x86-global-label cgc 'print_string))
  (define print_word_hex (x86-global-label cgc 'print_word_hex))
  (define print_regs     (x86-global-label cgc 'print_regs))
  (define mmap           (x86-global-label cgc 'mmap))
  (define exit           (x86-global-label cgc 'exit))

  ;; use known dependencies to determine which routines need to be generated

  (if (and (not print_word_hex)
           print_regs)
      (set! print_word_hex (asm-make-label cgc 'print_word_hex)))

  (if (and (not print_string)
           (or print_word_hex print_regs))
      (set! print_string (asm-make-label cgc 'print_string)))

  (if (and (not putchar)
           (or print_string print_word_hex print_regs))
      (set! putchar (asm-make-label cgc 'putchar)))

  (if putchar        (gen-putchar cgc putchar))
  (if getchar        (gen-getchar cgc getchar))
  (if print_string   (gen-print_string cgc print_string putchar))
  (if print_word_hex (gen-print_word_hex cgc print_word_hex putchar print_string))
  (if print_regs     (gen-print_regs cgc print_regs print_string print_word_hex))
  (if mmap           (gen-mmap cgc mmap))
  (if exit           (gen-exit cgc exit)))

;;;============================================================================
