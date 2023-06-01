;;; This is an implementation of the Ribbit Virtual Machine as an
;;; x86-32 program for execution on linux. The NASM assembler must be
;;; used to create an executable program.  See the asm2exe.sh script.
;;;
;;; It uses a very compact ELF header which is explained in the
;;; delightful article:
;;;
;;; A Whirlwind Tutorial on Creating Really Teensy ELF Executables for Linux
;;; by Brian Raiter.
;;; http://muppetlabs.com/~breadbox/software/tiny/teensy.html
;;;
;;; We use the 91 byte version for simplicity, but the article
;;; explains a 45 byte version!

;;; ######################################## Beginning of ELF header
%macro DB_PRINT 1
    push %1
    call print_int
%ifndef NEED_PRINT_INT
%define NEED_PRINT_INT
%endif
%endmacro


%macro DB_PRINT_RIB 2
    push %2 ;; depth
    push %1
    call print_rib
    push eax
    mov eax, 0x0a ;; newline
    call putchar
    pop eax
%ifndef NEED_PRINT_RIB
%define NEED_PRINT_RIB
%endif
%endmacro

	BITS 32

        org     0x08048000

ehdr:                                                 ; Elf32_Ehdr
              db      0x7F, "ELF", 1, 1, 1, 0         ;   e_ident
      times 8 db      0
              dw      2                               ;   e_type
              dw      3                               ;   e_machine
              dd      1                               ;   e_version
              dd      _start                          ;   e_entry
              dd      phdr - $$                       ;   e_phoff
              dd      0                               ;   e_shoff
              dd      0                               ;   e_flags
              dw      ehdrsize                        ;   e_ehsize
              dw      phdrsize                        ;   e_phentsize
              dw      1                               ;   e_phnum
              dw      0                               ;   e_shentsize
              dw      0                               ;   e_shnum
              dw      0                               ;   e_shstrndx

ehdrsize      equ     $ - ehdr

phdr:                                                 ; Elf32_Phdr
              dd      1                               ;   p_type
              dd      0                               ;   p_offset
              dd      $$                              ;   p_vaddr
              dd      $$                              ;   p_paddr
              dd      filesize                        ;   p_filesz
              dd      filesize                        ;   p_memsz
              dd      5                               ;   p_flags
              dd      0x1000                          ;   p_align

phdrsize      equ     $ - phdr


_start:

;;; ######################################## Beginning of RVM


;%define DEBUG
;%define DEBUG_GC
;%define DEBUG_INSTR


%if 0
%define DEBUG
%define DEBUG_INSTR
%define DEBUG_PRIM
%endif

%if 0 ;; @@(replace "0" "1")@@
%define RVM_GEN
%endif

%ifdef DEBUG
%define NEED_PRINT_REGS
%endif

%define WORD_SIZE       4
%define RIB_SIZE_WORDS  4
%define HEAP_SIZE_RIBS  10000
%define HEAP_SIZE (HEAP_SIZE_RIBS*RIB_SIZE_WORDS*WORD_SIZE)

%define SYS_EXIT        1
%define SYS_READ        3
%define SYS_WRITE       4

%define SYS_OPEN        5
%define O_RDONLY    0
%define O_WRONLY    1
%define O_RDWR      2
%define O_CREAT     64     ; octal 0100
%define O_EXCL      200    ; octal 0200
%define O_NOCTTY    400    ; octal 0400
%define O_TRUNC     1000   ; octal 01000
%define O_APPEND    2000   ; octal 02000
%define O_NONBLOCK  4000   ; octal 04000
%define O_SYNC      101000 ; octal 04010000
%define O_ASYNC     20000  ; octal 020000

%define SYS_MMAP        90
%define MAP_PRIVATE     2
%define MAP_ANONYMOUS   32

%define PROT_READ       0x1
%define PROT_WRITE      0x2

%define CALL_KERNEL     int 0x80

%define STDIN  0
%define STDOUT 1

;;; register assignment
%define heap_base    ebp
%define heap_alloc   edi
%define rvm_code_ptr esi
%define pc           esi
%define stack        ecx

;;; representation of fixnums and ribs (FIX_TAG can be 0 or 1)
%define FIX_TAG 1
%define FIX(n)  ((n)*2+FIX_TAG)
%define RIB_TAG (1-FIX_TAG)

;;; type codes of Scheme objects
%define PAIR_TYPE       0
%define PROCEDURE_TYPE  1
%define SYMBOL_TYPE     2
%define STRING_TYPE     3
%define VECTOR_TYPE     4
%define SINGLETON_TYPE  5

;;; first 4 ribs of heap are preallocated for #f, #t, (), and "rib" procedure
%define FALSE    heap_base
%define TRUE     heap_base+WORD_SIZE*RIB_SIZE_WORDS*1
%define NIL      heap_base+WORD_SIZE*RIB_SIZE_WORDS*2
%define RIB_PROC heap_base+WORD_SIZE*RIB_SIZE_WORDS*3
%define PREALLOCATED_RIBS 4

%define SYMBOL_TABLE FIELD1(RIB_PROC)
%define TEMP0        FIELD0(TRUE)
%define TEMP1        FIELD1(TRUE)
%define TEMP2        FIELD0(NIL)
%define TEMP3        FIELD1(NIL)

%define FIELD0(x) [x-RIB_TAG+WORD_SIZE*0]
%define FIELD1(x) [x-RIB_TAG+WORD_SIZE*1]
%define FIELD2(x) [x-RIB_TAG+WORD_SIZE*2]
%define FIELD3(x) [x-RIB_TAG+WORD_SIZE*3]

;;; the RVM encodes instructions with codes in the range 0 .. MAX_CODE
%define MAX_CODE 91

%define INSTR_JUMP_CALL  0
%define INSTR_SET        1
%define INSTR_GET        2
%define INSTR_CONST      3
%define INSTR_CONST_PROC 3
%define INSTR_IF         4
%define INSTR_HALT       5

%define SHORT_JUMP       20
%define SHORT_CALL       30
%define SHORT_SET        0
%define SHORT_GET        10
%define SHORT_CONST      11
%define SHORT_CONST_PROC 4

%macro movC 2
%if %2 == 0
	xor  %1, %1
%elif %2 == -1
	xor  %1, %1
	dec  %1
%elif %2 == 1
	xor  %1, %1
	inc  %1
%elif %2 >= -128 && %2 <= 127
	push %2
	pop  %1
%else
	mov  %1, %2
%endif
%endmacro


;;;;;;;;; GC SPECIFICS ;;;;;

%define BROKEN_HEART       0 ;; Broken heart is set as the null pointer

%define SAFE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

alloc_heap:

;;; allocate fixed size heap using mmap

	push 0					; mmap offset
	push -1					; mmap fd
	push MAP_ANONYMOUS + MAP_PRIVATE	; mmap flags
	push PROT_READ + PROT_WRITE 		; mmap prot
	push HEAP_SIZE 	; mmap length
	push 0				    	; mmap addr
	mov  ebx, esp
	movC eax, SYS_MMAP
	CALL_KERNEL
;	add  esp, WORD_SIZE*6		; OK to leave garbage on stack

	mov  heap_base, eax		; save heap base for convenient access

    ; set end-of-heap for each of the heaps (stop-and-copy)
    mov   FIELD1(heap_base+HEAP_SIZE/2), eax
    add   eax, HEAP_SIZE/2
    mov   FIELD1(heap_base), eax

    ;mov  FIELD1((heap_base+HEAP_SIZE)-(RIB_SIZE_WORDS*WORD_SIZE)), eax
    ;add  eax, HEAP_SIZE/2
    ;mov  FIELD1((heap_base+HEAP_SIZE/2)-(RIB_SIZE_WORDS*WORD_SIZE)), eax

%ifdef SAFE

;;; check if returned address is valid (multiple of 4096)

    mov  eax, heap_base
	and  ax, 4095
	jz   init_heap_call
    

%ifdef DEBUG
	push heap_error_msg
	call print_string
%endif

	movC ebx, 1			; return error code 1
	movC eax, SYS_EXIT
	CALL_KERNEL

%ifdef DEBUG
heap_error_msg:	db "*** ERROR -- could not allocate heap",0x0a,0
%endif

init_heap_call:
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    call init_heap

	lea  eax, [TRUE]
	mov  FIELD0(FALSE), eax
	lea  eax, [NIL]
	mov  FIELD1(FALSE), eax

	mov  SYMBOL_TABLE, eax		; init symbol table to NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG
	push rvm_code
	call print_string
	mov  al, 0x0a
	call putchar
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%macro get_byte 0
	mov  al, [rvm_code_ptr]
	inc  rvm_code_ptr
%ifdef DEBUG
	push eax
	call putchar
	pop  eax
%endif
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

build_symbol_table:
	mov  rvm_code_ptr, rvm_code
	movC eax, 0	 	; start accumulating at 0
	call get_int
	mov  edx, eax		; edx = number of anonymous symbols to create
	jmp  build_symbol_table_loop1_start

build_symbol_table_loop1:
	lea  eax, [NIL]		; symbol name = empty
	movC stack, 0		; symbol name length = 0
	call alloc_symbol	; create symbol and add it to symbol table
build_symbol_table_loop1_start:
	dec  edx
	jns  build_symbol_table_loop1

build_symbol_table_loop2:
	lea  stack, [NIL]	; character accumulator
	movC edx, 0		; edx = number of characters accumulated

build_symbol_table_loop3:
	movC eax, 0
	get_byte		; get next character
	cmp  al, 44		; "," ?
	je   build_symbol_table_symbol
	cmp  al, 59		; ";" ?
	je   build_symbol_table_symbol

	shl  eax, 1		; convert character to fixnum
%if FIX_TAG != 0
	inc  eax
%endif
	push FIX(PAIR_TYPE)
    ;DB_PRINT(7)
	call alloc_rib		; stack_register <- [eax, stack_register, PAIR_TYPE]
	inc  edx		; increment character count
	jmp  build_symbol_table_loop3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

get_code:
	movC eax, 0
	get_byte
	sub  al, 35
	jae  get_code_done
	mov  al, 57
get_code_done:
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

get_int_loop:
	sub  eax, (MAX_CODE+1)/2
get_int:
	push edx
	movC edx, (MAX_CODE+1)/2
	mul  edx
	mov  edx, eax
	call get_code
	add  edx, eax
	sub  al, (MAX_CODE+1)/2
	mov  eax, edx
	pop  edx
	jae  get_int_loop
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


init_heap:

%if RIB_TAG != 0
	inc  heap_base
%endif
    ;; add one rib padding for informations like 
    ;;  other rib location and scan_ptr buffer
    add  heap_base, RIB_SIZE_WORDS*WORD_SIZE 
	mov  heap_alloc, heap_base
	push FIX(SINGLETON_TYPE)
	pop  eax
	mov  cl, PREALLOCATED_RIBS * RIB_SIZE_WORDS
init_heap_loop1:
	mov  [heap_alloc-RIB_TAG], eax
	add  heap_alloc, WORD_SIZE
	dec  cl
	jne  init_heap_loop1

    ;; one rib is reserved to store the information about the to_space pointer 
	movC ecx, HEAP_SIZE_RIBS/2 - PREALLOCATED_RIBS - 1
	mov  ebx, heap_alloc
init_heap_loop2:
	add  ebx, WORD_SIZE * RIB_SIZE_WORDS
	mov  FIELD0(heap_alloc), ebx
	mov  FIELD3(heap_alloc), eax
    mov  heap_alloc, ebx
	dec  ecx
	jne  init_heap_loop2
    mov  dword FIELD0(heap_alloc-RIB_SIZE_WORDS * WORD_SIZE), 0x0 ;; mark the end

    mov  heap_alloc, heap_base
    add  heap_alloc, PREALLOCATED_RIBS*RIB_SIZE_WORDS * WORD_SIZE
	;mov  heap_alloc, ebx
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


alloc_rib:

;;; Creates a rib containing:
;;;   field 0 = eax
;;;   field 1 = stack_register
;;;   field 2 = last value pushed on stack before call
;;; The stack_register will be a reference to the allocated rib.
;;; If the rib was the last free rib, a garbage collection is initiated.
;;; When the control returns to the caller the heap has at least
;;; one free rib.
	mov  FIELD1(heap_alloc), stack	; store field 1
	mov  stack, [esp+WORD_SIZE*1]
	mov  FIELD2(heap_alloc), stack	; store field 2
	mov  stack, FIELD0(heap_alloc)	; remember next rib
	mov  FIELD0(heap_alloc), eax	; store field 0
	xchg heap_alloc, stack
	cmp  heap_alloc, 0	; last free rib?
	jne  alloc_rib_done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

gc:
%define scan_ptr eax
%ifdef DEBUG_GC
    call print_gc_starting
%endif
    push ebx
	mov  TEMP0, stack
	mov  TEMP1, pc
    push heap_base
    mov  heap_base, FIELD1(heap_base-RIB_SIZE_WORDS*WORD_SIZE)
    call init_heap ;; clean up the to_space
    ;mov  ecx, PREALLOCATED_RIBS*RIB_SIZE_WORDS
    pop  esi
    mov  heap_alloc, heap_base
    mov  scan_ptr, heap_base
    sub  scan_ptr, WORD_SIZE ;; start the stack pointer a little bit outside for simplicity
    ;; But it's fine because 
    ;jmp  scan_copy
scan_copy:
    ;; If its a rib
    movC  ecx, 4
    mov   ebx, heap_alloc
    rep  movsd
    mov dword FIELD0(esi-RIB_SIZE_WORDS*WORD_SIZE), 0x0
    mov FIELD1(esi-RIB_SIZE_WORDS*WORD_SIZE), ebx 
    mov [scan_ptr], ebx
    ;jmp scan

    ;movC ecx, 4
    ;rep movsd ;; take only 2 bytes !
scan:
    add scan_ptr, WORD_SIZE
    cmp scan_ptr, heap_alloc
    je  scan_end
    mov  esi, [scan_ptr]
    ;shr  esi, 1 don't work because we need esi afterwords for the copy
    test esi, 1  ;; Test if its tagged
    %if RIB_TAG==0
    JNZ  scan
    %else
    JZ scan
    %endif 
    ;cmp dword [esi], 0
    mov ebx, [esi]
    test ebx, ebx
    jz   scan_BH ;; is a broken heart
    jmp scan_copy

scan_BH:
    mov esi, FIELD1(esi)
    mov [scan_ptr], esi
    jmp scan
scan_end:
	mov  stack, TEMP0 
	mov  pc, TEMP1 
    mov  dword TEMP0, 0xb
    mov  dword TEMP1, 0xb
    pop  ebx

%ifdef DEBUG_GC
    call print_gc_end
%endif

alloc_rib_done:
	ret  WORD_SIZE*1

heap_overflow:

%ifdef DEBUG
	push heap_overflow_msg
	call print_string
%endif

	movC ebx, 2			; return error code 2
	movC eax, SYS_EXIT
	CALL_KERNEL

%ifdef DEBUG
heap_overflow_msg:	db "*** ERROR -- heap overflow",0x0a,0
%endif

%ifdef DEBUG_GC
gc_starting_msg: db "*** STARTING GC...",0x0a,0
gc_end_msg_1: db "*** ENDING GC : cleaned  ",0
gc_end_msg_2: db " ribs, ",0
gc_end_msg_3: db " ribs left",0x0a,0

print_gc_starting:
    push gc_starting_msg
    call print_string
    ret

print_gc_end:
    push gc_end_msg_1
    call print_string
    
    push HEAP_SIZE/2
    add  [esp], ebp
    sub  [esp], edi
    shr  dword [esp], 4 ;; must change if a rib is not on 16 bits
    call print_int

    push gc_end_msg_2
    call print_string

    push edi
    sub  [esp], ebp
    shr  dword [esp], 4
    call print_int

    push gc_end_msg_3
    call print_string
    ret

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%ifndef NEED_PRINT_INT
%define NEED_PRINT_INT
%endif

%ifndef NEED_PRINT_STRING
%define NEED_PRINT_STRING
%endif

%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

alloc_symbol:

;;; Creates a symbol from the list of characters in its name
;;; eax = character list, stack_register = character count

	shl  stack, 1		; convert string's length to fixnum
%if FIX_TAG != 0
	inc  stack
%endif
	push FIX(STRING_TYPE)
	call alloc_rib		; create symbol's name (as a string)
	mov  eax, FALSE		; global variable's value
	push FIX(SYMBOL_TYPE)
	call alloc_rib		; create symbol
	push FIX(PAIR_TYPE)
	mov  eax, stack		; add it to symbol table
	mov  stack, SYMBOL_TABLE
	call alloc_rib		; create pair at head of symbol table
	mov  SYMBOL_TABLE, stack

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_global:
	mov  eax, FIELD0(edx)
	mov  FIELD0(eax), stack
	mov  edx, FIELD1(edx)
	lea  stack, [stack+WORD_SIZE*RIB_SIZE_WORDS]
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

build_symbol_table_symbol:
	mov  ebx, eax
	mov  eax, stack
	mov  stack, edx
	call alloc_symbol	; eax = character list, stack_register = character count
	cmp  bl, 59		; ";" ?
	jne  build_symbol_table_loop2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_globals:

;;; Initializes global variables "rib", "false", "true", and "nil"

	movC edx, FIX(0)	; create "rib" primitive (always at index 0)
	mov  FIELD0(RIB_PROC), edx
	movC edx, FIX(PROCEDURE_TYPE)
	mov  FIELD2(RIB_PROC), edx

	mov  edx, SYMBOL_TABLE
	lea  stack, [RIB_PROC]
	call init_global	; set "rib"
	mov  stack, FALSE
	call init_global	; set "false"
	call init_global	; set "true"
	call init_global	; set "nil"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

decompress:
	movC stack, FALSE	; stack <- #f
	jmp  decompress_loop

decompress_jump:
	push eax
	movC eax, FIX(INSTR_IF)	; in case this is an if instruction
	push FIX(0)
	call alloc_rib		; stack_register <- [fixnum INSTR_IF, stack_register, fixnum 0]
	pop  eax
	jmp  decompress_instr

decompress_create_instr_const_proc:
	dec  dh			; convert to INSTR_CONST

decompress_create_instr:
	push eax		; push operand
	movC eax, 0
	mov  al, dh
	shl  eax, 1		; convert operator to fixnum
%if FIX_TAG != 0
	inc  eax
%endif
	call alloc_rib		; stack_register <- [operator, stack_register, operand]

decompress_if:
decompress_append_instr:
	mov  eax, stack
	mov  stack, FIELD1(eax)
	mov  edx, FIELD2(eax)
	mov  FIELD1(eax), edx
	mov  edx, FIELD2(stack)
	mov  FIELD2(eax), edx
	mov  FIELD2(stack), eax

%ifdef DEBUG

	push eax
	movC eax, 32
	call putchar
	pop  eax
	push eax
	call print_word_hex
	push eax
	movC eax, 58
	call putchar
	movC eax, 32
	call putchar
	pop  eax
	push dword FIELD0(eax)
	call print_word_hex
	push eax
	movC eax, 32
	call putchar
	pop  eax
	push dword FIELD1(eax)
	call print_word_hex
	push eax
	movC eax, 32
	call putchar
	pop  eax
	push dword FIELD2(eax)
	call print_word_hex
	push eax
	movC eax, 0x0a
	call putchar
	pop  eax

%endif

decompress_loop:
	call get_code		; eax <- next code
	cmp  al, MAX_CODE
	je   decompress_if
	mov  edx, eax		; side effect: sets dh to 0 and dl to al
	sub  al, SHORT_JUMP+3
	jb   decompress_jump

decompress_not_jump:
	mov  dl, al
	sub  al, SHORT_CALL+3
	jb   decompress_instr
	inc  dh
	mov  dl, al
	sub  al, SHORT_SET+3
	jb   decompress_instr
	inc  dh
	mov  dl, al
	sub  al, SHORT_GET+3
	jb   decompress_instr
	inc  dh
	mov  dl, al
	sub  al, SHORT_CONST+3
	jb   decompress_instr
	inc  dh
	mov  dl, al
	sub  al, SHORT_CONST_PROC+3

decompress_instr:
	add  al, 3
	jns  decompress_long 

decompress_short:
	movC eax, 0	 	; eax <- dl
	mov  al, dl
	cmp  dh, 2
	jae  decompress_short_int
	jmp  decompress_short_symbol

decompress_long:
	jne   decompress_long_symbol

decompress_long_int:
	movC eax, 0	 	; start accumulating at 0
	call get_int

decompress_short_int:
decompress_int:
	shl  eax, 1		; convert to fixnum
%if FIX_TAG != 0
	inc  eax
%endif
	jmp  decompress_opnd_done

decompress_long_symbol:
	dec  al			; start accumulating at 0 or 1
	call get_int


decompress_short_symbol:
decompress_symbol:
	mov  ebx, eax
	mov  eax, SYMBOL_TABLE
	jmp  decompress_symbol_loop_start
decompress_symbol_loop:
	mov  eax, FIELD1(eax)	; advance in the list of symbols
decompress_symbol_loop_start:
	dec  ebx
	jns  decompress_symbol_loop
decompress_symbol_done:
	mov  eax, FIELD0(eax)	; extract symbol

decompress_opnd_done:
	cmp  dh, INSTR_CONST_PROC+1
	jne  decompress_create_instr

decompress_create_proc:
	mov  FIELD0(stack), eax
	mov  eax, stack
	push FIX(PROCEDURE_TYPE)
	call alloc_rib		; stack_register <- [stack_register, stack_register, PROCEDURE_TYPE]
	mov  eax, stack
	mov  stack, FIELD1(stack)
	mov  FIELD1(eax), FALSE
	mov  ebx, stack
	mov  stack, FIELD1(stack)
	mov  FIELD1(ebx), FALSE
	cmp  stack, FALSE
	jne  decompress_create_instr_const_proc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_stack_and_pc:

;;; Initializes stack and pc registers

	mov  stack, eax
	mov  FIELD0(stack), FALSE
	mov  FIELD2(stack), ebx

	push FIX(INSTR_HALT)
	pop  dword FIELD0(ebx)
	mov  pc, FIELD2(ebx)
	mov  FIELD2(ebx), FALSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

run:

;;; Run the decompressed RVM code

	jmp  run_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define NBARGS(n) mov bl, n

%macro POP_STACK 0
	mov  stack, FIELD1(stack)
%endmacro

%macro POP_STACK_TO 1
	mov  %1, FIELD0(stack)
	POP_STACK
%endmacro


%define RESULT   eax
%define LAST_ARG RESULT
%define PREV_ARG edx

%define TOP_OF_STACK FIELD0(stack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_INSTR
string_jump	db "jump --",0
string_call	db "call --",0
%endif

; @@(feature arity-check
string_arity_error db "Arity check error",0x0a,0
string_test_rest db "Test rest params",0x0a,0
%ifndef NEED_PRINT_STRING
%define NEED_PRINT_STRING
%endif
; )@@

run_instr_jump_call:
	mov  eax, FIELD0(edx)	; eax = procedure to call

%ifdef DEBUG_INSTR
	cmp  dword FIELD2(pc), FIX(PAIR_TYPE)	; jump? (tail call)
	je   print_jump
print_call:
	push string_call
	call print_string
	jmp  print_jump_call_done
print_jump:
	push string_jump
	call print_string
print_jump_call_done:
    DB_PRINT_RIB eax, 4
%endif

    ; @@(feature arity-check
    POP_STACK_TO(edx)
    mov TEMP3, edx
    ;shr edx, 1
    ;push edx ;; push number of arguments
    ;mov  TEMP3, edx 
    ; )@@
	mov  edx, FIELD0(eax)	; edx = field0 of procedure (int or rib)
	shr  edx, 1
%if FIX_TAG == 0
	jnc  is_primitive
%else
	jc   is_primitive
%endif

is_closure:
    
    ;DB_PRINT(999)
	push eax
	call alloc_rib		; stack_register <- [closure, stack_register, closure]
	mov  eax, stack
	POP_STACK
	mov  TEMP2, eax		; remember the continuation rib
	mov  edx, FIELD0(eax)
	;mov  edx, FIELD0(eax)
	mov  FIELD1(eax), edx
	mov  edx, FIELD0(edx)
	mov  edx, FIELD0(edx)	; get nparams

    ; @@(feature arity-check
    mov  ebx, TEMP3
    shr  ebx, 1 ;; remove tag 
    ; )@@

    shr  edx, 2 ;; remove tagging and rest param
    jc  with_rest ; @@(feature rest-param)@@
    ; @@(feature arity-check (use exit)
no_rest:
    cmp  edx, ebx 
	je   create_frame_loop_start ;; pass arity-check
    ; @@(feature rest-param (use arity-check)
    jmp  error_arity_check
with_rest:
    sub   ebx, edx
	jge   rest_loop_prepare ;; pass arity-check
    ; )@@
error_arity_check:
    push string_arity_error
    call print_string
    call prim_exit
    ; )@@
	jmp  create_frame_loop_start

; @@(feature rest-param (use arity-check)
rest_loop_prepare:
    push edx ;; save edx
    push eax ;; save eax
    lea  eax, [NIL]
    mov  edx, ebx
    jmp  rest_loop_start
rest_frame_loop:
	mov  TEMP3, eax		; remember the frame's head
	POP_STACK_TO(eax)
	push FIX(PAIR_TYPE)
    ;DB_PRINT(1)
	call alloc_rib		; stack_register <- [arg, stack_register, PAIR_TYPE]
	mov  eax, stack
	POP_STACK
	mov  ebx, TEMP3
	mov  FIELD1(eax), ebx
rest_loop_start:
	dec  edx
	jns  rest_frame_loop
	push FIX(PAIR_TYPE)
    ;DB_PRINT(2)
    call alloc_rib ;; push result to stack
    pop eax
    pop edx
    inc edx 
    jmp create_frame_loop_start
; )@@
create_frame_loop:
	mov  TEMP3, eax		; remember the frame's head
	POP_STACK_TO(eax)
	push FIX(PAIR_TYPE)

    ;DB_PRINT(3)
	call alloc_rib		; stack_register <- [arg, stack_register, PAIR_TYPE]
	mov  eax, stack
	POP_STACK
	mov  ebx, TEMP3
	mov  FIELD1(eax), ebx
create_frame_loop_start:
	dec  edx
	jns  create_frame_loop
	mov  edx, TEMP2	      ; get continuation rib
	cmp  dword FIELD2(pc), FIX(PAIR_TYPE)	; jump? (tail call)
	je   jump_closure

call_closure:
	mov  FIELD0(edx), stack
	mov  pc, FIELD2(pc)
	mov  FIELD2(edx), pc
	jmp  activate_closure

jump_closure_pop_frame_loop:
	POP_STACK
jump_closure:
	cmp  dword FIELD2(stack), FIX(PAIR_TYPE)
	je   jump_closure_pop_frame_loop

	mov  ebx, FIELD0(stack)
	mov  FIELD0(edx), ebx
	mov  ebx, FIELD2(stack)
	mov  FIELD2(edx), ebx

activate_closure:
	mov  stack, eax		; set stack to frame of callee
	mov  pc, TEMP2
	mov  pc, FIELD1(pc)
	mov  pc, FIELD0(pc)
	mov  TEMP2, FALSE
	mov  TEMP3, FALSE

	jmp  run_next

is_primitive:
%ifdef DEBUG_PRIM
	push stack
	call print_list
%endif
	push dword prim_ret ; primitive will return to prim_ret
	push dword [prim_dispatch_table+edx*4]
	mov  LAST_ARG, FIELD0(stack)	; get last arg
	mov  ebx, FIELD1(stack)
	mov  PREV_ARG, FIELD0(ebx)	; get arg before last
	NBARGS(2)			; expect 2 arguments
	ret 				; jump to primitive

prim_ret:
	cmp  dword FIELD2(pc), FIX(PAIR_TYPE)	; jump? (tail call)
	je   jump_prim

call_prim:
	dec  bl
	js   push_result
	POP_STACK
	dec  bl
	js   push_result
	POP_STACK
	dec  bl
	js   push_result
	POP_STACK
	jmp  push_result

jump_prim_pop_frame_loop:
	POP_STACK
jump_prim:
	cmp  dword FIELD2(stack), FIX(PAIR_TYPE)
	je   jump_prim_pop_frame_loop

	mov  pc, stack			; caller's next instruction
	mov  stack, FIELD0(stack)	; set stack to caller's frame

	jmp  push_result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_INSTR
string_get:	db "get --",0
%endif

run_instr_get:
	mov  eax, FIELD0(edx)

%ifdef DEBUG_INSTR
	push string_get
	call print_string
    DB_PRINT_RIB eax, 3
%endif

push_result:
	push FIX(PAIR_TYPE)
    ;DB_PRINT(4)
	call alloc_rib

run_next:
	mov  pc, FIELD2(pc)

	;; fallthrough

run_loop:
	mov  eax, FIELD0(pc)
	shr  eax, 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cmp  al, INSTR_CONST
	jae  run_instr_no_mem_operand

;;; The instruction has a memory operand (i.e. a rib).
;;; The rib will be either a symbol or a stack cell.

	mov  edx, FIELD1(pc)
	test dl, 1
%if RIB_TAG == 0
	je   got_opnd
%else
	jne  got_opnd
%endif

	;; loop to find the cell on the stack
	mov  ebx, edx
	shr  ebx, 1
	mov  edx, stack
	jmp  stack_opnd_loop_start
stack_opnd_loop:
	mov  edx, FIELD1(edx)
stack_opnd_loop_start:
	dec  ebx
	jns  stack_opnd_loop

got_opnd:
	;;  operand is in edx
	cmp  al, INSTR_SET
	ja   run_instr_get
	jne  run_instr_jump_call

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

run_instr_set:

	POP_STACK_TO(eax)

%ifdef DEBUG_INSTR
	push string_set
	call print_string
    DB_PRINT_RIB eax, 3
%endif

	mov  FIELD0(edx), eax

	jmp  run_next

%ifdef DEBUG_INSTR
string_set:	db "set --",0x0a,0
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

run_instr_const:
	mov  eax, FIELD1(pc)

%ifdef DEBUG_INSTR
	push string_const
	call print_string
    DB_PRINT_RIB eax, 3
%endif

	jmp  push_result

%ifdef DEBUG_INSTR
string_const	db "const --",0
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_INSTR
string_if:	db "if --",0
%endif

run_instr_no_mem_operand:
	je   run_instr_const
	cmp  al, INSTR_IF
	jne  run_instr_halt

run_instr_if:
	mov  eax, FIELD0(stack)

%ifdef DEBUG_INSTR
	push string_if
	call print_string
    DB_PRINT_RIB eax, 3
%endif
	mov  stack, FIELD1(stack)
	cmp  eax, FALSE
	je   run_next

	mov  pc, FIELD1(pc)
	jmp  run_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @@(feature scm2str
;; assumes that LAST_ARG contains the list of number (ascii)
%define scm2str_STOP 0x01020300
scm2str:
    pop ecx
    mov ebx, FIELD1(LAST_ARG)
    ;DB_PRINT_RIB LAST_ARG , 3
    shr ebx, 3 ;; remove tagging and align on multiple of 4
    push dword scm2str_STOP
scm2str_setup_loop:
    push dword 0x0
    dec ebx
    js  scm2str_setup_loop_done
    jmp scm2str_setup_loop
scm2str_setup_loop_done:
    mov ebx, esp
    mov LAST_ARG, FIELD0(LAST_ARG)
scm2str_write_loop:
    ;DB_PRINT_RIB LAST_ARG , 3
    lea edx, [NIL]
    cmp LAST_ARG, edx
    je  scm2str_write_loop_done
    mov edx, FIELD0(eax)
    shr edx, 1
    mov [ebx], dl
    add dword ebx, 1
    mov LAST_ARG, FIELD1(LAST_ARG)
    jmp scm2str_write_loop
scm2str_write_loop_done:
    push ecx
    ret
    




;; )@@


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

prim_dispatch_table:
;; @@(primitives (gen body)
	dd   prim_rib         ;; @@(primitive (rib a b c))@@
	dd   prim_id          ;; @@(primitive (id x))@@
	dd   prim_arg1        ;; @@(primitive (arg1 x y))@@
	dd   prim_arg2        ;; @@(primitive (arg2 x y))@@
	dd   prim_close       ;; @@(primitive (close rib))@@
	dd   prim_isrib       ;; @@(primitive (rib? rib))@@
	dd   prim_field0      ;; @@(primitive (field0 rib))@@
	dd   prim_field1      ;; @@(primitive (field1 rib))@@
	dd   prim_field2      ;; @@(primitive (field2 rib))@@
	dd   prim_field0set   ;; @@(primitive (field0-set! rib))@@
	dd   prim_field1set   ;; @@(primitive (field1-set! rib))@@
	dd   prim_field2set   ;; @@(primitive (field2-set! rib))@@
	dd   prim_eqv         ;; @@(primitive (eqv? x y))@@
	dd   prim_lt          ;; @@(primitive (< x y))@@
	dd   prim_add         ;; @@(primitive (+ x y))@@
	dd   prim_sub         ;; @@(primitive (- x y))@@
	dd   prim_mul         ;; @@(primitive (* x y))@@
	dd   prim_quotient    ;; @@(primitive (quotient x y))@@
	dd   prim_getchar     ;; @@(primitive (getchar))@@
	dd   prim_putchar     ;; @@(primitive (putchar c))@@
	dd   prim_exit        ;; @@(primitive (exit n))@@  
	dd   prim_stdin       ;; @@(primitive (##stdin))@@  
	dd   prim_stdout      ;; @@(primitive (##stdout))@@  
    dd   prim_get_fd_input_file ;; @@(primitive (##get-fd-input-file filename))@@
    dd   prim_get_fd_output_file ;; @@(primitive (##get-fd-output-file filename))@@
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_rib	db "rib",0x0a,0
%endif

;; @@(feature rib
prim_rib:

%ifdef DEBUG_PRIM
	push string_rib
	call print_string
%endif

	push LAST_ARG
	mov  eax, PREV_ARG
    ;DB_PRINT(5)
	call alloc_rib
	mov  LAST_ARG, stack	; RESULT = LAST_ARG
	mov  stack, FIELD1(LAST_ARG)
	mov  ebx, FIELD1(stack)
	mov  ebx, FIELD1(ebx)
	mov  PREV_ARG, FIELD0(LAST_ARG)
	mov  FIELD1(LAST_ARG), PREV_ARG
	mov  PREV_ARG, FIELD0(ebx)
	mov  FIELD0(LAST_ARG), PREV_ARG
	NBARGS(3)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_id	db "id",0x0a,0
%endif

;; @@(feature id
prim_id:

%ifdef DEBUG_PRIM
	push string_id
	call print_string
%endif

	NBARGS(1)

%ifndef DEBUG_PRIM
prim_arg2:
%endif

	ret
;; )@@

;; @@(feature (and arg2 (not id))
%ifdef RVM_GEN
prim_arg2:
    ret
%endif
;; )@@



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_arg1	db "arg1",0x0a,0
%endif

;; @@(feature arg1
prim_arg1:

%ifdef DEBUG_PRIM
	push string_arg1
	call print_string
%endif

	;; TODO: remove arg1 prim and use "set" instead
	pop  eax		; discard normal return address
	POP_STACK
	jmp  run_next
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM

string_arg2	db "arg2",0x0a,0

prim_arg2:

	push string_arg2
	call print_string

	ret

%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_close	db "close",0x0a,0
%endif

;; @@(feature close
prim_close:

%ifdef DEBUG_PRIM
	push string_close
	call print_string
%endif

	mov  eax, FIELD0(LAST_ARG)
	push FIX(PROCEDURE_TYPE)
    ;DB_PRINT(6)
	call alloc_rib
	mov  LAST_ARG, stack	; RESULT = LAST_ARG
	POP_STACK
	mov  ebx, FIELD1(stack)
	mov  FIELD1(LAST_ARG), ebx
	NBARGS(1)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_field0	db "field0",0x0a,0
%endif

;; @@(feature field0
prim_field0:

%ifdef DEBUG_PRIM
	push string_field0
	call print_string
%endif

	mov  LAST_ARG, FIELD0(LAST_ARG)	; RESULT = LAST_ARG
	NBARGS(1)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_field1	db "field1",0x0a,0
%endif

;; @@(feature field1
prim_field1:

%ifdef DEBUG_PRIM
	push string_field1
	call print_string
%endif

	mov  LAST_ARG, FIELD1(LAST_ARG)	; RESULT = LAST_ARG
	NBARGS(1)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_field2	db "field2",0x0a,0
%endif

;; @@(feature field2
prim_field2:

%ifdef DEBUG_PRIM
	push string_field2
	call print_string
%endif

	mov  LAST_ARG, FIELD2(LAST_ARG)	; RESULT = LAST_ARG
	NBARGS(1)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_field0set	db "field0-set!",0x0a,0
%endif

;; @@(feature field0-set!
prim_field0set:

%ifdef DEBUG_PRIM
	push string_field0set
	call print_string
%endif

	mov  FIELD0(PREV_ARG), LAST_ARG	; RESULT = LAST_ARG
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_field1set	db "field1-set!",0x0a,0
%endif

;; @@(feature field1-set!
prim_field1set:

%ifdef DEBUG_PRIM
	push string_field1set
	call print_string
%endif

	mov  FIELD1(PREV_ARG), LAST_ARG	; RESULT = LAST_ARG
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_field2set	db "field2-set!",0x0a,0
%endif

;; @@(feature field2-set!
prim_field2set:

%ifdef DEBUG_PRIM
	push string_field2set
	call print_string
%endif

	mov  FIELD2(PREV_ARG), LAST_ARG	; RESULT = LAST_ARG
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_isrib	db "rib?",0x0a,0
%endif

;; @@(feature rib? (use eqv?_feature)
prim_isrib:

%ifdef DEBUG_PRIM
	push string_isrib
	call print_string
%endif

	mov  PREV_ARG, LAST_ARG
%if RIB_TAG == 0
	and  al, 0xfe	; set lowest bit of LAST_ARG to 0
%else
	or   al, 1	; set lowest bit of LAST_ARG to 1
%endif

	NBARGS(1)

%ifdef DEBUG_PRIM
	jmp  prim_eqv_internal
%else
	;; fallthrough (will test if the two bit patterns are the same)
%endif
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_eqv	db "eqv?",0x0a,0
%endif

;; @@(feature (or eqv? eqv?_feature) (use <_feature)
prim_eqv:

%ifdef DEBUG_PRIM
	push string_eqv
	call print_string
%endif

prim_eqv_internal:
	cmp  PREV_ARG, LAST_ARG
	je   return_true

;	NBARGS(2)

	mov  LAST_ARG, PREV_ARG

%ifdef DEBUG_PRIM
	jmp  prim_lt_internal
%else
	;; fallthrough (will return #f)
%endif
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_lt	db "<",0x0a,0
%endif

;; @@(feature (or < <_feature)
prim_lt:

%ifdef DEBUG_PRIM
	push string_lt
	call print_string
%endif

prim_lt_internal:
	cmp  PREV_ARG, LAST_ARG
	mov  LAST_ARG, FALSE
	jge  return_boolean
return_true:
	lea  LAST_ARG, [TRUE]
return_boolean:
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_add	db "+",0x0a,0
%endif

;; @@(feature +
prim_add:

%ifdef DEBUG_PRIM
	push string_add
	call print_string
%endif

%if FIX_TAG != 0
	dec  LAST_ARG
%endif
	add  LAST_ARG, PREV_ARG	; RESULT = LAST_ARG
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_sub	db "-",0x0a,0
%endif

;; @@(feature -
prim_sub:

%ifdef DEBUG_PRIM
	push string_sub
	call print_string
%endif

%if FIX_TAG != 0
	dec  LAST_ARG
%endif
	xchg LAST_ARG, PREV_ARG
	sub  LAST_ARG, PREV_ARG	; RESULT = LAST_ARG
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_mul	db "*",0x0a,0
%endif

;; @@(feature *
prim_mul:

%ifdef DEBUG_PRIM
	push string_mul
	call print_string
%endif

%if FIX_TAG != 0
	dec  PREV_ARG
%endif
	shr  LAST_ARG, 1
	imul PREV_ARG
%if FIX_TAG != 0
	inc  LAST_ARG
%endif
;	NBARGS(2)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_quotient	db "quotient",0x0a,0
%endif

;; @@(feature quotient
prim_quotient:

%ifdef DEBUG_PRIM
	push string_quotient
	call print_string
%endif

%if FIX_TAG != 0
	dec  LAST_ARG
	dec  PREV_ARG
%endif
	push LAST_ARG
	xchg LAST_ARG, PREV_ARG
	cdq
	idiv dword [esp]
	pop  PREV_ARG
;	NBARGS(2)
raw_int_to_scheme_int:
	shl  LAST_ARG, 1
%if FIX_TAG != 0
	inc  LAST_ARG
%endif
	ret
;; )@@

;; @@(feature (and raw_int_to_scheme_int (not quotient))
%ifdef RVM_GEN
raw_int_to_scheme_int:
	shl  LAST_ARG, 1
%if FIX_TAG != 0
	inc  LAST_ARG
%endif
	ret
%endif
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_getchar	db "getchar",0x0a,0
%endif

;; @@(feature getchar (use raw_int_to_scheme_int)
prim_getchar:

%ifdef DEBUG_PRIM
	push string_getchar
	call print_string
%endif
    int3

	push ecx
	movC ebx, 0		; ebx = 0 = STDIN
	push ebx		; buffer to read byte
	movC edx, 1		; edx = 1 = number of bytes to read
	mov  ecx, esp		; to the stack
	movC eax, SYS_READ
	CALL_KERNEL
	test eax, eax
	pop  LAST_ARG		; get buffer (0 if no byte read)
	jne  prim_getchar_done
	dec  LAST_ARG		; -1 on EOF
prim_getchar_done:
	pop  ecx
	NBARGS(0)
	jmp  raw_int_to_scheme_int
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_putchar	db "putchar",0x0a,0
%endif

;; @@(feature putchar
prim_putchar:

%ifdef DEBUG_PRIM
	push string_putchar
	call print_string
%endif

	push LAST_ARG
	push ecx
	shr  LAST_ARG, 1
	push LAST_ARG
	movC ebx, STDOUT	; ebx = 1 = STDOUT
	mov  edx, ebx		; edx = 1 = number of bytes to write
	mov  ecx, esp		; from the stack
	movC eax, SYS_WRITE
	CALL_KERNEL
	pop  eax
	pop  ecx
	pop  LAST_ARG
	NBARGS(1)
	ret
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifdef DEBUG_PRIM
string_exit	db "exit",0x0a,0
%endif

%ifdef DEBUG_INSTR
string_halt:	db "halt --",0x0a,0
%endif

run_instr_halt:

%ifdef DEBUG_INSTR
	push string_halt
	call print_string
%endif

	movC LAST_ARG, 0
	;; fallthrough

;; @@(feature exit
prim_exit:

%ifdef DEBUG_PRIM
	push string_exit
	call print_string
%endif

	shr  LAST_ARG, 1
	xchg LAST_ARG, ebx
	movC eax, SYS_EXIT
	CALL_KERNEL
;	NBARGS(1)		; can be avoided because we are exiting!
;	ret
;; )@@


;; @@(feature ##stdin
prim_stdin:
    NBARGS(0)
    mov dword LAST_ARG, FIX(0x0)
    ret
;; )@@

;; @@(feature ##stdout
prim_stdout:
    NBARGS(0)
    mov dword LAST_ARG, FIX(0x1)
    ret
;; )@@

;; @@(feature ##get-fd-input-file (use scm2str)
prim_get_fd_input_file:
    push ecx ;; save ecx
    call scm2str ;; transform eax (LAST_ARG) into a string on stack
; First, prepare the arguments for the 'open' syscall
    mov eax, SYS_OPEN   ; 'open' syscall number
    mov ebx, esp  ; pointer to the null-terminated filename string
    mov ecx, (O_RDONLY ^ O_CREAT)   ; flags (0 means read-only)
    mov edx, 0  ; mode (not used when opening a file for reading)
    CALL_KERNEL

    ;; remove string on top
    xchg edi, esp
    xchg eax, ebx
    mov eax, scm2str_STOP
    repne scasd
    xchg edi, esp
    xchg eax, ebx

    ;; tag eax
    %if FIX_TAG
    shl eax, 1
    add eax, 1
    %endif

    pop ecx ;; retrive stack
    NBARGS(1)
    ret 


;; )@@

;; @@(feature ##get-fd-output-file (use scm2str)
prim_get_fd_output_file:
    push ecx
    call scm2str ;; transform eax (LAST_ARG) into a string on stack
    int3


    NBARGS(1)
    pop ecx
    ret


;; )@@

; @@(location prims)@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The compressed RVM code

;; @@(replace ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{" (encode 92)
rvm_code:	db ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{",0 ; RVM code that prints HELLO!
;; )@@

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Miscellaneous debugging routines

%ifdef DEBUG

print_tos:

	push eax
	movC eax, 32
	call putchar
	pop  eax
	push stack
	call print_word_hex
	push eax
	movC eax, 58
	call putchar
	movC eax, 32
	call putchar
	pop  eax
	push dword FIELD0(stack)
	call print_word_hex
	push eax
	movC eax, 32
	call putchar
	pop  eax
	push dword FIELD1(stack)
	call print_word_hex
	push eax
	movC eax, 32
	call putchar
	pop  eax
	push dword FIELD2(stack)
	call print_word_hex
	push eax
	movC eax, 0x0a
	call putchar
	pop  eax

	ret
%endif

%ifdef DEBUG

print_list:

	push eax

	push eax
	movC eax, 0x0a
	call putchar
	pop  eax

	mov  eax, [esp+8]

print_list_loop:

	push eax
	movC eax, 32
	call putchar
	pop  eax

	push eax
	call print_word_hex

	push eax
	movC eax, 58
	call putchar
	movC eax, 32
	call putchar
	pop  eax

	push dword FIELD0(eax)
	call print_word_hex

	push eax
	movC eax, 32
	call putchar
	pop  eax

	push dword FIELD1(eax)
	call print_word_hex

	push eax
	movC eax, 32
	call putchar
	pop  eax

	push dword FIELD2(eax)
	call print_word_hex

	push eax
	movC eax, 0x0a
	call putchar
	pop  eax

	cmp  dword FIELD1(eax), FALSE
	mov  eax, FIELD1(eax)
	jne   print_list_loop

	pop  eax

	ret  4

%endif

%ifdef DEBUG

print_heap:

	push eax
	movC eax, 0x0a
	call putchar
	pop  eax

	call print_regs

	push eax
	movC eax, 0x0a
	call putchar
	pop  eax

	call print_eax

	movC ecx, HEAP_SIZE_RIBS
	mov  ebx, heap_base
print_heap_loop:

	push ebx
	call print_word_hex
	mov  al, 58
	call putchar
	mov  al, 32
	call putchar
	push dword FIELD0(ebx)
	call print_word_hex
	mov  al, 32
	call putchar
	push dword FIELD1(ebx)
	call print_word_hex
	mov  al, 32
	call putchar
	push dword FIELD2(ebx)
	call print_word_hex
	mov  al, 32
	call putchar
	push dword FIELD3(ebx)
	call print_word_hex
	mov  al, 0x0a
	call putchar

	add  ebx, WORD_SIZE * RIB_SIZE_WORDS
	dec  ecx
	jne  print_heap_loop

	ret

%endif

%ifdef NEED_PRINT_REGS

print_regs:
	push eax
	mov  al, 0x0a
	call putchar
	pop  eax
	call print_eax
	call print_ebx
	call print_ecx
	call print_edx
	call print_esi
	call print_edi
	call print_ebp
	jmp  print_esp

%ifndef NEED_PRINT_EAX
%define NEED_PRINT_EAX
%endif

%ifndef NEED_PRINT_EBX
%define NEED_PRINT_EBX
%endif

%ifndef NEED_PRINT_ECX
%define NEED_PRINT_ECX
%endif

%ifndef NEED_PRINT_EDX
%define NEED_PRINT_EDX
%endif

%ifndef NEED_PRINT_ESI
%define NEED_PRINT_ESI
%endif

%ifndef NEED_PRINT_EDI
%define NEED_PRINT_EDI
%endif

%ifndef NEED_PRINT_EBP
%define NEED_PRINT_EBP
%endif

%ifndef NEED_PRINT_ESP
%define NEED_PRINT_ESP
%endif

%endif

%ifdef NEED_PRINT_EAX

print_eax:
	push eax
        push string_eax
        call print_string
	push eax
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_eax:	db "eax=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_EBX

print_ebx:
	push eax
        push string_ebx
        call print_string
	push ebx
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_ebx:	db "ebx=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_ECX

print_ecx:
	push eax
        push string_ecx
        call print_string
	push ecx
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_ecx:	db "ecx=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_EDX

print_edx:
	push eax
        push string_edx
        call print_string
	push edx
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_edx:	db "edx=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_ESI

print_esi:
	push eax
        push string_esi
        call print_string
	push esi
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_esi:	db "esi=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_EDI

print_edi:
	push eax
        push string_edi
        call print_string
	push edi
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_edi:	db "edi=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_EBP

print_ebp:
	push eax
        push string_ebp
        call print_string
	push ebp
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_ebp:	db "ebp=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_ESP

print_esp:
	push eax
        push string_esp
        call print_string
	push esp
	add  dword [esp], WORD_SIZE*2	; compensate for pushes
	call print_word_hex
	mov  al, 0x0a
	call putchar
	pop  eax
	ret

string_esp:	db "esp=",0

%ifndef NEED_PRINT_WORD_HEX
%define NEED_PRINT_WORD_HEX
%endif

%endif

%ifdef NEED_PRINT_WORD_HEX

print_word_hex:
        pushf
        push eax
        push ebx
        push ecx
        push edx
        push esi
        push edi
        push ebp

        push string_0x
        call print_string

        mov  eax, [esp+WORD_SIZE*9]	; get the number to print
        mov  cl, WORD_SIZE*2

print_word_hex_loop:
        rol  eax, 4
        push eax
        call print_hex1
        dec  cl
        jnz  print_word_hex_loop

        pop  ebp
        pop  edi
        pop  esi
        pop  edx
        pop  ecx
        pop  ebx
        pop  eax
        popf
        ret  WORD_SIZE*1

string_0x:	db "0x",0

%ifndef NEED_PRINT_HEX1
%define NEED_PRINT_HEX1
%endif
	
%ifndef NEED_PRINT_STRING
%define NEED_PRINT_STRING
%endif
	
%endif

%ifdef NEED_PRINT_HEX1

print_hex1:
        push eax

        mov  eax, [esp+WORD_SIZE*2]
        and  al, 0x0f
        cmp  al, 10
        jb   print_hex1_putchar
        add  al, 39		; adjust for a-f
print_hex1_putchar:
        add  al, 48		; offset for 0-9
        call putchar

        pop  eax
        ret  WORD_SIZE*1

%ifndef NEED_PUTCHAR
%define NEED_PUTCHAR
%endif

%endif


%ifdef NEED_PRINT_RIB

print_rib_str_dot: db "...", 0x0

print_rib:
	push eax
	push ebx
	push ecx
	push edx
	push esi
        mov  ecx, [esp+WORD_SIZE*6] ;; rib
        mov  ebx, [esp+WORD_SIZE*7] ;; depth
        cmp  ebx, 0
        jz   print_rib_dot
        test ecx, 0x1
        jz   print_rib_aux
        ; print_int
        shr  ecx, 1
        push ecx
        call print_int
        
        jmp  print_rib_done

print_rib_aux:
        sub ebx, 1
        mov eax, 0x5b ;; [
        call putchar

        push ebx
        push dword FIELD0(ecx)
        call print_rib

        mov eax, 0x2c ;; ,
        call putchar

        push ebx
        push dword FIELD1(ecx)
        call print_rib

        mov eax, 0x2c ;; ,
        call putchar

        push ebx
        push dword FIELD2(ecx)
        call print_rib

        mov eax, 0x5d ;; ]
        call putchar
        jmp print_rib_done

print_rib_dot:
    push print_rib_str_dot
    call print_string

print_rib_done:
    
	pop  esi
	pop  edx
	pop  ecx
	pop  ebx
	pop  eax
	ret  WORD_SIZE*2

%ifndef NEED_PRINT_INT
%define NEED_PRINT_INT
%endif

%ifndef NEED_PUTCHAR
%define NEED_PUTCHAR
%endif

%ifndef NEED_PRINT_STRING
%define NEED_PRINT_STRING
%endif

%ifndef CALLEE_SAVE_EBX_ECX_EDX
%define CALLEE_SAVE_EBX_ECX_EDX
%endif

%endif






%ifdef NEED_PRINT_INT

;; Can only print posivite numbers
print_int:
	push eax
	push ebx
	push ecx
	push edx
	push esi
        mov  eax, [esp+WORD_SIZE*6]
        cmp  eax, 0
        je   print_int_0
        push 10

print_int_loop:
        cmp  eax, 0
        je   print_int_loop2
        mov  edx, 0
        mov  ebx, 10
        div  ebx
        push edx
        jmp print_int_loop

print_int_loop2: 
        pop eax
        cmp eax, 10 
        je  print_int_done
        add eax, 0x30
        call putchar
        jmp  print_int_loop2

print_int_0:
        mov eax, 0x30
        call putchar

print_int_done:
	pop  esi
	pop  edx
	pop  ecx
	pop  ebx
	pop  eax
	ret  WORD_SIZE

%ifndef NEED_PUTCHAR
%define NEED_PUTCHAR
%endif

%ifndef CALLEE_SAVE_EBX_ECX_EDX
%define CALLEE_SAVE_EBX_ECX_EDX
%endif

%endif

%ifdef NEED_PRINT_STRING


print_string:
	push eax
	push ebx
	push ecx
	push edx
	push esi
        mov  esi, [esp+WORD_SIZE*6]	; get string to print
        movC eax, 0

print_string_loop:
        mov  al, [esi]
        cmp  al, 0
        je   print_string_loop_done
        call putchar
        inc  esi
        jmp  print_string_loop

print_string_loop_done:
	pop  esi
	pop  edx
	pop  ecx
	pop  ebx
	pop  eax
	ret  WORD_SIZE*1

%ifndef NEED_PUTCHAR
%define NEED_PUTCHAR
%endif

%ifndef CALLEE_SAVE_EBX_ECX_EDX
%define CALLEE_SAVE_EBX_ECX_EDX
%endif

%endif

%ifdef NEED_GETCHAR

getchar:
%ifdef CALLEE_SAVE_EBX_ECX_EDX
	push ebx
	push ecx
	push edx
%endif
	movC ebx, 0		; ebx = 0 = STDIN
	push ebx		; buffer to read byte
	movC edx, 1		; edx = 1 = number of bytes to read
	mov  ecx, esp		; to the stack
	movC eax, SYS_READ
%ifdef NEED_PUTCHAR
	jmp  call_kernel
%endif
%ifndef NEED_CALL_KERNEL
%define NEED_CALL_KERNEL
%endif

%endif

%ifdef NEED_PUTCHAR

putchar:
%ifdef CALLEE_SAVE_EBX_ECX_EDX
	push ebx
	push ecx
	push edx
%endif
	push eax
	movC ebx, STDOUT	; ebx = 1 = STDOUT
	mov  edx, ebx		; edx = 1 = number of bytes to write
	mov  ecx, esp		; from the stack
	movC eax, SYS_WRITE
%ifndef NEED_CALL_KERNEL
%define NEED_CALL_KERNEL
%endif

%endif

%ifdef NEED_CALL_KERNEL

call_kernel:
	CALL_KERNEL
	pop  eax
%ifdef CALLEE_SAVE_EBX_ECX_EDX
	pop  edx
	pop  ecx
	pop  ebx
%endif
	ret

%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ######################################## End of RVM

filesize      equ     $ - $$
