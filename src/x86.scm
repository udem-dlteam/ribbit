;;;============================================================================

;;; File: "x86.scm"

;;; Copyright (c) 1994-2022 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; This module implements an Intel X86-64 instruction encoder.

;;;============================================================================

;; X86 register representation.

(define (x86-reg8 n)  (fx+ 48 n))
(define (x86-reg16 n) (fx+ 32 n))
(define (x86-reg32 n) (fx+ 16 n))
(define (x86-reg64 n) n)

(define (x86-reg? x) (fixnum? x))
(define (x86-reg8? reg) (fx>= reg 48))
(define (x86-reg8-h? reg) (fx>= reg 64))
(define (x86-reg16? reg) (and (fx>= reg 32) (fx< reg 48)))
(define (x86-reg32? reg) (and (fx>= reg 16) (fx< reg 32)))
(define (x86-reg64? reg) (fx< reg 16))

(define (x86-reg-field reg) (fxand reg 15))

(define (x86-reg-width reg)
  (cond ((fx< reg 16) 64)
        ((fx< reg 32) 32)
        ((fx< reg 48) 16)
        (else         8)))

(define (x86-register-name reg)
  (vector-ref
   '#(rax  rcx  rdx  rbx  rsp  rbp  rsi  rdi
      r8   r9   r10  r11  r12  r13  r14  r15
      eax  ecx  edx  ebx  esp  ebp  esi  edi
      r8d  r9d  r10d r11d r12d r13d r14d r15d
      ax   cx   dx   bx   sp   bp   si   di
      r8w  r9w  r10w r11w r12w r13w r14w r15w
      al   cl   dl   bl   spl  bpl  sil  dil
      r8b  r9b  r10b r11b r12b r13b r14b r15b
      ?    ?    ?    ?    ah   ch   dh   bh)
   reg))

(define (x86-rax) 0)
(define (x86-rcx) 1)
(define (x86-rdx) 2)
(define (x86-rbx) 3)
(define (x86-rsp) 4)
(define (x86-rbp) 5)
(define (x86-rsi) 6)
(define (x86-rdi) 7)
(define (x86-r8)  8)
(define (x86-r9)  9)
(define (x86-r10) 10)
(define (x86-r11) 11)
(define (x86-r12) 12)
(define (x86-r13) 13)
(define (x86-r14) 14)
(define (x86-r15) 15)

(define (x86-eax)  16)
(define (x86-ecx)  17)
(define (x86-edx)  18)
(define (x86-ebx)  19)
(define (x86-esp)  20)
(define (x86-ebp)  21)
(define (x86-esi)  22)
(define (x86-edi)  23)
(define (x86-r8d)  24)
(define (x86-r9d)  25)
(define (x86-r10d) 26)
(define (x86-r11d) 27)
(define (x86-r12d) 28)
(define (x86-r13d) 29)
(define (x86-r14d) 30)
(define (x86-r15d) 31)

(define (x86-ax)   32)
(define (x86-cx)   33)
(define (x86-dx)   34)
(define (x86-bx)   35)
(define (x86-sp)   36)
(define (x86-bp)   37)
(define (x86-si)   38)
(define (x86-di)   39)
(define (x86-r8w)  40)
(define (x86-r9w)  41)
(define (x86-r10w) 42)
(define (x86-r11w) 43)
(define (x86-r12w) 44)
(define (x86-r13w) 45)
(define (x86-r14w) 46)
(define (x86-r15w) 47)

(define (x86-al)   48)
(define (x86-cl)   49)
(define (x86-dl)   50)
(define (x86-bl)   51)
(define (x86-ah)   68)
(define (x86-ch)   69)
(define (x86-dh)   70)
(define (x86-bh)   71)
(define (x86-spl)  52)
(define (x86-bpl)  53)
(define (x86-sil)  54)
(define (x86-dil)  55)
(define (x86-r8b)  56)
(define (x86-r9b)  57)
(define (x86-r10b) 58)
(define (x86-r11b) 59)
(define (x86-r12b) 60)
(define (x86-r13b) 61)
(define (x86-r14b) 62)
(define (x86-r15b) 63)

(define (x86-ah) 64)
(define (x86-ch) 65)
(define (x86-dh) 66)
(define (x86-bh) 67)

;; X86 memory operands.

;;; Architecture selection (either x86-32 or x86-64).

(define (x86-arch-set! cgc arch)
  (x86-codegen-context-arch-set! cgc arch)
  (if (x86-codegen-context-listing-format cgc)
      (asm-listing
       cgc
       (if (eq? 'gnu (x86-codegen-context-listing-format cgc))
           (list2 "\t.code" (x86-word-width cgc))
           (list2 "\tbits " (x86-word-width cgc))))))

(define (x86-64bit-mode? cgc)
  (eq? (x86-codegen-context-arch cgc) 'x86-64))

(define (x86-word-width cgc)
  (if (x86-64bit-mode? cgc) 64 32))

(define (x86-assert-64bit-mode cgc)
  (assert (x86-64bit-mode? cgc)
          "instruction only valid for x86-64"
          '()))

(define (x86-assert-32bit-mode cgc)
  (assert (not (x86-64bit-mode? cgc))
          "instruction only valid for x86-32"
          '()))

;;;============================================================================

;;; Instruction operands.

(define (x86-force-width opnd width)
  ;; useful for formatting to get an operand with an explicit width
  (vector2 opnd width))

(define (x86-force-width? x) (and (vector? x) (fx= (vector-length x) 2)))
(define (x86-force-width-opnd x) (vector-ref x 0))
(define (x86-force-width-width x) (vector-ref x 1))

(define (x86-imm? x) (pair? x))

(define (x86-imm-int value width) (cons width value))
(define (x86-imm-int? x) (and (pair? x) (integer? (cdr x))))
(define (x86-imm-int-width x) (car x))
(define (x86-imm-int-value x) (cdr x))

(define (x86-imm-late handler width) (cons width handler))
(define (x86-imm-late? x) (and (pair? x) (procedure? (cdr x))))
(define (x86-imm-late-width x) (car x))
(define (x86-imm-late-handler x) (cdr x))

(define (x86-mem offset reg1)
  (vector4 offset reg1 #f 0))

(define (x86-mem-indexing offset reg1 reg2 scale)
  (vector4 offset reg1 reg2 scale))

(define (x86-mem? x) (and (vector? x) (fx= (vector-length x) 4)))
(define (x86-mem-offset x) (vector-ref x 0))
(define (x86-mem-reg1 x) (vector-ref x 1))
(define (x86-mem-reg2 x) (vector-ref x 2))
(define (x86-mem-scale x) (vector-ref x 3))

(define (x86-mem-abs? x)
  (and (not (x86-mem-reg1 x))
       (not (x86-mem-reg2 x))))

;;;============================================================================

;;; Listing generation.

(define (x86-offset->string offset)
  (cond ((fx= offset 0) "")
        ((fx< offset 0) (number->string offset))
        (else           (string-append "+" (number->string offset)))))

(define (x86-listing cgc mnemonic width opnds)

  (define (instr-format-gnu)

    (define (opnd-format opnd)
      (cond ((asm-label? opnd)
             (if (x86-64bit-mode? cgc) ;; using RIP relative addressing?
                 (list2 (asm-label-name opnd) "(%rip)")
                 (asm-label-name opnd)))
            ((x86-force-width? opnd)
             (opnd-format (x86-force-width-opnd opnd)))
            ((x86-reg? opnd)
             (list2 "%" (x86-register-name opnd)))
            ((x86-imm? opnd)
             (list2 "$"
                    (cond ((x86-imm-int? opnd)
                           (number->string (x86-imm-int-value opnd)))
                          ((x86-imm-late? opnd)
                           ((x86-imm-late-handler opnd) cgc 'listing))
                          (else
                           (compiler-internal-error
                            "unknown immediate"
                            opnd)))))
            ((x86-mem? opnd)
             (let ((reg1 (x86-mem-reg1 opnd))
                   (reg2 (x86-mem-reg2 opnd))
                   (scale (x86-mem-scale opnd))
                   (offset (x86-mem-offset opnd)))
               (if (or reg1 reg2)
                   (let ((x
                          (cons "("
                                (cons (if reg1 (opnd-format reg1) "")
                                      (if reg2
                                          (cons ","
                                                (cons (opnd-format reg2)
                                                      (if (fx= scale 0)
                                                          '(")")
                                                          (list3 ","
                                                                 (fxarithmetic-shift-left
                                                                  1
                                                                  scale)
                                                                 ")"))))
                                          '(")"))))))
                     (if (fx= offset 0)
                         x
                         (cons (number->string offset) x)))
                   offset)))
            (else
             opnd)))

    (let ((operands
           (asm-separated-list (map opnd-format (reverse opnds)) ",")))
      (cons "\t"
            (cons mnemonic
                  (if (fx>= width 0)
                      (cons (x86-width-suffix cgc width)
                            (if (pair? operands)
                                (cons "\t"
                                      operands)
                                '()))
                      (if (pair? operands)
                          (cons "\t"
                                (cons "*"
                                      operands))
                          '()))))))

  (define (instr-format-nasm)

    (define (data-width-qualifier width)
      (cond ((eqv? 8 width)  "byte ")
            ((eqv? 16 width) "word ")
            ((eqv? 32 width) "dword ")
            ((eqv? 64 width) "qword ")
            ((eqv? 1 width)  "short ") ;; special width for short jumps
            (else            "")))

    (define (opnd-format opnd)
      (cond ((asm-label? opnd)
             (if (x86-64bit-mode? cgc) ;; using RIP relative addressing?
                 (list3 "[rel " (asm-label-name opnd) "]")
                 (asm-label-name opnd)))
            ((x86-force-width? opnd)
             (list2 (data-width-qualifier (x86-force-width-width opnd))
                    (opnd-format (x86-force-width-opnd opnd))))
            ((x86-reg? opnd)
             (x86-register-name opnd))
            ((x86-imm? opnd)
             (cond ((x86-imm-int? opnd)
                    (let ((value (x86-imm-int-value opnd))
                          (opnd-width (x86-imm-int-width opnd)))
                      (if (or (fx= width 8)
                              (fx= width opnd-width)
                              (and (fx= width 64)
                                   (fx= opnd-width 32)
                                   (not (equal? mnemonic "mov"))))
                          value
                          (list2 (data-width-qualifier opnd-width) value))))
                   ((x86-imm-late? opnd)
                    ((x86-imm-late-handler opnd) cgc 'listing))
                   (else
                    (compiler-internal-error
                     "unknown immediate"
                     opnd))))
            ((x86-mem? opnd)
             (let ((offset (x86-mem-offset opnd))
                   (reg1 (x86-mem-reg1 opnd))
                   (reg2 (x86-mem-reg2 opnd))
                   (scale (x86-mem-scale opnd)))
               (list5 "["
                      (if reg1
                          (opnd-format reg1)
                          "")
                      (if reg2
                          (list3 (if reg1 "+" "")
                                 (opnd-format reg2)
                                 (if (fx= scale 0)
                                     ""
                                     (list2 "*"
                                            (fxarithmetic-shift-left 1 scale))))
                          "")
                      (x86-offset->string offset)
                      "]")))
            (else
             opnd)))

    (cons "\t"
          (cons mnemonic
                (if (pair? opnds)
                    (let ((width-implicit? #f))

                      (define (opnd-fmt opnd)
                        (if (x86-reg? opnd)
                            (set! width-implicit? #t))
                        (opnd-format opnd))

                      (let ((opnds-listing
                             (asm-separated-list (map opnd-fmt opnds) ",")))
                        (cons "\t"
                              (if width-implicit?
                                  opnds-listing
                                  (cons (data-width-qualifier width)
                                        opnds-listing)))))
                    '()))))

  (asm-listing
   cgc
   (if (eq? 'gnu (x86-codegen-context-listing-format cgc))
       (instr-format-gnu)
       (instr-format-nasm))))

(define (x86-width-suffix cgc width)
  (if (eq? 'gnu (x86-codegen-context-listing-format cgc))
      (cond ((fx= width 64) "q")
            ((fx= width 32) "l")
            ((fx= width 16) "w")
            ((fx= width 8)  "b")
            ((fx= width 1)  "")
            (else           ""))
      ""))

;; Escape opcode

(define (x86-esc-opcode cgc)
  (asm-8 cgc #x0f))

;;;============================================================================

;;; X86 operand encoding.

(define (x86-opnd-prefix-reg-opnd cgc reg opnd)
  (let* ((width
          (x86-reg-width reg))
         (field
          (x86-reg-field reg))
         (ext-lo8-reg?
          (and (fx= width 8)
               (fx>= field 4)
               (not (x86-reg8-h? reg)))))
    (if (x86-reg? opnd)
        (begin
          (let* ((field2
                  (x86-reg-field opnd))
                 (ext-lo8-reg2?
                  (and (fx= (x86-reg-width opnd) 8)
                       (fx>= field2 4)
                       (not (x86-reg8-h? opnd))))
                 (rex?
                  (x86-opnd-prefix cgc
                                   width
                                   field
                                   opnd
                                   (or ext-lo8-reg? ext-lo8-reg2?))))
            (assert (not (and rex?
                              (or (x86-reg8-h? reg)
                                  (x86-reg8-h? opnd))))
                    "cannot use high 8 bit register here"
                    (list2 reg opnd))
            rex?))
        (x86-opnd-prefix cgc
                         width
                         field
                         opnd
                         ext-lo8-reg?))))

(define (x86-opnd-prefix-opnd cgc width opnd)
  (if (x86-reg? opnd)
      (let* ((field
              (x86-reg-field opnd))
             (ext-lo8-reg?
              (and (fx= width 8)
                   (fx>= field 4)
                   (not (x86-reg8-h? opnd)))))
        (x86-opnd-prefix cgc width 0 opnd ext-lo8-reg?))
      (x86-opnd-prefix cgc width 0 opnd #f)))

(define (x86-opnd-modrm/sib-reg-opnd cgc reg opnd imm-width)
  (x86-opnd-modrm/sib cgc (x86-reg-field reg) opnd imm-width))

(define (x86-opnd-prefix cgc width field opnd force-rex?)
  (let ((rex*
         (fx+ (fx+ ;; if needed emit REX.W (64 bit operand size)
               (if (and (not (fx= width 0)) ;; implicit width?
                        (or (fx= width 64)
                            (and (x86-reg? opnd) (x86-reg64? opnd))))
                   8
                   0)
               ;; if needed emit REX.R (Extension of the ModR/M reg field)
               (fxarithmetic-shift-left
                (fxarithmetic-shift-right
                 field
                 3)
                2))
              (cond ((x86-reg? opnd)
                     ;; if needed emit REX.B (Extension of
                     ;; the ModR/M r/m field, SIB base field,
                     ;; or Opcode reg field)
                     (fxarithmetic-shift-right
                      (x86-reg-field opnd)
                      3))
                    ((x86-mem? opnd)
                     (let ((reg1 (x86-mem-reg1 opnd))
                           (reg2 (x86-mem-reg2 opnd)))

                       (define (reg2-rexx reg)
                         (if reg2
                             (begin
                               (assert (if (x86-reg32? reg)
                                           (x86-reg32? reg2)
                                           (x86-reg64? reg2))
                                       "index register must have same width as base"
                                       (list2 reg reg2))
                               ;; if needed emit REX.X (Extension
                               ;; of the SIB index field)
                               (fxarithmetic-shift-left
                                (fxarithmetic-shift-right
                                 (x86-reg-field reg2)
                                 3)
                                1))
                             0))

                       (if reg1
                           (begin
                             (assert (or (x86-reg32? reg1)
                                         (and (x86-reg64? reg1)
                                              (x86-64bit-mode? cgc)))
                                     "invalid width base register"
                                     reg1)
                             (fx+ ;; if needed emit REX.B (Extension of
                                  ;; the ModR/M r/m field, SIB base field,
                                  ;; or Opcode reg field)
                                  (fxarithmetic-shift-right
                                   (x86-reg-field reg1)
                                   3)
                                  (reg2-rexx reg1)))
                           (reg2-rexx reg2))))
                    ((asm-label? opnd)
                     0)
                    (else
                     (compiler-internal-error
                      "unknown operand"
                      opnd))))))
    (x86-opnd-size-override-prefix cgc width)
    (x86-addr-size-override-prefix cgc opnd)
    (if (or force-rex?
            (not (fx= rex* 0)))
        (begin
          (x86-assert-64bit-mode cgc)
          (asm-8 cgc (fx+ #x40 rex*)) ;; REX
          #t)
        #f)))

(define (x86-opnd-size-override-prefix cgc width)
  (if (fx= width 16)
      (asm-8 cgc #x66))) ;; operand size override prefix

(define (x86-addr-size-override-prefix cgc opnd)
  (if (and (x86-mem? opnd)
           (let ((reg1 (x86-mem-reg1 opnd)))
             (and reg1
                  (eq? (x86-64bit-mode? cgc)
                       (not (x86-reg64? reg1))))))
      (asm-8 cgc #x67))) ;; address size override prefix

(define (x86-opnd-modrm/sib cgc field opnd imm-width)
  (let ((modrm-rf
         (fxarithmetic-shift-left (fxand 7 field) 3)))

    (define (abs-addr)
      (if (x86-64bit-mode? cgc) ;; avoid RIP relative encoding?
          (begin
            (asm-8 cgc (fx+ modrm-rf 4)) ;; ModR/M
            (asm-8 cgc #x25))            ;; SIB
          (asm-8 cgc (fx+ modrm-rf 5)))) ;; ModR/M

    (cond ((x86-reg? opnd)
           (let ((modrm*
                  (fx+ modrm-rf (fxand 7 (x86-reg-field opnd)))))
             (asm-8 cgc (fx+ #xc0 modrm*)))) ;; ModR/M

          ((x86-mem? opnd)
           (let ((offset (x86-mem-offset opnd))
                 (reg1   (x86-mem-reg1 opnd))
                 (reg2   (x86-mem-reg2 opnd)))

             (if (or reg1 reg2)

                 (let* ((field1    (if reg1 (x86-reg-field reg1) 5))
                        (field1-lo (fxand 7 field1)))

                   (if (or reg2 ;; need a SIB when using an index register
                           (not reg1)         ;; or no base register
                           (fx= field1-lo 4)) ;; or base = RSP/R12

                       ;; SIB with base register needed

                       (let ((modrm*
                              (fx+ modrm-rf 4))
                             (sib
                              (fx+ field1-lo
                                   (if reg2
                                       (let ((field2 (x86-reg-field reg2)))
                                         (assert (not (fx= field2 4))
                                                 "SP not allowed as index"
                                                 reg2)
                                         (fx+ (fxarithmetic-shift-left
                                               (fxand 7 field2)
                                               3)
                                              (fxarithmetic-shift-left
                                               (x86-mem-scale opnd)
                                               6)))
                                       #x20)))) ;; no index and no scaling

                         (cond ((not reg1)
                                ;; use 32 bit displacement
                                (asm-8 cgc (fx+ #x00 modrm*)) ;; ModR/M
                                (asm-8 cgc sib)               ;; SIB
                                (asm-32-le cgc offset))
                               ((asm-signed8? offset)
                                (if (or (not (fx= offset 0)) ;; non-null offset?
                                        (fx= field1-lo 5))   ;; or RBP/R13
                                    (begin ;; use 8 bit displacement
                                      (asm-8 cgc (fx+ #x40 modrm*)) ;; ModR/M
                                      (asm-8 cgc sib) ;; SIB
                                      (asm-8 cgc offset))
                                    (begin
                                      (asm-8 cgc (fx+ #x00 modrm*)) ;; ModR/M
                                      (asm-8 cgc sib)))) ;; SIB
                               (else
                                ;; use 32 bit displacement
                                (asm-8 cgc (fx+ #x80 modrm*)) ;; ModR/M
                                (asm-8 cgc sib)               ;; SIB
                                (asm-32-le cgc offset))))

                       ;; SIB not needed

                       (let ((modrm*
                              (fx+ modrm-rf field1-lo)))
                         (if (asm-signed8? offset)
                             (if (or (not (fx= offset 0)) ;; non-null offset?
                                     (fx= field1-lo 5)) ;; or RBP/R13
                                 (begin ;; use 8 bit displacement
                                   (asm-8 cgc (fx+ #x40 modrm*)) ;; ModR/M
                                   (asm-8 cgc offset))
                                 (asm-8 cgc (fx+ #x00 modrm*))) ;; ModR/M
                             (begin ;; use 32 bit displacement
                               (asm-8 cgc (fx+ #x80 modrm*)) ;; ModR/M
                               (asm-32-le cgc offset))))))

                 (begin ;; absolute address, use disp32 ModR/M
                   (abs-addr)
                   (asm-32-le cgc offset)))))

          ((asm-label? opnd)
           (if (x86-64bit-mode? cgc) ;; can use RIP relative addressing?
               (begin
                 (asm-8 cgc (fx+ modrm-rf 5)) ;; ModR/M for RIP relative addressing
                 (asm-at-assembly
                  cgc
                  (list2
                   (lambda (cb self)
                     4)
                   (lambda (cb self)
                     (let ((dist (fx- (asm-label-pos opnd)
                                      (fx+ (fxarithmetic-shift-right imm-width 3)
                                           (fx+ self 4)))))
                       (asm-32-le cb dist)))))) ;; 32 bit distance
               (compiler-internal-error
                "relative addressing not supported on x86-32"
                opnd)))

          (else
           (compiler-internal-error
            "unknown operand"
            opnd)))))

(define (x86-imm-encode cgc imm imm-width)
  (cond ((x86-imm-int? imm)
         (let* ((n1 (x86-imm-int-value imm))
                (n2 (asm-int-le cgc n1 imm-width)))
           (assert (= n1 n2)
                   "immediate operand has been truncated"
                   imm)
           (x86-imm-int n2 imm-width)))
        ((x86-imm-late? imm)
         ((x86-imm-late-handler imm) cgc 'encode))
        (else
         (compiler-internal-error
          "unknown immediate"
          imm))))

;;;============================================================================

;;; X86 pseudo operations.

(define (x86-label cgc label)
  (asm-label cgc label)
  (if (x86-codegen-context-listing-format cgc)
      (asm-listing cgc (list2 (asm-label-name label) ":"))))

(define (x86-global-label cgc name)
  (table-ref (x86-codegen-context-global-labels cgc) name #f))

(define (x86-make-global-label cgc name)
  (let ((lbl (asm-make-label cgc name)))
    (table-set! (x86-codegen-context-global-labels cgc) name lbl)
    lbl))

(define (x86-db cgc elems)
  (x86-data-elems cgc elems 8))

(define (x86-dw cgc elems)
  (x86-data-elems cgc elems 16))

(define (x86-dd cgc elems)
  (x86-data-elems cgc elems 32))

(define (x86-dq cgc elems)
  (x86-data-elems cgc elems 64))

(define (x86-data-elems cgc elems width)

  (define max-per-line 4)

  (let ((v (list->vector elems)))
    (let loop1 ((i 0))
      (if (fx< i (vector-length v))
          (let ((lim (fxmin (fx+ i max-per-line) (vector-length v))))
            (let loop2 ((j i) (rev-lst '()))
              (if (fx< j lim)
                  (let ((x (vector-ref v j)))
                    (asm-int-le cgc x width)
                    (loop2 (fx+ j 1) (cons x rev-lst)))
                  (begin
                    (if (x86-codegen-context-listing-format cgc)
                        (asm-listing
                         cgc
                         (list4 "\t"
                                (if (eq? 'gnu (x86-codegen-context-listing-format cgc))
                                    (cond ((fx= width 8)  ".byte")
                                          ((fx= width 16) ".word")
                                          ((fx= width 32) ".long")
                                          ((fx= width 64) ".quad")
                                          (else
                                           (compiler-internal-error
                                            "unknown data width"
                                            width)))
                                    (cond ((fx= width 8)  "db")
                                          ((fx= width 16) "dw")
                                          ((fx= width 32) "dd")
                                          ((fx= width 64) "dq")
                                          (else
                                           (compiler-internal-error
                                            "unknown data width"
                                            width))))
                                "\t"
                                (asm-separated-list (reverse rev-lst) ","))))
                    (loop1 lim)))))))))

;;;============================================================================

;;; X86 instructions: ADD, OR, ADC, SBB, AND, SUB, XOR, CMP, and MOV.

(define (x86-add cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 0))

(define (x86-or cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 1))

(define (x86-adc cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 2))

(define (x86-sbb cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 3))

(define (x86-and cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 4))

(define (x86-sub cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 5))

(define (x86-xor cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 6))

(define (x86-cmp cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 7))

(define (x86-mov cgc opnd1 opnd2)
  (define width #f)
  (x86-op cgc opnd1 opnd2 width 17))

(define (x86-op cgc opnd1 opnd2 width op)

  ;; opnd1 = destination, opnd2 = source

  (define (listing width opnd1 opnd2)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (if (fx<= op 7)
                         (vector-ref
                          '#("add" "or" "adc" "sbb" "and" "sub" "xor" "cmp")
                          op)
                         "mov")
                     width
                     (list2 opnd1
                            opnd2))))

  (define (mov-imm)

    (define (register-mov-imm width)
      (x86-opnd-prefix-opnd cgc width opnd1) ;; prefix
      ;; opcode = #xb0-#xb7 (for 8 bit registers)
      ;;       or #xb8-#xbf (for 16/32/64 bit registers)
      (asm-8 cgc (fx+ (if (fx= width 8) #xb0 #xb8) ;; opcode
                      (fxand 7 (x86-reg-field opnd1))))
      (listing width
               opnd1
               (x86-imm-encode cgc opnd2 width)))

    (define (general-mov-imm width imm-width)
      (x86-opnd-prefix-opnd cgc width opnd1)     ;; prefix
      (asm-8 cgc (if (fx= width 8) #xc6 #xc7))   ;; opcode
      (x86-opnd-modrm/sib cgc 0 opnd1 imm-width) ;; ModR/M
      (listing width
               opnd1
               (x86-imm-encode cgc opnd2 imm-width)))

    (assert (if (x86-reg? opnd1)
                (or (not width)
                    (fx= (x86-reg-width opnd1) width))
                width)
            "missing or inconsistent operand width"
            width)

    (assert (or (x86-reg? opnd1)
                (not (fx= width 64))
                (and (x86-imm-int? opnd2)
                     (asm-signed32? (x86-imm-int-value opnd2))))
            "move of a 64 bit immediate only possible with register"
            opnd2)

    (if (x86-reg? opnd1)
        (let ((width (x86-reg-width opnd1)))
          (if (and (fx= width 64)
                   (x86-imm-int? opnd2)
                   (asm-signed32? (x86-imm-int-value opnd2)))
              (general-mov-imm width 32)
              (register-mov-imm width)))
        (general-mov-imm width (fxmin width 32))))

  (define (op-imm)

    (define (accumulator-op-imm width)
      (x86-opnd-prefix-opnd cgc width opnd1) ;; prefix
      ;; opcode = #x04, #x0c, #x14, ..., #x3c (for AL)
      ;;       or #x05, #x0d, #x15, ..., #x3d (for AX/EAX/RAX)
      (asm-8 cgc (fx+ (if (fx= width 8) #x04 #x05) ;; opcode
                      (fxarithmetic-shift-left op 3)))
      (listing width
               opnd1
               (x86-imm-encode cgc opnd2 (fxmin 32 width))))

    (define (general-op-imm width)
      (x86-opnd-prefix-opnd cgc width opnd1) ;; prefix
      (cond ((fx= width 8)
             (asm-8 cgc #x80) ;; opcode = 8 bit operation
             (x86-opnd-modrm/sib cgc op opnd1 8) ;; ModR/M
             (listing width
                      opnd1
                      (x86-imm-encode cgc opnd2 8)))
            ((and (x86-imm-int? opnd2)
                  (asm-signed8? (x86-imm-int-value opnd2)))
             (asm-8 cgc #x83) ;; opcode = sign extended 8 bit imm
             (x86-opnd-modrm/sib cgc op opnd1 8) ;; ModR/M
             (listing width
                      opnd1
                      (x86-imm-encode cgc opnd2 8)))
            (else
             (let ((imm-width (fxmin 32 width)))
               (asm-8 cgc #x81) ;; opcode = sign extended 16/32 bit imm
               (x86-opnd-modrm/sib cgc op opnd1 imm-width) ;; ModR/M
               (listing width
                        opnd1
                        (x86-imm-encode cgc opnd2 imm-width))))))

    (assert (if (x86-reg? opnd1)
                (or (not width)
                    (fx= (x86-reg-width opnd1) width))
                width)
            "missing or inconsistent operand width"
            width)

    (if (x86-reg? opnd1)
        (let ((width (x86-reg-width opnd1))
              (field (x86-reg-field opnd1)))
          (if (and (fx= field 0)
                   (or (fx= width 8)
                       (not (x86-imm-int? opnd2))
                       (not (asm-signed8? (x86-imm-int-value opnd2)))))
              (accumulator-op-imm width)
              (general-op-imm width)))
        (general-op-imm width)))

  (define (reg-op reg opnd swapped?)

    (assert (or (not width)
                (fx= (x86-reg-width reg) width))
            "inconsistent operand width"
            width)

    (x86-opnd-prefix-reg-opnd cgc reg opnd) ;; prefix

    (if (and (and #f ;; comment-out for consistency with system assembler
                  (not (x86-64bit-mode? cgc)))
             (fx= op 17)                 ;; move?
             (fx= (x86-reg-field reg) 0) ;; accumulator?
             (x86-mem? opnd)             ;; absolute address
             (x86-mem-abs? opnd))
        (begin
          ;; opcode = #xa0/#xa2 (for 8 bit registers)
          ;;       or #xa1/#xa3 (for 16/32/64 bit registers)
          (asm-8 cgc (fx+ (if swapped? #xa2 #xa0) ;; opcode
                          (if (x86-reg8? reg) 0 1)))
          (asm-32-le cgc (x86-mem-offset opnd)))
        (begin
          (asm-8 cgc (fx+ (fxarithmetic-shift-left op 3) ;; opcode
                          (fx+ (if swapped? 0 2)
                               (if (x86-reg8? reg) 0 1))))
          (x86-opnd-modrm/sib-reg-opnd cgc reg opnd 0))) ;; ModR/M

    (listing (x86-reg-width reg)
             (if swapped? opnd reg)
             (if swapped? reg opnd)))

  (cond ((x86-imm? opnd2)
         (if (fx= op 17) ;; move?
             (mov-imm)
             (op-imm)))
        ((x86-reg? opnd2)
         (reg-op opnd2 opnd1 #t))
        ((x86-reg? opnd1)
         (reg-op opnd1 opnd2 #f))
        (else
         (compiler-internal-error
          "invalid operand combination"
          (list2 opnd1 opnd2)))))

;;;============================================================================

;;; X86 instructions: INC and DEC.

(define (x86-inc cgc opnd)
  (define width #f)
  (x86-inc-dec cgc opnd width #x40))

(define (x86-dec cgc opnd)
  (define width #f)
  (x86-inc-dec cgc opnd width #x48))

(define (x86-inc-dec cgc opnd width op)

  ;; opnd = destination

  (define (listing width)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (if (fx= op #x40) "inc" "dec")
                     width
                     (list1 opnd))))

  (define (register width)
    (x86-opnd-prefix-opnd cgc width opnd) ;; prefix
    (let ((field (x86-reg-field opnd)))
      (asm-8 cgc (fx+ op field))) ;; opcode
    (listing width))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd)                  ;; prefix
    (asm-8 cgc (if (fx= width 8) #xfe #xff))               ;; opcode
    (x86-opnd-modrm/sib cgc (if (fx= op #x40) 0 1) opnd 0) ;; ModR/M
    (listing width))

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width"
          width)

  (if (x86-reg? opnd)
      (let ((width (x86-reg-width opnd)))
        (if (and (not (x86-64bit-mode? cgc))
                 (or (fx= width 16)
                     (fx= width 32)))
            (register width)
            (general width)))
      (general width)))

;;;============================================================================

;;; X86 instruction: LEA.

(define (x86-lea cgc reg opnd)

  ;; reg = destination, opnd = source

  (assert (not (x86-reg8? reg))
          "destination of lea must not be an 8 bit register"
          reg)

  (x86-opnd-prefix-reg-opnd cgc reg opnd)      ;; prefix
  (asm-8 cgc #x8d)                             ;; opcode
  (x86-opnd-modrm/sib-reg-opnd cgc reg opnd 0) ;; ModR/M

  (if (x86-codegen-context-listing-format cgc)
      (x86-listing cgc
                   "lea"
                   (x86-reg-width reg)
                   (list2 reg
                          opnd))))

;;;============================================================================

;;; X86 instruction: RET.

(define (x86-ret cgc n)
  (if (fx= n 0)
      (begin
        (asm-8 cgc #xc3) ;; opcode
        (if (x86-codegen-context-listing-format cgc)
            (x86-listing cgc
                         "ret"
                         0
                         (list0))))
      (begin
        (asm-8 cgc #xc2) ;; opcode
        (asm-16-le cgc n)
        (if (x86-codegen-context-listing-format cgc)
            (x86-listing cgc
                         "ret"
                         0
                         (list1 (x86-imm-int n 0)))))))

;;;============================================================================

;;; X86 instruction: ENTER.

(define (x86-enter cgc size level)
  (let* ((size-opnd (x86-imm-int size 16))
         (level-opnd (x86-imm-int level 8)))
    (asm-8 cgc #xc8) ;; opcode
    (x86-imm-encode cgc size-opnd 16)
    (x86-imm-encode cgc level-opnd 8)

    (if (x86-codegen-context-listing-format cgc)
        (if (eq? 'gnu (x86-codegen-context-listing-format cgc))
            (x86-listing cgc
                         "enter"
                         0
                         (list2 level-opnd
                                size-opnd))
            (x86-listing cgc
                         "enter"
                         0
                         (list2 (number->string size)
                                (number->string level)))))))

;;;============================================================================

;;; X86 instructions: NOP, PUSHF, POPF, LEAVE, HLT, CMC, CLC, STC,
;;; CLI, STI, CLD, and STD.

(define (x86-nop cgc) (x86-no-opnd-instr cgc #x90))
(define (x86-pushf cgc) (x86-no-opnd-instr cgc #x9c))
(define (x86-popf cgc) (x86-no-opnd-instr cgc #x9d))
(define (x86-leave cgc) (x86-no-opnd-instr cgc #xc9))
(define (x86-hlt cgc) (x86-no-opnd-instr cgc #xf4))
(define (x86-cmc cgc) (x86-no-opnd-instr cgc #xf5))
(define (x86-clc cgc) (x86-no-opnd-instr cgc #xf8))
(define (x86-stc cgc) (x86-no-opnd-instr cgc #xf9))
(define (x86-cli cgc) (x86-no-opnd-instr cgc #xfa))
(define (x86-sti cgc) (x86-no-opnd-instr cgc #xfb))
(define (x86-cld cgc) (x86-no-opnd-instr cgc #xfc))
(define (x86-std cgc) (x86-no-opnd-instr cgc #xfd))

(define (x86-no-opnd-instr cgc opcode)

  (asm-8 cgc opcode) ;; opcode

  (if (x86-codegen-context-listing-format cgc)
      (x86-listing cgc
                   (cond ((fx= opcode #x90)
                          "nop")
                         ((fx= opcode #x9c)
                          "pushf")
                         ((fx= opcode #x9d)
                          "popf")
                         ((fx= opcode #xc9)
                          "leave")
                         (else
                          (vector-ref
                           '#("hlt" "cmc" "???" "???" "clc" "stc"
                              "cli" "sti" "cld" "std")
                           (fx- opcode #xf4))))
                   0
                   (list0))))

;;;============================================================================

;;; X86 instruction: INT.

(define (x86-int cgc n)
  (asm-8 cgc #xcd) ;; opcode
  (asm-8 cgc n)
  (if (x86-codegen-context-listing-format cgc)
      (x86-listing cgc
                   "int"
                   0
                   (list1 (x86-imm-int n 0)))))

(define (x86-int3 cgc) ;; one byte encoding for "int 3"
  (asm-8 cgc #xcc) ;; opcode
  (if (x86-codegen-context-listing-format cgc)
      (x86-listing cgc
                   "int"
                   0
                   (list1 (x86-imm-int 3 0)))))

;;;============================================================================

;;; X86 instructions: SYSCALL, SYSRET, WRMSR, RDTSC, RDMSR, RDPMC,
;;; and CPUID.

(define (x86-syscall cgc) (x86-no-opnd-instr-esc cgc #x05))
(define (x86-sysret cgc)  (x86-no-opnd-instr-esc cgc #x07))
(define (x86-wrmsr cgc) (x86-no-opnd-instr-esc cgc #x30))
(define (x86-rdtsc cgc) (x86-no-opnd-instr-esc cgc #x31))
(define (x86-rdmsr cgc) (x86-no-opnd-instr-esc cgc #x32))
(define (x86-rdpmc cgc) (x86-no-opnd-instr-esc cgc #x33))
(define (x86-cpuid cgc) (x86-no-opnd-instr-esc cgc #xa2))

(define (x86-no-opnd-instr-esc cgc opcode)

  (x86-esc-opcode cgc) ;; escape opcode
  (asm-8 cgc opcode)   ;; opcode

  (if (x86-codegen-context-listing-format cgc)
      (x86-listing cgc
                   (cond ((fx= opcode #x05)
                          "syscall")
                         ((fx= opcode #x07)
                          "sysret")
                         ((fx= opcode #xa2)
                          "cpuid")
                         (else ;; (fx<= opcode #x33)
                          (vector-ref
                           '#("wrmsr" "rdtsc" "rdmsr" "rdpmc")
                           (fx- opcode #x30))))
                   0
                   (list0))))

;;;============================================================================

;;; X86 instructions: JMP, CALL, JO, JNO, JB, JAE, JE, JNE, JBE, JA,
;;; JS, JNS, JP, JNP, JL, JGE, JLE, and JG.

;; Unconditional jump/call opcodes

(define x86-jmp-rel8-opcode   #xeb)
(define x86-jmp-rel32-opcode  #xe9)
(define x86-call-rel32-opcode #xe8)

;; Conditional jump opcodes (for the rel32 kind, add #x10 with #x0f opcode)

(define x86-jo-rel8-opcode   #x70)
(define x86-jno-rel8-opcode  #x71)
(define x86-jb-rel8-opcode   #x72)
;;(define x86-jnae-rel8-opcode x86-jb-rel8-opcode)
;;(define x86-jc-rel8-opcode   x86-jb-rel8-opcode)
(define x86-jae-rel8-opcode  #x73)
;;(define x86-jnb-rel8-opcode  x86-jae-rel8-opcode)
;;(define x86-jnc-rel8-opcode  x86-jae-rel8-opcode)
(define x86-je-rel8-opcode   #x74)
;;(define x86-jz-rel8-opcode   x86-je-rel8-opcode)
(define x86-jne-rel8-opcode  #x75)
;;(define x86-jnz-rel8-opcode  x86-jne-rel8-opcode)
(define x86-jbe-rel8-opcode  #x76)
;;(define x86-jna-rel8-opcode  x86-jbe-rel8-opcode)
(define x86-ja-rel8-opcode   #x77)
;;(define x86-jnbe-rel8-opcode x86-ja-rel8-opcode)
(define x86-js-rel8-opcode   #x78)
(define x86-jns-rel8-opcode  #x79)
(define x86-jp-rel8-opcode   #x7a)
;;(define x86-jpe-rel8-opcode  x86-jp-rel8-opcode)
(define x86-jnp-rel8-opcode  #x7b)
;;(define x86-jpo-rel8-opcode  x86-jnp-rel8-opcode)
(define x86-jl-rel8-opcode   #x7c)
;;(define x86-jnge-rel8-opcode x86-jl-rel8-opcode)
(define x86-jge-rel8-opcode  #x7d)
;;(define x86-jnl-rel8-opcode  x86-jge-rel8-opcode)
(define x86-jle-rel8-opcode  #x7e)
;;(define x86-jng-rel8-opcode  x86-jle-rel8-opcode)
(define x86-jg-rel8-opcode   #x7f)
;;(define x86-jnle-rel8-opcode x86-jg-rel8-opcode)

(define (x86-jmp cgc opnd)
  (if (asm-label? opnd)
      (x86-jump-label cgc opnd x86-jmp-rel8-opcode #f)
      (x86-jump-general cgc opnd 4)))

(define (x86-call cgc opnd)
  (if (asm-label? opnd)
      (x86-jump-label cgc opnd x86-call-rel32-opcode #f)
      (x86-jump-general cgc opnd 2)))

(define (x86-jo cgc label)  (x86-jump-label cgc label x86-jo-rel8-opcode #t))
(define (x86-jno cgc label) (x86-jump-label cgc label x86-jno-rel8-opcode #t))
(define (x86-jb cgc label)  (x86-jump-label cgc label x86-jb-rel8-opcode #t))
(define (x86-jae cgc label) (x86-jump-label cgc label x86-jae-rel8-opcode #t))
(define (x86-je cgc label)  (x86-jump-label cgc label x86-je-rel8-opcode #t))
(define (x86-jne cgc label) (x86-jump-label cgc label x86-jne-rel8-opcode #t))
(define (x86-jbe cgc label) (x86-jump-label cgc label x86-jbe-rel8-opcode #t))
(define (x86-ja cgc label)  (x86-jump-label cgc label x86-ja-rel8-opcode #t))
(define (x86-js cgc label)  (x86-jump-label cgc label x86-js-rel8-opcode #t))
(define (x86-jns cgc label) (x86-jump-label cgc label x86-jns-rel8-opcode #t))
(define (x86-jp cgc label)  (x86-jump-label cgc label x86-jp-rel8-opcode #t))
(define (x86-jnp cgc label) (x86-jump-label cgc label x86-jnp-rel8-opcode #t))
(define (x86-jl cgc label)  (x86-jump-label cgc label x86-jl-rel8-opcode #t))
(define (x86-jge cgc label) (x86-jump-label cgc label x86-jge-rel8-opcode #t))
(define (x86-jle cgc label) (x86-jump-label cgc label x86-jle-rel8-opcode #t))
(define (x86-jg cgc label)  (x86-jump-label cgc label x86-jg-rel8-opcode #t))

(define (x86-jump-label cgc label opcode labels-only)

  (define (listing width)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (cond ((fx= opcode x86-jmp-rel8-opcode)
                            "jmp")
                           ((fx= opcode x86-call-rel32-opcode)
                            "call")
                           (else
                            (vector-ref
                             '#("jo" "jno" "jb" "jae" "je" "jne" "jbe" "ja"
                                "js" "jns" "jp" "jnp" "jl" "jge" "jle" "jg")
                             (fx- opcode x86-jo-rel8-opcode))))
                     width
                     (list1 (asm-label-name label)))))

  (define (label-dist label self offset)
    (fx- (asm-label-pos label)
         (fx+ self offset)))

  (assert (or (not labels-only)
              (asm-label? label))
          "Jump on condition on invalid operand"
          label)

  (asm-at-assembly

   cgc

   (list4

    ;; short displacement (-128..127 bytes)

    (if (fx= opcode x86-call-rel32-opcode)
        (lambda (cb self)
          #f)
        (lambda (cb self)
          (let ((dist (label-dist label self 2)))
            (if (asm-signed8? dist)
                2
                #f))))
    (lambda (cb self)
      (let ((dist (label-dist label self 2)))
        (asm-8 cb opcode)           ;; opcode
        (asm-8 cb (fxand 255 dist))) ;; 8 bit distance
      (listing 1))

    ;; 32 bit relative address

    (lambda (cb self)
      (cond ((or (fx= opcode x86-jmp-rel8-opcode)
                 (fx= opcode x86-call-rel32-opcode))
             5)
            (else
             6)))
    (lambda (cb self)
      (let ((dist (label-dist label self 5)))
        (cond ((fx= opcode x86-jmp-rel8-opcode)
               (asm-8 cb x86-jmp-rel32-opcode) ;; opcode
               (asm-32-le cb dist))            ;; 32 bit distance
              ((fx= opcode x86-call-rel32-opcode)
               (asm-8 cb opcode)   ;; opcode
               (asm-32-le cb dist)) ;; 32 bit distance
              (else
               ;; opcode is for a conditional jump
               (x86-esc-opcode cb)         ;; escape opcode
               (asm-8 cb (fx+ #x10 opcode)) ;; opcode = #x80, #x81, etc
               (asm-32-le cb (fx- dist 1))))) ;; 32 bit distance
      (listing 0)))))

(define (x86-jump-general cgc opnd field)

  (assert (or (not (x86-reg? opnd))
              (fx= (x86-reg-width opnd)
                   (x86-word-width cgc)))
          "invalid width register"
          opnd)

  (x86-opnd-prefix cgc 0 0 opnd #f)     ;; prefix (width is implicit)
  (asm-8 cgc #xff)                      ;; opcode
  (x86-opnd-modrm/sib cgc field opnd 0) ;; ModR/M

  (if (x86-codegen-context-listing-format cgc)
      (x86-listing cgc
                   (if (fx= field 4) "jmp" "call")
                   -1
                   (list1 opnd))))

;;;============================================================================

;;; X86 instructions: SETO, SETNO, SETB, SETAE, SETE, SETNE, SETBE, SETA,
;;; SETS, SETNS, SETP, SETNP, SETL, SETGE, SETLE, and SETG.

;; Conditional set opcodes

(define (x86-seto cgc opnd)  (x86-set-cc cgc opnd   0))
(define (x86-setno cgc opnd) (x86-set-cc cgc opnd   1))
(define (x86-setb cgc opnd)  (x86-set-cc cgc opnd   2))
(define (x86-setae cgc opnd) (x86-set-cc cgc opnd   3))
(define (x86-sete cgc opnd)  (x86-set-cc cgc opnd   4))
(define (x86-setne cgc opnd) (x86-set-cc cgc opnd   5))
(define (x86-setbe cgc opnd) (x86-set-cc cgc opnd   6))
(define (x86-seta cgc opnd)  (x86-set-cc cgc opnd   7))
(define (x86-sets cgc opnd)  (x86-set-cc cgc opnd   8))
(define (x86-setns cgc opnd) (x86-set-cc cgc opnd   9))
(define (x86-setp cgc opnd)  (x86-set-cc cgc opnd  10))
(define (x86-setnp cgc opnd) (x86-set-cc cgc opnd  11))
(define (x86-setl cgc opnd)  (x86-set-cc cgc opnd  12))
(define (x86-setge cgc opnd) (x86-set-cc cgc opnd  13))
(define (x86-setle cgc opnd) (x86-set-cc cgc opnd  14))
(define (x86-setg cgc opnd)  (x86-set-cc cgc opnd  15))

(define (x86-set-cc cgc opnd cc)

  (define (listing opnd)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (vector-ref
                      '#("seto" "setno" "setb" "setae"
                         "sete" "setne" "setbe" "seta"
                         "sets" "setns" "setp" "setnp"
                         "setl" "setge" "setle" "setg")
                      cc)
                     (list1 opnd))))

  (x86-esc-opcode cgc)
  (asm-8 cgc (+ #x90 cc))  ;; opcode
  (x86-opnd-prefix cgc 0 0 opnd #f) ;; prefix (width is implicit)
  (x86-opnd-modrm/sib cgc 0 opnd 0) ;; ModR/M
  (listing opnd))

;;;============================================================================

;;; X86 instructions: PUSH and POP.

(define (x86-push cgc opnd)
  (x86-push-pop cgc opnd #x50))

(define (x86-pop cgc opnd)
  (x86-push-pop cgc opnd #x58))

(define (x86-push-pop cgc opnd op) ;; width is always width of stack pointer

  (define (listing opnd)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (if (fx= op #x50) "push" "pop")
                     (x86-word-width cgc)
                     (list1 (if (and #f ;; comment-out to avoid duplicate width qualifier in nasm format
                                     (x86-imm-int? opnd))
                                (x86-imm-int (x86-imm-int-value opnd) 0)
                                opnd)))))

  (define (immediate)
    (if (and (x86-imm-int? opnd)
             (asm-signed8? (x86-imm-int-value opnd)))
        (begin
          (asm-8 cgc #x6a) ;; opcode = #x6a
          (listing (x86-imm-encode cgc opnd 8)))
        (begin
          (asm-8 cgc #x68) ;; opcode = #x68
          (listing (x86-imm-encode cgc opnd 32)))))

  (define (register)
    (let ((field (x86-reg-field opnd)))

      (if (x86-reg32? opnd)
          (begin
            (x86-assert-32bit-mode cgc)
            (assert (fx< field 8)
                    "cannot push/pop extended registers in 32 bit mode"
                    opnd))
          (begin
            (x86-assert-64bit-mode cgc)
            (if (fx>= field 8)
                (asm-8 cgc #x41)))) ;; REX

      (asm-8 cgc (fx+ op ;; opcode = #x50-#x5f
                      (fxand 7 field))))

    (listing opnd))

  (define (general)

    (x86-opnd-prefix cgc 0 0 opnd #f) ;; prefix (width is implicit)

    (let ((field
           (if (fx= op #x50) ;; push?
               (begin
                 (asm-8 cgc #xff) ;; opcode = #xff
                 6)
               (begin
                 (asm-8 cgc #x8f) ;; opcode = #x8f
                 0))))
      (x86-opnd-modrm/sib cgc field opnd 0)) ;; ModR/M

    (listing opnd))

  (cond ((and (fx= op #x50) ;; push?
              (x86-imm? opnd))
         (immediate))
        ((x86-reg? opnd)
         (register))
        (else
         (general))))

;;;============================================================================

;;; X86 instructions: CWDE, CDQ, CBW, CWD, CDQE, and CQO.

(define (x86-cwde cgc) (x86-widen cgc 0))
(define (x86-cdq  cgc) (x86-widen cgc 1))
(define (x86-cbw  cgc) (x86-widen cgc 2))
(define (x86-cwd  cgc) (x86-widen cgc 3))
(define (x86-cdqe cgc) (x86-widen cgc 4))
(define (x86-cqo  cgc) (x86-widen cgc 5))

(define (x86-widen cgc op)

  (define (listing)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (vector-ref
                      '#("cwde" "cdq" "cbw" "cwd" "cdqe" "cqo")
                      op)
                     0 ;; implicit width
                     (list0))))

  (cond ((fx>= op 4)
         (x86-assert-64bit-mode cgc)
         (asm-8 cgc #x48)) ;; REX prefix
        ((fx>= op 2)
         (asm-8 cgc #x66))) ;; operand size override prefix

  (asm-8 cgc (fx+ #x98 (fxand op 1)))

  (listing))

;;;============================================================================

;;; X86 instructions: ROL, ROR, RCL, RCR, SHL, SHR, and SAR.

(define (x86-rol cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 0))

(define (x86-ror cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 1))

(define (x86-rcl cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 2))

(define (x86-rcr cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 3))

(define (x86-shl cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 4))

(define (x86-shr cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 5))

(define (x86-sar cgc opnd1 opnd2)
  (define width #f)
  (x86-shift cgc opnd1 opnd2 width 7))

(define (x86-shift cgc opnd1 opnd2 width op)

  (define (listing width)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (vector-ref
                      '#("rol" "ror" "rcl" "rcr" "shl" "shr" "???" "sar")
                      op)
                     width
                     (list2 opnd1
                            (x86-force-width
                             (if (x86-imm-int? opnd2)
                                 (x86-imm-int (x86-imm-int-value opnd2) width)
                                 opnd2)
                             0)))))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd1) ;; prefix
    (cond ((x86-imm-int? opnd2)
           (let ((n (x86-imm-int-value opnd2)))
             (assert (and (>= n 0)
                          (<= n 255))
                     "immediate shift count out of range"
                     n)
             (asm-8 cgc (fx+ (if (fx= n 1) #xd0 #xc0) ;; opcode
                             (if (fx= width 8) 0 1)))
             (x86-opnd-modrm/sib cgc op opnd1 (if (fx= n 1) 0 8)) ;; ModR/M
             (if (not (fx= n 1))
                 (asm-8 cgc n))))
          ((eqv? opnd2 (x86-cl))
           (asm-8 cgc (fx+ #xd2 ;; opcode
                           (if (fx= width 8) 0 1)))
           (x86-opnd-modrm/sib cgc op opnd1 0)) ;; ModR/M
          (else
           (compiler-internal-error
            "invalid shift count operand"
            opnd2)))
    (listing width))

  (assert (if (x86-reg? opnd1)
              (or (not width)
                  (fx= (x86-reg-width opnd1) width))
              width)
          "missing or inconsistent operand width"
          width)

  (general (or width (x86-reg-width opnd1))))

;;;============================================================================

;;; X86 instructions: NOT and NEG.

(define (x86-not cgc opnd)
  (define width #f)
  (x86-negate cgc opnd width 2))

(define (x86-neg cgc opnd)
  (define width #f)
  (x86-negate cgc opnd width 3))

(define (x86-negate cgc opnd width op)

  (define (listing width)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (if (fx= op 2) "not" "neg")
                     width
                     (list1 opnd))))

  (define (general width)
    (x86-opnd-prefix-opnd cgc width opnd)    ;; prefix
    (asm-8 cgc (if (fx= width 8) #xf6 #xf7)) ;; opcode
    (x86-opnd-modrm/sib cgc op opnd 0)       ;; ModR/M
    (listing width))

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width"
          width)

  (general (or width (x86-reg-width opnd))))

;;;============================================================================

;;; X86 instruction: TEST.

(define (x86-test cgc opnd1 opnd2)

  (define width #f)

  (define (listing width opnd1 opnd2)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     "test"
                     width
                     (list2 opnd1
                            opnd2))))

  (define (accumulator-imm width)
    (x86-opnd-prefix-opnd cgc width opnd1)   ;; prefix
    (asm-8 cgc (if (fx= width 8) #xa8 #xa9)) ;; opcode
    (listing width
             opnd1
             (x86-imm-encode cgc opnd2 (fxmin 32 width))))

  (define (general-imm width)
    (let ((imm-width (fxmin 32 width)))
      (x86-opnd-prefix-opnd cgc width opnd1)     ;; prefix
      (asm-8 cgc (if (fx= width 8) #xf6 #xf7))   ;; opcode
      (x86-opnd-modrm/sib cgc 0 opnd1 imm-width) ;; ModR/M
      (listing width
               opnd1
               (x86-imm-encode cgc opnd2 imm-width))))

  (define (general-reg width)
    (x86-opnd-prefix-reg-opnd cgc opnd2 opnd1)      ;; prefix
    (asm-8 cgc (if (fx= width 8) #x84 #x85))        ;; opcode
    (x86-opnd-modrm/sib-reg-opnd cgc opnd2 opnd1 0) ;; ModR/M
    (listing width
             opnd1
             opnd2))

  (assert (cond ((x86-reg? opnd2)
                 (and (or (not width)
                          (fx= (x86-reg-width opnd2) width))
                      (or (not (x86-reg? opnd1))
                          (fx= (x86-reg-width opnd2) (x86-reg-width opnd1)))))
                ((x86-reg? opnd1)
                 (or (not width)
                     (fx= (x86-reg-width opnd1) width)))
                (else
                 width))
          "missing or inconsistent operand width"
          width)

  (if (x86-reg? opnd2)
      (general-reg (x86-reg-width opnd2))
      (begin
        (assert (x86-imm-int? opnd2)
                "second operand must be a register or an immediate"
                opnd2)
        (if (x86-reg? opnd1)
            (let ((width (x86-reg-width opnd1))
                  (field (x86-reg-field opnd1)))
              (if (fx= field 0)
                  (accumulator-imm width)
                  (general-imm width)))
            (general-imm width)))))

;;;============================================================================

;;; X86 instruction: XCHG.

(define (x86-xchg cgc opnd1 opnd2)

  ;; opnd1 = source/destination1, opnd2 = source/destination2

  (define (listing width reg opnd swapped?)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     "xchg"
                     width
                     (list2 (if swapped? opnd reg)
                            (if swapped? reg opnd)))))

  (define (accumulator-reg width acc reg swapped?)
    (let ((field (x86-reg-field reg)))
      (if (or #t ;; comment-out to avoid encoding "xchg eax,eax" as "nop" on x86-64
              (not (and (fx= width 32)
                        (fx= field 0)
                        (x86-64bit-mode? cgc))))
          (begin
            (x86-opnd-prefix-reg-opnd cgc acc reg) ;; prefix
            (asm-8 cgc (fx+ #x90 (fxand 7 field))) ;; opcode
            (listing width acc reg swapped?))
          (general-reg width acc reg swapped?)))) ;; don't generate nop

  (define (general-reg width reg opnd swapped?)
    (x86-opnd-prefix-reg-opnd cgc reg opnd)      ;; prefix
    (asm-8 cgc (if (fx= width 8) #x86 #x87))     ;; opcode
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd 0) ;; ModR/M
    (listing width reg opnd swapped?))

  (define (reg-op reg opnd swapped?)
    (let ((field (x86-reg-field reg))
          (width (x86-reg-width reg)))
      (if (x86-reg? opnd)
          (let ((field2 (x86-reg-field opnd)))
            (assert (fx= width (x86-reg-width opnd)) "inconsistent width" width)
            (cond ((and (not (fx= width 8)) (fx= field 0)) ;; AX/EAX/RAX
                   (accumulator-reg width reg opnd swapped?))
                  ((and (not (fx= width 8)) (fx= field2 0)) ;; AX/EAX/RAX
                   (accumulator-reg width opnd reg (not swapped?)))
                  (else
                   (if (or #t ;; comment-out for consistency with gnu assembler
                           (not (eq? (x86-codegen-context-listing-format cgc) 'gnu)))
                       (general-reg width reg opnd swapped?)
                       (general-reg width opnd reg (not swapped?))))))
          (general-reg width reg opnd swapped?))))

  (cond ((x86-reg? opnd1)
         (reg-op opnd1 opnd2 #f))
        ((x86-reg? opnd2)
         (reg-op opnd2 opnd1 #t))
        (else
         (compiler-internal-error
          "one of the two operands must be a register"
          (list2 opnd1 opnd2)))))

;;;============================================================================

;;; X86 instructions: MUL, IMUL, DIV, and IDIV.

(define (x86-mul cgc opnd)
  (define width #f)
  (x86-multiply-divide cgc opnd #f #f width 4))

(define (x86-imul1 cgc opnd1)
  (define width #f)
  (x86-multiply-divide cgc opnd1 #f #f width 5))

(define (x86-imul2 cgc opnd1 opnd2)
  (define width #f)
  (x86-multiply-divide cgc opnd1 opnd2 #f width 5))

(define (x86-imul3 cgc opnd1 opnd2 opnd3)
  (define width #f)
  (x86-multiply-divide cgc opnd1 opnd2 opnd3 width 5))

(define (x86-div cgc opnd)
  (define width #f)
  (x86-multiply-divide cgc opnd #f #f width 6))

(define (x86-idiv cgc opnd)
  (define width #f)
  (x86-multiply-divide cgc opnd #f #f width 7))

(define (x86-multiply-divide cgc opnd1 opnd2 opnd3 width op)

  (define (listing width opnd1 opnd2 opnd3)
    (if (x86-codegen-context-listing-format cgc)
        (let ((mnemonic
               (vector-ref '#("mul" "imul" "div" "idiv") (fx- op 4))))
          (cond (opnd3
                 (x86-listing cgc
                              mnemonic
                              width
                              (list3 opnd1
                                     opnd2
                                     opnd3)))
                (opnd2
                 (x86-listing cgc
                              mnemonic
                              width
                              (list2 opnd1
                                     opnd2)))
                (else
                 (x86-listing cgc
                              mnemonic
                              width
                              (list1 opnd1)))))))

  (define (general-1-opnd width)
    (x86-opnd-prefix-opnd cgc width opnd1)    ;; prefix
    (asm-8 cgc (if (fx= width 8) #xf6 #xf7))  ;; opcode
    (x86-opnd-modrm/sib cgc op opnd1 0)       ;; ModR/M
    (listing width opnd1 #f #f))

  (define (register-2-opnds width)
    (assert (not (fx= width 8))
            "8 bit wide multiply not possible"
            width)
    (assert (or (not (x86-reg? opnd2))
                (fx= width (x86-reg-width opnd2)))
            "inconsistent width operands"
            width)
    (x86-opnd-prefix-reg-opnd cgc opnd1 opnd2)      ;; prefix
    (x86-esc-opcode cgc)                            ;; escape opcode
    (asm-8 cgc #xaf)                                ;; opcode
    (x86-opnd-modrm/sib-reg-opnd cgc opnd1 opnd2 0) ;; ModR/M
    (listing width opnd1 opnd2 #f))

  (define (register-3-opnds width)
    (let ((imm-width
           (if (and (x86-imm-int? opnd3)
                    (asm-signed8? (x86-imm-int-value opnd3)))
               8
               (fxmin 32 width))))
      (x86-opnd-prefix-reg-opnd cgc opnd1 opnd2)              ;; prefix
      (asm-8 cgc (if (fx= imm-width 8) #x6b #x69))            ;; opcode
      (x86-opnd-modrm/sib-reg-opnd cgc opnd1 opnd2 imm-width) ;; ModR/M
      (listing width
               opnd1
               opnd2
               (x86-imm-encode cgc opnd3 imm-width))))

  (assert (if (x86-reg? opnd1)
              (or (not width)
                  (fx= (x86-reg-width opnd1) width))
              width)
          "missing or inconsistent operand width"
          width)

  (if (x86-reg? opnd1)
      (let ((width (x86-reg-width opnd1)))
        (cond (opnd3
               (register-3-opnds width))
              (opnd2
               (register-2-opnds width))
              (else
               (general-1-opnd width))))
      (general-1-opnd width)))

;;;============================================================================

;;; X86 instructions: MOVZX, MOVSX, and MOVSXD.

(define (x86-movzx cgc reg opnd)
  (define width #f)
  (x86-move-extended cgc reg opnd width #xb6))

(define (x86-movsx cgc reg opnd)
  (define width #f)
  (x86-move-extended cgc reg opnd width #xbe))

(define (x86-move-extended cgc reg opnd width op)

  ;; reg = destination, opnd = source

  (define (listing width-opnd)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (if (eq? 'gnu (x86-codegen-context-listing-format cgc))
                         (if (fx= width-opnd 32)
                             "movsl"
                             (list2 (if (fx= op #xb6)
                                        "movz"
                                        "movs")
                                    (x86-width-suffix cgc width-opnd)))
                         (if (fx= width-opnd 32)
                             "movsxd"
                             (if (fx= op #xb6)
                                 "movzx"
                                 "movsx")))
                     (x86-reg-width reg)
                     (list2 reg
                            (if (x86-reg? opnd)
                                opnd
                                (x86-force-width opnd width-opnd))))))

  (assert (x86-reg? reg)
          "destination of movzx/movsx/movsxd must be a register"
          reg)

  (assert (not (x86-reg8? reg))
          "destination of movzx/movsx/movsxd must not be an 8 bit register"
          reg)

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width"
          width)

  (let ((width-reg (x86-reg-width reg))
        (width-opnd (or width (x86-reg-width opnd))))

    (assert (or (fx= width-opnd 8)
                (fx= width-opnd 16)
                (and (fx= op #xbe) ;; movsxd?
                     (fx= width-opnd 32)))
            "invalid combination of operands"
            width-opnd)

    (x86-opnd-prefix-reg-opnd cgc reg opnd)    ;; prefix
    (if (fx= width-opnd 32)
        (asm-8 cgc #x63)                       ;; opcode for movsxd
        (begin
          (x86-esc-opcode cgc)                 ;; escape opcode
          (asm-8 cgc (fx+ op (if (fx= width-opnd 8) 0 1))))) ;; opcode
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd 0) ;; ModR/M

    (listing width-opnd)))

;;;============================================================================

;;; X86 instructions: BT, BTS, BTR, BTC, BSF, and BSR.

(define (x86-bt cgc opnd1 opnd2)
  (define width #f)
  (x86-bit-op cgc opnd1 opnd2 width 0)) ;; 0f a3    0f ba /4

(define (x86-bts cgc opnd1 opnd2)
  (define width #f)
  (x86-bit-op cgc opnd1 opnd2 width 1)) ;; 0f ab    0f ba /5

(define (x86-btr cgc opnd1 opnd2)
  (define width #f)
  (x86-bit-op cgc opnd1 opnd2 width 2)) ;; 0f b3    0f ba /6

(define (x86-btc cgc opnd1 opnd2)
  (define width #f)
  (x86-bit-op cgc opnd1 opnd2 width 3)) ;; 0f bb    0f ba /7

(define (x86-bit-op cgc opnd1 opnd2 width op)

  (define (listing width)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (vector-ref
                      '#("bt" "bts" "btr" "btc")
                      op)
                     width
                     (list2 opnd1
                            (if (x86-imm-int? opnd2)
                                (x86-imm-int (x86-imm-int-value opnd2) width)
                                opnd2)))))

  (define (general width)
    (cond ((x86-imm-int? opnd2)
           (let ((n (x86-imm-int-value opnd2)))
             (assert (and (>= n 0)
                          (< n width))
                     "immediate bit position out of range"
                     n)
             (x86-opnd-prefix-opnd cgc width opnd1) ;; prefix
             (x86-esc-opcode cgc)                   ;; escape opcode
             (asm-8 cgc #xba)                       ;; opcode
             (x86-opnd-modrm/sib cgc (fx+ op 4) opnd1 8) ;; ModR/M
             (asm-8 cgc n)))                        ;; immediate bit position
          ((x86-reg? opnd2)
           (x86-opnd-prefix-reg-opnd cgc opnd2 opnd1)     ;; prefix
           (x86-esc-opcode cgc)                           ;; escape opcode
           (asm-8 cgc (fx+ #xa3 (fxarithmetic-shift-left op 3))) ;; opcode
           (x86-opnd-modrm/sib-reg-opnd cgc opnd2 opnd1 0)) ;; ModR/M
          (else
           (compiler-internal-error
            "invalid bit position operand"
            opnd2)))
    (listing width))

  (assert (and (or (not (x86-reg? opnd1))
                   (not width)
                   (fx= (x86-reg-width opnd1) width))
               (or (not (x86-reg? opnd2))
                   (not width)
                   (fx= (x86-reg-width opnd2) width))
               (or (not (x86-reg? opnd1))
                   (not (x86-reg? opnd2))
                   (fx= (x86-reg-width opnd1) (x86-reg-width opnd2))))
          "missing or inconsistent operand width"
          width)

  (cond ((x86-reg? opnd1)
         (general (x86-reg-width opnd1)))
        ((x86-reg? opnd2)
         (general (x86-reg-width opnd2)))
        (else
         (general width))))

;;;============================================================================

;;; X86 instructions: CMOVA, CMOVAE, CMOVB, CMOVBE, CMOVC, CMOVE, CMOVG,
;;; CMOVGE, CMOVL, CMOVLE, CMOVNA, CMOVNAE, CMOVNB, CMOVNBE, CMOVNC, CMOVNE,
;;; CMOVNG, CMOVNGE, CMOVNL, CMOVNLE, CMOVNO, CMOVNP, CMOVNS, CMOVNZ, CMOVO,
;;; CMOVP, CMOVPE, CMOVPO, CMOVS, CMOVZ

; XXX Lazy
(define (x86-cmova cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmova" #x47))
(define (x86-cmovae cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovae" #x43))
(define (x86-cmovb cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovb" #x42))
(define (x86-cmovbe cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovbe" #x46))
(define (x86-cmovc cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovc" #x42))
(define (x86-cmove cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmove" #x44))
(define (x86-cmovg cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovg" #x4f))
(define (x86-cmovge cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovge" #x4d))
(define (x86-cmovl cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovl" #x4c))
(define (x86-cmovle cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovle" #x4e))
(define (x86-cmovna cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovna" #x46))
(define (x86-cmovnae cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnae" #x42))
(define (x86-cmovnb cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnb" #x43))
(define (x86-cmovnbe cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnbe" #x47))
(define (x86-cmovnc cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnc" #x43))
(define (x86-cmovne cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovne" #x45))
(define (x86-cmovng cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovng" #x4e))
(define (x86-cmovnge cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnge" #x4c))
(define (x86-cmovnl cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnl" #x4d))
(define (x86-cmovnle cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnle" #x4f))
(define (x86-cmovno cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovno" #x41))
(define (x86-cmovnp cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnp" #x4b))
(define (x86-cmovns cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovns" #x49))
(define (x86-cmovnz cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovnz" #x45))
(define (x86-cmovo cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovno" #x40))
(define (x86-cmovp cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovp" #x4a))
(define (x86-cmovpe cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovpe" #x4a))
(define (x86-cmovpo cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovpo" #x4b))
(define (x86-cmovs cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovs" #x48))
(define (x86-cmovz cgc reg opnd)
  (define width #f)
  (x86-cmovcc cgc reg opnd width "cmovz" #x44))

(define (x86-cmovcc cgc reg opnd width name op)

  (define (listing width-opnd)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     name
                     0
                     (list2 reg
                            (if (x86-reg? opnd)
                                opnd
                                (x86-force-width opnd width-opnd))))))

  (assert (x86-reg? reg)
          "destination of popcnt must be a register"
          reg)

  (assert (not (x86-reg8? reg))
          "destination of popcnt must not be an 8 bit register"
          reg)

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width"
          width)

  (let ((width-reg (x86-reg-width reg))
        (width-opnd (or width (x86-reg-width opnd))))

    (assert (or (and (fx= width-reg 16) (fx= width-opnd 16))
                (and (fx= width-reg 32) (fx= width-opnd 32))
                (and (fx= width-reg 64) (fx= width-opnd 64)))
            "invalid combination of operands"
            (list2 reg opnd))

    (x86-opnd-prefix-reg-opnd cgc reg opnd)
    (x86-esc-opcode cgc)
    (asm-8 cgc op)
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd 0)

    (listing width-opnd)))

;;;============================================================================

;;; X86 instructions: POPCNT, LZCNT.

(define (x86-popcnt cgc reg opnd)
  (define width #f)
  (x86-count cgc reg opnd width #xb8))

(define (x86-lzcnt cgc reg opnd)
  (define width #f)
  (x86-count cgc reg opnd width #xbd))

(define (x86-count cgc reg opnd width op)

  (define (listing width-opnd)
    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     (if (= op #xb8) "popcnt" "lzcnt")
                     0
                     (list2 reg
                            (if (x86-reg? opnd)
                                opnd
                                (x86-force-width opnd width-opnd))))))

  (assert (x86-reg? reg)
          "destination of popcnt must be a register"
          reg)

  (assert (not (x86-reg8? reg))
          "destination of popcnt must not be an 8 bit register"
          reg)

  (assert (if (x86-reg? opnd)
              (or (not width)
                  (fx= (x86-reg-width opnd) width))
              width)
          "missing or inconsistent operand width"
          width)

  (let ((width-reg (x86-reg-width reg))
        (width-opnd (or width (x86-reg-width opnd))))

    (assert (or (and (fx= width-reg 16) (fx= width-opnd 16))
                (and (fx= width-reg 32) (fx= width-opnd 32))
                (and (fx= width-reg 64) (fx= width-opnd 64)))
            "invalid combination of operands"
            (list2 reg opnd))

    (asm-8 cgc #xf3)
    (x86-opnd-prefix-reg-opnd cgc reg opnd)
    (x86-esc-opcode cgc)
    (asm-8 cgc op)
    (x86-opnd-modrm/sib-reg-opnd cgc reg opnd 0)

    (listing width-opnd)))

;;;============================================================================

;;; X86 instructions: IN, OUT.

(define (x86-in-imm cgc reg n)
  (let ((width-reg (x86-reg-width reg)))
    (assert (and (fx= (x86-reg-field reg) 0) ;; only allow AL, AX, EAX
                 (not (fx= width-reg 64)))
            "invalid register operand"
            reg)

    (x86-opnd-size-override-prefix cgc width-reg)
    (asm-8 cgc (if (fx= width-reg 8) #xe4 #xe5)) ;; opcode
    (asm-8 cgc n)

    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     "in"
                     width-reg
                     (list2 reg
                            (x86-imm-int n 0))))))

(define (x86-in-dx cgc reg)
  (let ((width-reg (x86-reg-width reg)))
    (assert (and (fx= (x86-reg-field reg) 0) ;; only allow AL, AX, EAX
                 (not (fx= width-reg 64)))
            "invalid register operand"
            reg)

    (x86-opnd-size-override-prefix cgc width-reg)
    (asm-8 cgc (if (fx= width-reg 8) #xec #xed)) ;; opcode

    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     "in"
                     width-reg
                     (list2 reg
                            (x86-dx))))))

(define (x86-out-imm cgc n reg)
  (let ((width-reg (x86-reg-width reg)))
    (assert (and (fx= (x86-reg-field reg) 0) ;; only allow AL, AX, EAX
                 (not (fx= width-reg 64)))
            "invalid register operand"
            reg)

    (x86-opnd-size-override-prefix cgc width-reg)
    (asm-8 cgc (if (fx= width-reg 8) #xe6 #xe7)) ;; opcode
    (asm-8 cgc n)

    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     "out"
                     width-reg
                     (list2 (x86-imm-int n 0)
                            reg)))))

(define (x86-out-dx cgc reg)
  (let ((width-reg (x86-reg-width reg)))
    (assert (and (fx= (x86-reg-field reg) 0) ;; only allow AL, AX, EAX
                 (not (fx= width-reg 64)))
            "invalid register operand"
            reg)

    (x86-opnd-size-override-prefix cgc width-reg)
    (asm-8 cgc (if (fx= width-reg 8) #xee #xef)) ;; opcode

    (if (x86-codegen-context-listing-format cgc)
        (x86-listing cgc
                     "out"
                     width-reg
                     (list2 (x86-dx)
                            reg)))))

;;;============================================================================

(define (make-x86-codegen-context arch listing-format)
  (let ((cgc (make-vector (+ (asm-code-block-size) 4))))
    (vector-set! cgc 0 'x86-codegen-context)
    (x86-codegen-context-listing-format-set! cgc listing-format)
    (x86-codegen-context-arch-set! cgc arch)
    (x86-codegen-context-global-labels-set! cgc (make-table))
    (asm-init-code-block cgc 0 'le) ;; little-endian
    (x86-arch-set! cgc arch)
    cgc))

(define (x86-codegen-context-listing-format cgc)
  (vector-ref cgc (+ (asm-code-block-size) 0)))

(define (x86-codegen-context-listing-format-set! cgc x)
  (vector-set! cgc (+ (asm-code-block-size) 0) x))

(define (x86-codegen-context-arch cgc)
  (vector-ref cgc (+ (asm-code-block-size) 1)))

(define (x86-codegen-context-arch-set! cgc x)
  (vector-set! cgc (+ (asm-code-block-size) 1) x))

(define (x86-codegen-context-global-labels cgc)
  (vector-ref cgc (+ (asm-code-block-size) 2)))

(define (x86-codegen-context-global-labels-set! cgc x)
  (vector-set! cgc (+ (asm-code-block-size) 2) x))

(define (x86-codegen-context-specific cgc)
  (vector-ref cgc (+ (asm-code-block-size) 3)))

(define (x86-codegen-context-specific-set! cgc x)
  (vector-set! cgc (+ (asm-code-block-size) 3) x))

;;;============================================================================
