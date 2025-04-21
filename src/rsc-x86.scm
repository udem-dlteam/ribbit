#!/usr/bin/env gsi

;;!#;; satisfy guile

;;; Ribbit Scheme compiler to x86.

;;;----------------------------------------------------------------------------

;; Cherchez le mot "TODO" pour trouver tous les endroits où des
;; modifications sont requises pour l'étape 3.

;;;----------------------------------------------------------------------------

(define debug? #t) ;; set to #t to show expanded code, intermediate code and x86 code

;;;----------------------------------------------------------------------------

;; X86 code generation.

(define prims '())

(define (def-prim name nparams generator)
  (set! prims
    (cons (cons (string->symbol
                 (list->string
                  (cons (integer->char 36) ;; #\$
                        (string->list (symbol->str name)))))
                (cons generator
                      (cons name nparams)))
          prims)))

(def-prim 'id 1 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($id)) (newline)))))

(def-prim 'rib 3 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($rib)) (newline)))
  ;; DONE...

  ;; Allocate 3 field objects on heap using r10 and r11, place evaluated field values and set type tags
  (x86-pop cgc (x86-rcx))
  (x86-pop cgc (x86-rbx))
  (x86-pop cgc (x86-rax))

  ;; Advance r10 by 24 bytes (3 fields of 8 bytes each)
  (x86-add cgc (x86-r10) (x86-imm-int 24 0))            ;; r10 = heap pointer + 24

  ;; Store the field values in the heap
  (x86-mov cgc (x86-mem -24 (x86-r10)) (x86-rax))       ;; field0
  (x86-mov cgc (x86-mem -16 (x86-r10)) (x86-rbx))       ;; field1
  (x86-mov cgc (x86-mem -8 (x86-r10)) (x86-rcx))        ;; field2

  ;; Get the refrence to the rib
  (x86-lea cgc (x86-rax) (x86-mem -24 (x86-r10)))       ;; rax = rib reference
  (x86-or cgc (x86-rax) (x86-imm-int 7 0))              ;; set type tag to 7 : rib
  (x86-push cgc (x86-rax))))                            ;; push rib reference

(def-prim 'rib? 1 (lambda (cgc)
  (define done (asm-make-label* cgc))
  (if debug? (begin (display "#  ") (write '($rib?)) (newline)))
  ;; Check if the value is a rib (the low bits == 7)
  ;; DONE...
  (x86-pop  cgc (x86-rax))
  (x86-and  cgc (x86-rax) (x86-imm-int 7 0))  ;; mask low bits
  (x86-cmp  cgc (x86-rax) (x86-imm-int 7 0))  ;; compare if == 7
  (x86-mov  cgc (x86-rax) (true-value cgc))   ;; result = #t (maybe)
  (x86-je   cgc done)                         ;; rib? if yes done, else false
  (x86-mov  cgc (x86-rax) (false-value cgc))  ;; result = #f
  (x86-label cgc done)
  (x86-push cgc (x86-rax)))) ;; push value

(def-prim 'field0 1 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($field0)) (newline)))
  (x86-pop  cgc (x86-rax))                ;; pop rib reference into rax
  (x86-push cgc (x86-mem -7 (x86-rax))))) ;; push result = field0 of rib

(def-prim 'field1 1 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($field1)) (newline)))
  (x86-pop  cgc (x86-rax))                ;; pop rib reference into rax
  (x86-push cgc (x86-mem 1 (x86-rax)))))  ;; push result = field1 of rib

(def-prim 'field2 1 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($field2)) (newline)))
  (x86-pop  cgc (x86-rax))                ;; pop rib reference into rax
  (x86-push cgc (x86-mem 9 (x86-rax)))))  ;; push result = field2 of rib

(def-prim 'field0-set! 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($field0-set!)) (newline)))
  (x86-pop  cgc (x86-rbx))                        ;; pop val into rbx
  (x86-pop  cgc (x86-rax))                        ;; pop rib reference into rax
  (x86-mov  cgc (x86-mem -7 (x86-rax)) (x86-rbx)) ;; set field0 of rib to val
  (x86-push cgc (x86-rbx))))                      ;; push result = val

(def-prim 'field1-set! 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($field1-set!)) (newline)))
  (x86-pop  cgc (x86-rbx))                        ;; pop val into rbx
  (x86-pop  cgc (x86-rax))                        ;; pop rib reference into rax
  (x86-mov  cgc (x86-mem 1 (x86-rax)) (x86-rbx))  ;; set field1 of rib to val
  (x86-push cgc (x86-rbx))))                      ;; push result = val

(def-prim 'field2-set! 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($field2-set!)) (newline)))
  (x86-pop  cgc (x86-rbx))                        ;; pop val into rbx
  (x86-pop  cgc (x86-rax))                        ;; pop rib reference into rax
  (x86-mov  cgc (x86-mem 9 (x86-rax)) (x86-rbx))  ;; set field2 of rib to val
  (x86-push cgc (x86-rbx))))                      ;; push result = val

(def-prim 'eqv? 2 (lambda (cgc)
  (define done (asm-make-label* cgc))
  (if debug? (begin (display "#  ") (write '($eqv?)) (newline)))
  (x86-pop  cgc (x86-rbx))                   ;; pop y into rbx
  (x86-pop  cgc (x86-rax))                   ;; pop x into rax
  (x86-cmp  cgc (x86-rax) (x86-rbx))         ;; compare x and y
  (x86-mov  cgc (x86-rax) (true-value cgc))  ;; result = #t (maybe)
  (x86-je   cgc done)                        ;; x == y?
  (x86-mov  cgc (x86-rax) (false-value cgc)) ;; result = #f
  (x86-label cgc done)
  (x86-push cgc (x86-rax))))                 ;; push result

(def-prim '< 2 (lambda (cgc)
  (define done (asm-make-label* cgc))
  (if debug? (begin (display "#  ") (write '($<)) (newline)))
  ;; DONE...
  (x86-pop  cgc (x86-rbx))                         ;; pop y into rbx
  (x86-pop  cgc (x86-rax))                         ;; pop x into rax
  ;; check if fixnums x < y
  (x86-sar  cgc (x86-rax) (x86-imm-int 3 0))       ;; untag x
  (x86-sar  cgc (x86-rbx) (x86-imm-int 3 0))       ;; untag y

  (x86-cmp  cgc (x86-rax) (x86-rbx))               ;; compare x and y
  (x86-mov  cgc (x86-rax) (true-value cgc))        ;; set result = #t (truthy)

  (x86-jl   cgc done)                              ;; jump to fin if x < y
  (x86-mov  cgc (x86-rax) (false-value cgc))       ;; else, set result = #f (false)
  (x86-label cgc done)

  (x86-push cgc (x86-rax))))                       ;; push result

(def-prim '+ 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($+)) (newline)))
  (x86-pop  cgc (x86-rbx))                         ;; pop y into rbx
  (x86-add  cgc (x86-mem 0 (x86-rsp)) (x86-rbx)))) ;; add it to x (TOS)

(def-prim '- 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($-)) (newline)))
  (x86-pop  cgc (x86-rbx))                         ;; pop y into rbx
  (x86-sub  cgc (x86-mem 0 (x86-rsp)) (x86-rbx)))) ;; substract it from x (TOS)

(def-prim '* 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($*)) (newline)))
  ;; DONE...
  (x86-pop  cgc (x86-rbx))                       ;; pop y into rbx
  (x86-pop  cgc (x86-rax))                       ;; pop x into rax
  (x86-sar  cgc (x86-rax) (x86-imm-int 3 0))     ;; untag x
  (x86-sar  cgc (x86-rbx) (x86-imm-int 3 0))     ;; untag y
  (x86-imul2 cgc (x86-rax) (x86-rbx))             ;; multiply x by y
  (x86-shl  cgc (x86-rax) (x86-imm-int 3 0))     ;; tag result as fixnum
  (x86-push cgc (x86-rax))))                     ;; push result

(def-prim 'quotient 2 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($quotient)) (newline)))
  (x86-pop  cgc (x86-rbx))                   ;; pop y into rbx
  (x86-pop  cgc (x86-rax))                   ;; pop x into rax
  (x86-cqo  cgc)                             ;; sign extend rax to rdx:rax
  (x86-idiv cgc (x86-rbx))                   ;; divide rdx:rax by rbx
  (x86-shl  cgc (x86-rax) (x86-imm-int 3 0)) ;; tag result as fixnum
  (x86-push cgc (x86-rax))))                 ;; push result = floor(x/y)

(def-prim 'getchar 0 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($getchar)) (newline)))
  (x86-call cgc (x86-global-label cgc 'getchar)) ;; call RTS getchar routine
  (x86-shl  cgc (x86-rax) (x86-imm-int 3 0))     ;; tag result as fixnum
  (x86-push cgc (x86-rax))))                     ;; push result

(def-prim 'putchar 1 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($putchar)) (newline)))
  (x86-mov  cgc (x86-rax) (x86-mem 0 (x86-rsp)))   ;; get x into rax without pop
  (x86-sar  cgc (x86-rax) (x86-imm-int 3 0))       ;; untag x
  (x86-push cgc (x86-rax))                         ;; prepare call to putchar
  (x86-call cgc (x86-global-label cgc 'putchar)))) ;; call RTS putchar routine

(def-prim 'exit 1 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($exit)) (newline)))
  (x86-pop  cgc (x86-rax))                      ;; pop x into rax
  (x86-sar  cgc (x86-rax) (x86-imm-int 3 0))    ;; untag x
  (x86-push cgc (x86-rax))                      ;; prepare call to exit
  (x86-call cgc (x86-global-label cgc 'exit)))) ;; call RTS exit routine

(def-prim 'print_regs 0 (lambda (cgc)
  (if debug? (begin (display "#  ") (write '($print_regs)) (newline)))
  (x86-call cgc (x86-global-label cgc 'print_regs))
  (x86-push cgc (x86-imm-int 0 0))))

(define (build-primitives)
  (map (lambda (prim)
         (let ((name (caddr prim)))
           (if (eqv? name 'rib)
               0 ;; don't define rib because it is predefined by runtime system
               (let ((params (build-params 1 (cdddr prim))))
                 (cons 'define
                       (cons name
                             (cons (cons 'lambda
                                         (cons params
                                               (cons (cons (car prim) params)
                                                     '())))
                                   '())))))))
       prims))

(define (build-params i n)
  (if (<= i n)
      (cons (string->symbol
             (list->string
              (cons (integer->char 36) ;; #\$
                    (string->list (number->string i)))))
            (build-params (+ i 1) n))
      '()))

(define (gen-prim cgc name) ;; execute primitive on values on top of stack
  (let ((p (assv name prims)))
    (and p
         ((cadr p) cgc))))

(define (gen-push-lit cgc val)
  (if debug? (begin (display "#  ") (write (cons 'push-lit (cons val '()))) (newline)))
  (if (and (integer? val)
           (not (asm-signed32? (* 8 val))))
      (begin
        (x86-mov  cgc (x86-rax) (x86-imm-int (* 8 val) 0))
        (x86-push cgc (x86-rax)))
      (x86-push cgc (encode-value cgc val)))) ;; push literal value val

(define (gen-push-loc cgc i)
  (if debug? (begin (display "#  ") (write (cons 'push-loc (cons i '()))) (newline)))
  (x86-push cgc (x86-mem (* 8 i) (x86-rsp)))) ;; push stack slot i

(define (gen-move cgc k n)
  (if debug? (begin (display "#  ") (write (cons 'move (cons k (cons n '())))) (newline)))
  (let loop ((i (- k 1)))
    (if (< i 0)
        (x86-add cgc (x86-rsp) (x86-imm-int (* 8 n) 0)) ;; shift sp by n slots
        (begin
          (x86-mov cgc (x86-rax) (x86-mem (* 8 i) (x86-rsp)))       ;; get slot
          (x86-mov cgc (x86-mem (* 8 (+ i n)) (x86-rsp)) (x86-rax)) ;; copy it
          (loop (- i 1))))))

(define (gen-proc cgc lbl)
  (asm-align cgc 8 0 0) ;; ensure label is on an address that is multiple of 8
  (gen-label cgc lbl))

(define (gen-push-proc cgc lbl)
  (if debug? (begin (display "#  ") (write (cons 'push-proc (cons (asm-label-id lbl) '()))) (newline)))
  (x86-lea  cgc (x86-rax) lbl) ;; get address of label
  (x86-push cgc (x86-rax)))    ;; push it

(define (gen-retaddr cgc lbl)
  (asm-align cgc 8 0 0) ;; ensure label is on an address that is multiple of 8
  (gen-label cgc lbl))

(define (gen-push-ra cgc lbl)
  (if debug? (begin (display "#  ") (write (cons 'push-ra (cons (asm-label-id lbl) '()))) (newline)))
  (x86-lea  cgc (x86-rax) lbl) ;; get address of label
  (x86-push cgc (x86-rax)))    ;; push it

; la convention d'appel est la suivante selon Leo/Marc:
; empiler f (une fermeture)
; empiler a
; empiler b
; empiler l'adresse de retour
; jump 2

; le jump va lire la fermeture dans rdi et va brancher à l'adresse se trouvant dans le field0 de ce rib (le point d'entrée de cette fermeture)
; ce point d'entrée sait que sur la pile il y a, en ordre, la fermeture + les 2 paramètre + l'adresse de retour
; donc le code de la fermeture pourra accéder à la fermeture pour lire les variables libres (à travers la variable $self, vu que la procedure est sur le stack)

; (define (gen-jump cgc nargs)
;   (define valid (asm-make-label* cgc))
;   (define invalid (asm-make-label* cgc))
;   (if debug? (begin (display "#  ") (write (cons 'jump (cons nargs '()))) (newline)))
;   ;; Stack : [return addr] [arg1] ... [argn] [closure]        closure @ argn + 1

;   ;; Do I need to untag the closure????
;   ;; The pointer to the rib is stored as the address of the rib + 7 (0xb111)
;     (x86-mov  cgc (x86-rax) (x86-mem (* 8 (+ nargs 1)) (x86-rsp)))  ;; get closure from stack into rax
;     (x86-mov  cgc (x86-rbx) (x86-rax))
;     (x86-and  cgc (x86-rbx) (x86-imm-int 7 0))       ;; mask low bits
;     (x86-cmp cgc (x86-rbx) (x86-imm-int 7 0)) ;; get the type tag
;     (x86-je cgc valid) ;; check if the closure is valid

;     (x86-label cgc invalid) ;; invalid closure
;     (x86-call cgc (x86-global-label cgc 'exit)) ;; call exit (Debug)

;     (x86-label cgc valid) ;; valid closure
;     (x86-mov  cgc (x86-rdi) (x86-rax)) ;; get the tagged code ptr of the closure
;     (x86-and cgc (x86-rdi) (x86-imm-int -8 0))              ;; untag the code ptr
;     (x86-mov cgc (x86-mem (* 8 (+ nargs 1)) (x86-rsp)) (x86-rax)) ;; move the closure to the top of the stack
;     (x86-jmp cgc (x86-mem 0 (x86-rdi))))                 ;; jump to closure

(define (gen-jump cgc nargs)
  (define valid   (asm-make-label* cgc))
  (define invalid (asm-make-label* cgc))
  (if debug? (begin (display "#  ") (write (cons 'jump (cons nargs '()))) (newline)))


  (let ((bytes (* 8 (+ nargs 1))))    ;; size of [args... closure] in bytes

    (x86-mov cgc (x86-rax)
              (x86-mem bytes (x86-rsp)))

    (x86-mov cgc (x86-rbx) (x86-rax))
    (x86-and cgc (x86-rbx) (x86-imm-int 7 0))
    (x86-cmp cgc (x86-rbx) (x86-imm-int 7 0))
    (x86-je  cgc valid)

    (x86-label cgc invalid)
    (x86-call  cgc (x86-global-label cgc 'exit))

    (x86-label cgc valid)

    (x86-mov  cgc (x86-rdi) (x86-rax)) ;; get the tagged code ptr of the closure

    (x86-and  cgc (x86-rdi) (x86-imm-int -8 0))

    (x86-mov  cgc (x86-rdi) (x86-mem 0 (x86-rdi)))

    (x86-mov  cgc
              (x86-mem bytes (x86-rsp))
              (x86-rax))

    ; (x86-add  cgc (x86-rsp) (x86-imm-int bytes 0))

    (x86-jmp  cgc (x86-rdi))))


(define (gen-ret cgc i n)
  (if debug? (begin (display "#  ") (write (cons 'ret (cons i (cons n '())))) (newline)))
  (x86-mov  cgc (x86-rdi) (x86-mem (* 8 i) (x86-rsp)))   ;; get return addr
  (x86-mov  cgc (x86-rax) (x86-mem 0 (x86-rsp)))         ;; get return value
  (x86-add  cgc (x86-rsp) (x86-imm-int (* 8 (+ n 1)) 0)) ;; pop frame
  (x86-push cgc (x86-rax))                               ;; push return value
  (x86-jmp  cgc (x86-rdi)))                              ;; jump to ret addr

(define (gen-iffalse cgc lbl)
  (if debug? (begin (display "#  ") (write (cons 'iffalse (cons (asm-label-id lbl) '()))) (newline)))
  ;; DONE...
  ;; Jump to target label if the value on top of the stack is false
  (x86-pop cgc (x86-rax) )                                ;; pop value into rax
  (x86-cmp  cgc (x86-rax) (false-value cgc))              ;; compare with false
  (x86-je   cgc lbl))                                     ;; jump to target label

(define (gen-goto cgc lbl)
  (if debug? (begin (display "#  ") (write (cons 'goto (cons (asm-label-id lbl) '()))) (newline)))
  (x86-jmp cgc lbl)) ;; branch to lbl

(define (gen-label cgc lbl)
  (if debug? (begin (display "# ") (display (asm-label-id lbl)) (display ":\n")))
  (x86-label cgc lbl)) ;; branch to lbl

;;;----------------------------------------------------------------------------

(define heap-size-MB 400) ;; 200 MB is sufficient for bootstrap, use twice that

(define (gen-program-entry cgc)
  (x86-label cgc (x86-global-label cgc 'entry))
  (x86-call cgc (x86-global-label cgc 'setup)) ;; setup execution context
;;  (x86-call cgc (x86-global-label cgc 'print_regs))
)

(define (gen-program-exit cgc)
;;  (x86-call cgc (x86-global-label cgc 'print_regs))
  (x86-pop cgc (x86-rax))                         ;; pop program's result
  (x86-mov cgc (x86-rax) (x86-imm-int (* 8 0) 0)) ;; replace with fixnum 0
  (x86-sar cgc (x86-rax) (x86-imm-int 3 0))       ;; untag result
  (x86-ret cgc 0))                                ;; terminate program

(define (false-value cgc) (encode-value cgc #f))
(define (true-value cgc)  (encode-value cgc #t))
(define (nil-value cgc)   (encode-value cgc '()))

(define (encode-value cgc val)
  (if (integer? val)
      (x86-imm-int (* 8 val) 0)
      (encode-value-rib cgc val)))

(define (encode-value-rib cgc val)
  (let ((literals (x86-codegen-context-specific cgc)))
    (or (table-ref literals val #f)
        (let ((opnd (get-lit (table-length literals))))
          (table-set! literals val opnd)
          opnd))))

(define (get-lit i)
  (x86-mem (* 8 i) (x86-r11)))

(cond-expand
  (ribbit) ;; These definitions already exist in the Ribbit library
  (else
   (define pair-type      0)
   (define procedure-type 1)
   (define symbol-type    2)
   (define string-type    3)
   (define vector-type    4)
   (define singleton-type 5)))

(define (gen-program-setup cgc exports live-global-vars)

  (define done (make-table)) ;; literals that are setup

  (define alloc_heap (asm-make-label cgc 'alloc_heap))

  (define make_rib (asm-make-label cgc 'make_rib))

  (define symbol-table (and (assv 'symtbl live-global-vars) '()))

  (define (build-rib val opnd field0 field1 field2)
    (let* ((f0 (build-value cgc field0 #f))
           (f1 (build-value cgc field1 #f))
           (f2 (build-value cgc field2 #f))
           (opnd (or opnd (encode-value-rib cgc val))))
      (x86-push cgc (x86-imm-int 0 0)) ;; dummy procedure
      (x86-push cgc f0)
      (x86-push cgc f1)
      (x86-push cgc f2)
      (x86-call cgc make_rib) ;; note: return address is not tagged
      (x86-pop  cgc opnd)
      (table-set! done val opnd)
      opnd))

  (define (build-value cgc val opnd)
    (or (table-ref done val #f)
        (cond ((pair? val)
               (build-rib val
                          opnd
                          (car val)
                          (cdr val)
                          pair-type))
              ((symbol? val)
               (let ((name (exported-name val exports)))
                 (if (and symbol-table name)
                     (set! symbol-table (cons val symbol-table)))
                 (build-rib val
                            opnd
                            0 ;; global variable value
                            (if name (symbol->str name) "")
                            symbol-type)))
              ((string? val)
               (build-rib val
                          opnd
                          (map char->integer (string->list val))
                          (string-length val)
                          string-type))
              ((vector? val)
               (build-rib val
                          opnd
                          (vector->list val)
                          (vector-length val)
                          vector-type))
              ((or (eqv? val #f) (eqv? val #t) (eqv? val '()))
               (build-rib val
                          opnd
                          0
                          0
                          singleton-type))
              ((integer? val)
               (x86-imm-int (* 8 val) 0))
              (else
               (error "cannot build value" val)))))

  (x86-label cgc (x86-global-label cgc 'setup))

  (x86-call cgc alloc_heap)  ;; allocate heap

  ;; build literals

  (for-each (lambda (val-opnd)
              (let* ((val (car val-opnd))
                     (opnd (cdr val-opnd)))
                (build-value cgc val opnd)))
            (table->list (x86-codegen-context-specific cgc)))

  (if (assv 'rib live-global-vars)
      (let* ((rib-sym-opnd
              (build-value cgc 'rib #f))
             (symtbl-opnd
              (if symbol-table
                  (build-value cgc symbol-table #f)
                  (x86-imm-int 0 0))))
        (x86-push cgc (x86-imm-int 0 0)) ;; dummy procedure
        (gen-push-proc cgc make_rib) ;; field0 of rib procedure
        (x86-push cgc symtbl-opnd)   ;; field1 is symbol table
        (x86-push cgc (x86-imm-int (* 8 procedure-type) 0)) ;; field2 = type
        (x86-call cgc make_rib)
        (x86-mov  cgc (x86-rax) rib-sym-opnd) ;; get rib symbol
        (x86-pop  cgc (x86-mem -7 (x86-rax))))) ;; set rib global var (field0)

  (x86-ret cgc 0)

  ;; rib primitive

  (gen-label cgc make_rib)
  (gen-push-loc cgc 3) ;; field0
  (gen-push-loc cgc 3) ;; field1
  (gen-push-loc cgc 3) ;; field2
  (gen-prim cgc '$rib)
  (gen-ret cgc 1 5)

  ;; allocate heap

  (x86-label cgc alloc_heap)

  (x86-mov  cgc (x86-rax) (x86-imm-int heap-size-MB 0))
  (x86-shl  cgc (x86-rax) (x86-imm-int 20 0))  ;; convert to MB
  (x86-push cgc (x86-rax))                     ;; parameter = heap size
  (x86-call cgc (x86-global-label cgc 'mmap))  ;; allocate heap
  (x86-mov  cgc (x86-r11) (x86-rax))           ;; r11 = address of heap start
  (x86-mov  cgc (x86-r10) (x86-r11))           ;; r10 = allocation pointer

  ;; reserve space for literals
  (x86-add  cgc (x86-r10) (x86-imm-int (* 8 (table-length done)) 0))

  ;; TODO...
  ;; Il y a possiblement des modifications à faire ici si la gestion
  ;; mémoire est modifiée, par exemple l'ajout d'un garbage collector.

  (x86-ret cgc 0))

(define (exported-name sym exports)
  (let ((x (assv sym exports)))
    (and x (cdr x))))

;;;----------------------------------------------------------------------------

;; Compiler back-end.

(define (back-end expr-and-exports-and-live-global-vars)
  (let* ((expr
          (car expr-and-exports-and-live-global-vars))
         (exports
          (cadr expr-and-exports-and-live-global-vars))
         (live-global-vars
          (cddr expr-and-exports-and-live-global-vars)))

    (define arch 'x86-64)
    (define listing-format (and debug? 'gnu))

    (if debug?
        (begin
          (display "# ") (write expr) (newline)
          (display "# ") (write exports) (newline)))

    (let ((cgc (make-x86-codegen-context arch listing-format)))

      (x86-codegen-context-specific-set! cgc (make-table)) ;; program literals

      (x86-make-global-label cgc 'entry)
      (x86-make-global-label cgc 'setup)
      (x86-make-global-label cgc 'putchar)
      (x86-make-global-label cgc 'getchar)
      (x86-make-global-label cgc 'print_regs)
      (x86-make-global-label cgc 'print_string)
      (x86-make-global-label cgc 'print_word_hex)
      (x86-make-global-label cgc 'mmap)
      (x86-make-global-label cgc 'exit)

      (gen-program-entry cgc)
      (gen-expr cgc expr '() #f)
      (gen-program-exit cgc)
      (gen-rts cgc)
      (gen-program-setup cgc exports live-global-vars)

      (let ((code (asm-assemble-to-u8vector cgc)))
        (if listing-format (asm-display-listing cgc #t))
        (display-gnu-assembler-bytes code)))))

(define (display-gnu-assembler-bytes bytes)
  (let ((cgc (make-x86-codegen-context 'x86-64 'gnu)))
    (x86-db cgc (u8vector->list bytes))
    (display "\t.globl main,_main # export entry point for linking\n")
    (display "\t.p2align 3 # align to multiple of 8 = 2**3\n")
    (display "main:\n")
    (display "_main:\n")
    (asm-display-listing cgc #f)))

;;;----------------------------------------------------------------------------

;; Expression compilation.

(define (gen-expr cgc expr cte tail?)

  (cond ((symbol? expr)
         (let* ((var expr)
                (i (- (length cte) (length (memv var cte)))))
           (gen-push-loc cgc i)
           (gen-ret-maybe cgc cte tail?)))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (gen-push-lit cgc (cadr expr))
                  (gen-ret-maybe cgc cte tail?))

                 ((eqv? first 'if)
                  (let ((false (asm-make-label* cgc)))
                    (gen-expr cgc (cadr expr) cte #f)
                    (gen-iffalse cgc false)
                    (gen-expr cgc (caddr expr) cte tail?)
                    (if tail?
                        (begin
                          (gen-label cgc false)
                          (gen-expr cgc (cadddr expr) cte tail?))
                        (let ((join (asm-make-label* cgc)))
                          (gen-goto cgc join)
                          (gen-label cgc false)
                          (gen-expr cgc (cadddr expr) cte tail?)
                          (gen-label cgc join)))))

                 ((eqv? first 'begin)
                  (gen-begin cgc (cdr expr) cte tail?))

                 ((eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (body (cddr expr))
                         (cte* (cons #t (reverse params))) ;; #t marks ret addr
                         (entry (asm-make-label* cgc)))
                    (gen-push-proc cgc entry) ;; push address of code
                    (if tail?
                        (begin
                          (gen-ret-maybe cgc cte tail?)
                          (gen-label cgc entry)
                          (gen-begin cgc body cte* #t))
                        (let ((cont (asm-make-label* cgc)))
                          (gen-goto cgc cont) ;; need to skip the lambda body
                          (gen-label cgc entry)
                          (gen-begin cgc body cte* #t)
                          (gen-label cgc cont)))))

                 ((eqv? first 'let)
                  (let* ((bindings (cadr expr))
                         (body (cddr expr)))
                    (let loop ((lst bindings) (cte cte) (cte* cte))
                      (if (pair? lst)
                          (let* ((binding (car lst))
                                 (var (car binding))
                                 (val (cadr binding)))
                            (gen-expr cgc val cte #f)
                            (loop (cdr lst)
                                  (cons #f cte)
                                  (cons var cte*)))
                          (begin
                            (gen-begin cgc body cte* tail?)
                            (if (not tail?)
                                (gen-move cgc 1 (length bindings))))))))

                 (else
                  (let ((p (assv first prims)))
                    (if p ;; calling a primitive?
                        (gen-list cgc
                                  (cdr expr)
                                  cte
                                  (lambda (cte*)
                                    ((cadr p) cgc) ;; inline the primitive
                                    (gen-ret-maybe cgc cte tail?)))
                        (gen-list cgc
                                  expr
                                  cte
                                  (lambda (cte*)
                                    (let ((nargs (length (cdr expr))))
                                      (if #f ;;tail?
                                          (begin
                                            ;; TODO...
                                            ;; appel terminal
                                            #f)
                                          (let ((ra (asm-make-label* cgc)))
                                            (gen-push-ra cgc ra)
                                            (gen-jump cgc nargs)
                                            (gen-label cgc ra)
                                            (gen-ret-maybe cgc cte tail?))))))))))))

        (else
         (gen-push-lit cgc expr) ;; self-evaluating
         (gen-ret-maybe cgc cte tail?))))

(define (gen-list cgc exprs cte cont)
  (if (pair? exprs)
      (begin
        (gen-expr cgc (car exprs) cte #f)
        (gen-list cgc (cdr exprs) (cons #f cte) cont))
      (cont cte)))

(define (gen-ret-maybe cgc cte tail?)
  (if tail?
      (let* ((fs (+ (length cte) 1))            ;; frame size
             (i (- fs (length (memv #t cte))))) ;; position of return address
        (gen-ret cgc i (- fs 1)))))

(define (gen-begin cgc exprs cte tail?)
  (if (pair? (cdr exprs))
      (begin
        (gen-expr cgc (car exprs) cte #f)
        (gen-move cgc 0 1) ;; pop value
        (gen-begin cgc (cdr exprs) cte tail?))
      (gen-expr cgc (car exprs) cte tail?)))

;;;----------------------------------------------------------------------------

;; Expansion of derived forms, like "define", "cond", "and", "or".

(define (expand-expr expr)

  (cond ((symbol? expr)
         expr)

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (expand-constant (cadr expr)))

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (cons 'set!
                          (cons var
                                (cons (expand-expr (caddr expr))
                                      '())))))

                 ((eqv? first 'if)
                  (cons 'if
                        (cons (expand-expr (cadr expr))
                              (cons (expand-expr (caddr expr))
                                    (cons (if (pair? (cdddr expr))
                                              (expand-expr (cadddr expr))
                                              #f)
                                          '())))))

                 ((eqv? first 'lambda)
                  (let ((params (cadr expr)))
                    (cons 'lambda
                          (cons params
                                (cons (expand-body (cddr expr))
                                      '())))))

                 ((eqv? first 'let)
                  (let ((x (cadr expr)))
                    (if (symbol? x) ;; named let?
                        (expand-expr
                         (let ((bindings (caddr expr)))
                           (cons
                            (cons
                             'letrec
                             (cons (cons
                                    (cons x
                                          (cons (cons 'lambda
                                                      (cons (map car bindings)
                                                            (cdddr expr)))
                                                '()))
                                    '())
                                   (cons x
                                         '())))
                            (map cadr bindings))))
                        (let ((bindings x))
                          (if (pair? bindings)
                              (cons 'let
                                    (cons (map (lambda (binding)
                                                 (cons (car binding)
                                                       (cons (expand-expr
                                                              (cadr binding))
                                                             '())))
                                               bindings)
                                          (cons (expand-body (cddr expr))
                                                '())))
                              (expand-body (cddr expr)))))))

                 ((eqv? first 'let*)
                  (let ((bindings (cadr expr)))
                    (expand-expr
                     (cons 'let
                           (if (and (pair? bindings) (pair? (cdr bindings)))
                               (cons (cons (car bindings) '())
                                     (cons (cons 'let*
                                                 (cons (cdr bindings)
                                                       (cddr expr)))
                                           '()))
                               (cdr expr))))))

                 ((eqv? first 'letrec)
                  (let ((bindings (cadr expr)))
                    (expand-expr
                     (cons 'let
                           (cons (map (lambda (binding)
                                        (cons (car binding) (cons #f '())))
                                      bindings)
                                 (union (map (lambda (binding)
                                                (cons 'set!
                                                      (cons (car binding)
                                                            (cons (cadr binding)
                                                                  '()))))
                                              bindings)
                                         (cddr expr)))))))

                 ((eqv? first 'begin)
                  (expand-begin (cdr expr)))

                 ((eqv? first 'define)
                  (let ((pattern (cadr expr)))
                    (if (pair? pattern)
                        (cons 'set!
                              (cons (car pattern)
                                    (cons (expand-expr
                                           (cons 'lambda
                                                 (cons (cdr pattern)
                                                       (cddr expr))))
                                          '())))
                        (cons 'set!
                              (cons pattern
                                    (cons (expand-expr (caddr expr))
                                          '()))))))

                 ((eqv? first 'and)
                  (expand-expr
                   (if (pair? (cdr expr))
                       (if (pair? (cddr expr))
                           (cons 'if
                                 (cons (cadr expr)
                                       (cons (cons 'and
                                                   (cddr expr))
                                             (cons #f
                                                   '()))))
                           (cadr expr))
                       #t)))

                 ((eqv? first 'or)
                  (expand-expr
                   (if (pair? (cdr expr))
                       (if (pair? (cddr expr))
                           (cons
                            'let
                            (cons
                             (cons (cons '$
                                         (cons (cadr expr)
                                               '()))
                                   '())
                             (cons
                              (cons 'if
                                    (cons '$
                                          (cons '$
                                                (cons (cons 'or
                                                            (cddr expr))
                                                      '()))))
                              '())))
                           (cadr expr))
                       #f)))

                 ((eqv? first 'cond)
                  (expand-expr
                   (if (pair? (cdr expr))
                       (if (eqv? 'else (car (cadr expr)))
                           (cons 'begin (cdr (cadr expr)))
                           (cons 'if
                                 (cons (car (cadr expr))
                                       (cons (cons 'begin
                                                   (cdr (cadr expr)))
                                             (cons (cons 'cond
                                                         (cddr expr))
                                                   '())))))
                       #f)))

                 (else
                  (expand-list expr)))))

        (else
         (expand-constant expr))))

(define (extend vars cte)
  (if (pair? vars)
      (cons (car vars) (extend (cdr vars) cte))
      cte))

(define (expand-constant x)
  (list2 'quote x))

(define (expand-body exprs)
  (let loop ((exprs exprs) (defs '()))
    (if (pair? exprs)
        (let ((expr (car exprs)))
          (if (and (pair? expr) (eqv? 'define (car expr)) (pair? (cdr expr)))
              (let ((pattern (cadr expr)))
                (if (pair? pattern)
                    (loop (cdr exprs)
                          (cons (cons (car pattern)
                                      (cons (cons 'lambda
                                                  (cons (cdr pattern)
                                                        (cddr expr)))
                                            '()))
                                defs))
                    (loop (cdr exprs)
                          (cons (cons pattern
                                      (cddr expr))
                                defs))))
              (expand-body-done defs exprs)))
        (expand-body-done defs (cons (expand-constant 0) '())))))

(define (expand-body-done defs exprs)
  (if (pair? defs)
      (expand-expr
       (cons 'letrec
             (cons (reverse defs)
                   exprs)))
      (expand-begin exprs)))

(define (expand-begin exprs)
  (let ((x (expand-begin* exprs '())))
    (if (pair? x)
        (if (pair? (cdr x))
            (cons 'begin x)
            (car x))
        (expand-constant 0)))) ;; unspecified value

(define (expand-begin* exprs rest)
  (if (pair? exprs)
      (let ((expr (car exprs)))
        (let ((r (expand-begin* (cdr exprs) rest)))
          (cond ((and (pair? expr) (eqv? (car expr) 'begin))
                 (expand-begin* (cdr expr) r))
                ((and (pair? expr) (eqv? (car expr) 'cond-expand))
                 (expand-cond-expand-clauses (cdr expr) r))
                (else
                 (cons (expand-expr expr) r)))))
      rest))

(define (cond-expand-eval expr)
  (cond ((and (pair? expr) (eqv? (car expr) 'not))
         (not (cond-expand-eval (cadr expr))))
        ((and (pair? expr) (eqv? (car expr) 'and))
         (not (memv #f (map cond-expand-eval (cdr expr)))))
        ((and (pair? expr) (eqv? (car expr) 'or))
         (not (not (memv #t (map cond-expand-eval (cdr expr))))))
        (else
         (eqv? expr 'ribbit))))

(define (expand-cond-expand-clauses clauses rest)
  (if (pair? clauses)
      (let ((clause (car clauses)))
        (if (or (eqv? 'else (car clause))
                (cond-expand-eval (car clause)))
            (expand-begin* (cdr clause) rest)
            (expand-cond-expand-clauses (cdr clauses) rest)))
      rest))

(define (expand-list exprs)
  (if (pair? exprs)
      (cons (expand-expr (car exprs))
            (expand-list (cdr exprs)))
      '()))

;;;----------------------------------------------------------------------------

;; Global variable liveness analysis.

(define (liveness-analysis expr exports)
  (let ((live (liveness-analysis-aux expr '())))
    (begin 
      (display "# symtbl in live ? ") (write live) (newline))
    (if (assv 'symtbl live)
        (liveness-analysis-aux expr exports)
        live)))

(define (liveness-analysis-aux expr exports)
  (let loop ((live-global-vars (exports->live (or exports '()))))
    (reset-defs live-global-vars)
    (let ((x (liveness expr live-global-vars #f)))
      (if (eqv? x live-global-vars)
          live-global-vars
          (loop x)))))

(define (exports->live exports)
  (if (pair? exports)
      (cons (list1 (car (car exports)))
            (exports->live (cdr exports)))
      '()))

(define (reset-defs lst)
  (let loop ((lst lst))
    (if (pair? lst)
        (begin
          (set-cdr! (car lst) '())
          (loop (cdr lst)))
        #f)))

(define (add-live var live-global-vars)
  (if (assv var live-global-vars)
      live-global-vars
      (cons (list1 var) live-global-vars)))

(define (constant-global-var? g)
  (and (pair? (cdr g))
       (null? (cddr g))
       (pair? (cadr g))
       (eqv? 'quote (car (cadr g)))))

(define (liveness expr live-global-vars export-all?)

  (define (add var)
    (set! live-global-vars (add-live var live-global-vars)))

  (define (add-val val)
    (cond ((symbol? val)
           (add val))
          ((pair? val)
           (add-val (car val))
           (add-val (cdr val)))
          ((vector? val)
           (for-each add-val (vector->list val)))))

  (define (liveness expr non-global-vars top?)

    (cond ((symbol? expr)
           (and (not (memv expr non-global-vars)) ;; global var?
                (add expr))) ;; mark the global variable as "live"

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eqv? first 'quote)
                    (let ((val (cadr expr)))
                      (add-val val)))

                   ((eqv? first 'set!)
                    (let* ((var (cadr expr))
                           (val (caddr expr)))
                      (if (memv var non-global-vars) ;; non-global var?
                          (liveness val non-global-vars #f)
                          (begin
                            (if export-all? (add var))
                            (let ((g (assv var live-global-vars))) ;; live?
                              (and g
                                   (begin
                                     (set-cdr! g (cons val (cdr g)))
                                     (liveness val non-global-vars #f))))))))

                   ((eqv? first 'if)
                    (liveness (cadr expr) non-global-vars #f)
                    (liveness (caddr expr) non-global-vars #f)
                    (liveness (cadddr expr) non-global-vars #f))

                   ((eqv? first 'let)
                    (let ((bindings (cadr expr)))
                      (liveness-list (map cadr bindings) non-global-vars #f)
                      (liveness (caddr expr) (union (map car bindings) non-global-vars) #f)))

                   ((eqv? first 'begin)
                    (liveness-list (cdr expr) non-global-vars top?))

                   ((eqv? first 'lambda)
                    (let ((params (cadr expr)))
                      (liveness (caddr expr) (extend params non-global-vars) #f)))

                   ((assv first prims)
                    (liveness-list (cdr expr) non-global-vars #f))

                   (else
                    (liveness-list expr non-global-vars #f)))))

          (else
           #f)))

  (define (liveness-list exprs non-global-vars top?)
    (and (pair? exprs)
         (begin
           (liveness (car exprs) non-global-vars top?)
           (liveness-list (cdr exprs) non-global-vars top?))))

  (liveness expr '() #t)

  live-global-vars)

;;;----------------------------------------------------------------------------

;; Free variable and mutable variable analyses.

(define (fv expr)

  (cond ((symbol? expr)
         (cons expr '()))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  '())

                 ((eqv? first 'set!)
                  (let* ((var (cadr expr))
                         (val (caddr expr)))
                    (union (fv val) (list1 var))))

                 ((or (eqv? first 'if) (eqv? first 'begin))
                  (fv-list (cdr expr)))

                 ((eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (body (cddr expr)))
                    (difference (fv-list body) params)))

                 ((eqv? first 'let)
                  (let* ((bindings (cadr expr))
                         (body (cddr expr))
                         (bound-vars (map car bindings)))
                    (union
                     (fv-list (map cadr bindings))
                     (difference (fv-list body) bound-vars))))

                 ((assv first prims)
                  (fv-list (cdr expr)))

                 (else
                  (fv-list expr)))))

        (else
         '()))) ;; self-evaluating

(define (fv-list exprs)
  (if (null? exprs)
      '()
      (union (fv (car exprs))
             (fv-list (cdr exprs)))))

(define (mv expr)

  (cond ((symbol? expr)
         '())

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  '())

                 ((eqv? first 'set!)
                  (let* ((var (cadr expr))
                         (val (caddr expr)))
                    (union (mv val) (cons var '()))))

                 ((or (eqv? first 'if) (eqv? first 'begin))
                  (mv-list (cdr expr)))

                 ((eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (body (cddr expr)))
                    (difference (mv-list body) params)))

                 ((eqv? first 'let)
                  (let* ((bindings (cadr expr))
                         (body (cddr expr))
                         (bound-vars (map car bindings)))
                    (union
                     (mv-list (map cadr bindings))
                     (difference (mv-list body) bound-vars))))

                 ((assv first prims)
                  (mv-list (cdr expr)))

                 (else
                  (mv-list expr)))))

        (else
         '()))) ;; self-evaluating

(define (mv-list exprs)
  (if (null? exprs)
      '()
      (union (mv (car exprs))
             (mv-list (cdr exprs)))))

;;;----------------------------------------------------------------------------

;; Operation on sets.

(define (filter predicate lst)
  (let loop ((lst lst) (acc '()))
    (if (pair? lst)
      (loop (cdr lst) (if (predicate (car lst)) 
                        (cons (car lst) acc) 
                        acc))
      (reverse acc))))

(define (list1 x) (cons x '()))
(define (list2 x y) (cons x (cons y '())))
(define (list3 x y z) (cons x (cons y (cons z '()))))
(define (list4 x y z a) (cons x (cons y (cons z (cons a '())))))
(define (list5 x y z a b) (cons x (cons y (cons z (cons a (cons b '()))))))
(define (list6 x y z a b c) (cons x (cons y (cons z (cons a (cons b (cons c '())))))))

(define (union lst1 lst2)
  (cond ((null? lst1)
         lst2)
        ((memv (car lst1) lst2)
         (union (cdr lst1) lst2))
        (else
         (cons (car lst1)
               (union (cdr lst1) lst2)))))

(define (intersection lst1 lst2)
  (cond ((null? lst1)
         '())
        ((memv (car lst1) lst2)
         (cons (car lst1)
               (intersection (cdr lst1) lst2)))
        (else
         (intersection (cdr lst1) lst2))))

(define (difference lst1 lst2)
  (cond ((null? lst1)
         '())
        ((memv (car lst1) lst2)
         (difference (cdr lst1) lst2))
        (else
         (cons (car lst1)
               (difference (cdr lst1) lst2)))))

(define (equal-sets? lst1 lst2)
  (and (null? (difference lst1 lst2))
       (null? (difference lst2 lst1))))

;;;----------------------------------------------------------------------------

;; Global variable elimination.

(define (gve expr non-global-vars live-global-vars)

  (cond ((symbol? expr)
         (let ((var expr))
           (if (memv var non-global-vars)
               expr
               (let ((g (assv var live-global-vars)))
                 (if g ;; live?
                     (if (constant-global-var? g) ;; propagate constant?
                         (cadr g)
                         (cons '$field0
                               (cons (expand-constant var)
                                     '())))
                     (expand-constant 0))))))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  expr)

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (if (memv var non-global-vars)
                        (let ((val (gve (caddr expr)
                                        non-global-vars
                                        live-global-vars)))
                          (list3 'set! var val))
                        (let ((g (assv var live-global-vars)))
                          (if (or (not g)
                                  (constant-global-var? g)) ;; propagated?
                              (expand-constant 0) ;; replace set! by noop
                              (let ((val (gve (caddr expr)
                                              non-global-vars
                                              live-global-vars)))
                                (cons '$field0-set!
                                      (cons (expand-constant var)
                                            (cons val '())))))))))

                 ((eqv? first 'if)
                  (cons 'if (gve-list (cdr expr)
                                      non-global-vars
                                      live-global-vars)))

                 ((eqv? first 'begin)
                  (cons 'begin (simplify-begin (gve-list (cdr expr)
                                                         non-global-vars
                                                         live-global-vars))))

                 ((eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (body (cddr expr))
                         (non-global-vars*
                          (union non-global-vars params)))
                    (cons 'lambda
                          (cons params
                                (simplify-begin (gve-list body
                                                          non-global-vars*
                                                          live-global-vars))))))

                 ((eqv? first 'let)
                  (let* ((bindings (cadr expr))
                         (body (cddr expr))
                         (vars (map car bindings))
                         (non-global-vars*
                          (union non-global-vars vars))
                         (body*
                          (simplify-begin (gve-list body
                                                    non-global-vars*
                                                    live-global-vars))))
                    (cons 'let
                          (cons (map (lambda (b)
                                       (cons (car b)
                                             (cons (gve (cadr b)
                                                        non-global-vars
                                                        live-global-vars)
                                                   '())))
                                     bindings)
                                body*))))

                 ((assv first prims)
                  (cons first (gve-list (cdr expr)
                                        non-global-vars
                                        live-global-vars)))

                 (else
                  (gve-list expr non-global-vars live-global-vars)))))

        (else
         (expand-constant expr)))) ;; self-evaluating

(define (gve-list exprs non-global-vars live-global-vars)
  (if (pair? exprs)
      (cons (gve (car exprs) non-global-vars live-global-vars)
            (gve-list (cdr exprs) non-global-vars live-global-vars))
      '()))

(define (simplify-begin exprs)

  (define (simplify rev-exprs tail)
    (if (pair? rev-exprs)
        (let ((expr (car rev-exprs)))

          (define (remove) (simplify (cdr rev-exprs) tail))
          (define (keep) (simplify (cdr rev-exprs) (cons expr tail)))

          (if (pair? expr)
              (let ((first (car expr)))
                (cond ((eqv? first 'quote)
                       (if (pair? tail) (remove) (keep)))
                      ((eqv? first 'begin)
                       (simplify (cdr rev-exprs)
                                 (simplify (reverse (cdr expr))
                                           tail)))
                      (else
                       (keep))))
              (if (pair? tail) (remove) (keep))))
        tail))

  (simplify (reverse exprs) '()))

;;;----------------------------------------------------------------------------

;; Mutable variable elimination.

(define (mve expr mutable-vars)

  ;; DONE?...
  ;; Cette fonction fait l'élimination des variables mutables.  Le
  ;; paramètre mutable-vars est l'ensemble des variables mutables qui
  ;; se fait calculer par les fonctions mv et mv-list lorsque des
  ;; formes lambda et let sont rencontrées dans le parcours récursif
  ;; de expr.

  (begin (display "# mve: ") (write expr) (newline))
  (cond
    ((symbol? expr)
     (let ((var expr))
       (if (memv var mutable-vars)
           (list2 '$field0 var)
           var)))
    ((pair? expr)
     (let ((first (car expr)))
       (cond
         ((eqv? first 'quote)
          expr)
         ((eqv? first 'set!)
          (mve-set! expr mutable-vars))
         ((eqv? first 'if)
          (mve-if expr mutable-vars))
         ((eqv? first 'begin)
          (mve-begin expr mutable-vars))
         ((eqv? first 'lambda)
          (mve-lambda expr mutable-vars))
         ((eqv? first 'let)
          (mve-let expr mutable-vars))
         ((assv first prims)
          (cons first (mve-list (cdr expr) mutable-vars)))
         (else
          (mve-list expr mutable-vars)))))
    (else expr)))  ;; self-evaluating

;;----------------------------------------------------------------------------
;; Helpers for mutable variable elimination.

(define (mve-set! expr mutable-vars)
  (let* ((var (cadr expr))
         (val (mve (caddr expr) mutable-vars)))
    (begin (display "# mve-set!: ") (write expr) (newline))
    (if (memv var mutable-vars)
        (list3 '$field0-set! var val)
        ;;(list3 '$field0-set! var val)
        (begin 
          (list3 '$field0-set! var val)))))
    ;; (list3 '$field0-set! var val)))

(define (mve-if expr mutable-vars)
  (cons 'if (mve-list (cdr expr) mutable-vars)))

(define (mve-begin expr mutable-vars)
  (cons 'begin (mve-list (cdr expr) mutable-vars)))

(define (mve-lambda expr mutable-vars)
  (let* ((params (cadr expr))                        ; '(x)
         (body (cddr expr))                              ; '((let ((a ($+ a x))) a)) or whatever
         ;;(fvs (difference  params))
         (mutables (intersection params (mv body)))             ; local vars + free vars
         (_ (begin (display "# mve-lambda(mutables): ") (write mutables) (newline)))
         ;; get the mutable vars
         (mutated (filter (lambda (v) (mutates? v body)) mutables))
         (_ (begin (display "# mve-lambda(mutated): ") (write mutated) (newline)))
         ;; rewrite body with new mutated vars
         (all-mutable (union mutable-vars mutated))
         (body* (mve-list body all-mutable)))
    (begin (display "# mve-lambda: ") (write mutated) (newline))
    ;; nothing was mutated, lambda
    (if (null? mutated)
        ;; no mutations
        (cons 'lambda (cons params body*))
        ;; wrap each mutated var in a rib at entry
        ; `(lambda ,params
        ;     (let ,(map (lambda (v) `(,v ($rib ,v 0 0))) mutated)
        ;       (begin
        ;         ,@body*)))

        (list3
          'lambda 
          params
          (list3
            'let 
            (map (lambda (v) (list2 v (list4 '$rib v 0 0)))
                 mutated)
            (cons 'begin body*))))))

(define (mutates? val expr)
  (cond
    ((not (pair? expr))
     #f)

    ((eqv? (car expr) 'set!)
     (or (eqv? (cadr expr) val)
         (mutates? val (cadr expr))))

    ((eqv? (car expr) 'let)
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (or (assv val bindings)
           (mutates? val body))))

    ((eqv? (car expr) 'lambda)
     (let ((params (cadr expr))
           (body (cddr expr)))
       (or (memv val params)
           (mutates? val body))))

    ((eqv? (car expr) 'begin)
     (mutates? val (cdr expr)))

    (else
      #f)))

(define (mve-let expr mutable-vars)
  (let* ((bindings   (cadr expr))               ;
         (body       (cddr expr))               ; the list of body expressions
         (vars       (map car bindings))        ;

         ;; Find set!
         (mutated
           (filter (lambda (v)
                     (let loop ((forms body))
                       (cond ((null? forms)
                              #f)
                             ((and (pair? (car forms))
                                   (eqv? (caar forms) 'set!)
                                   (eqv? (cadar forms) v))
                              #t)
                             (else
                              (loop (cdr forms))))))
                   vars))

         ;; Wrap mutated vars in $rib
         (bindings*
           (map (lambda (b)
                  (let ((v (car b))
                        (e (cadr b)))
                    (if (memv v mutated)
                        ;; wrap the og in closure
                        (list2 v (list4 '$rib (mve e mutable-vars) 0 0))
                        ;; return the og
                        (list2 v (mve e mutable-vars)))))
                bindings))

         ;; Body now
         (body*
           (mve-list body (union mutable-vars mutated))))
    ;; Reconstruct the let
    (begin (display "# mve-let: ") (write mutated) (newline))
    (list3 'let bindings* (cons 'begin body*))))


(define (mve-prim expr mutable-vars)
  (cons (car expr) (mve-list (cdr expr) mutable-vars)))

(define (mve-list exprs mutable-vars)
  (if (pair? exprs)
      (cons (mve (car exprs) mutable-vars)
            (mve-list (cdr exprs) mutable-vars))
      '()))


;;;----------------------------------------------------------------------------

;; Closure conversion.

(define (cc expr free-vars)

  ;; DONE?...
  ;; Cette fonction fait l'élimination des variables libres (conversion
  ;; des fermetures).
  ;; Le paramètre free-vars est l'ensemble des variables libres qui se
  ;; fait calculer par les fonctions fv et fv-list lorsqu'une forme
  ;; lambda est rencontrée dans le parcours récursif de expr.  La
  ;; position d'une variable libre dans cette liste indique la
  ;; position de la variable libre dans la fermeture plate.
  ;; Lorsqu'une variable libre devient inaccessible à cause d'un let à
  ;; une variable de même nom, par exemple
  ;;
  ;;       (lambda (a b) (lambda (x) (let ((a (+ a b))) ...
  ;;
  ;; le nom de variable est remplacé par #f (on ne peut pas retirer
  ;; le nom de la liste car ça changerait la position des autres variables
  ;; libres).

  (cond
    ((symbol? expr)
     (cc-symbol expr free-vars))
    ((pair? expr)
     (let ((first (car expr)))
       (cond
         ((eqv? first 'quote)
          expr)
        ((eqv? first 'set!)
          (cc-set! expr free-vars))
         ((eqv? first 'if)
          (cc-if expr free-vars))
        ((eqv? first 'begin)
          (cc-begin expr free-vars))
         ((eqv? first 'lambda)
          (cc-lambda expr free-vars))
         ((eqv? first 'let)
          (cc-let expr free-vars))
         (else
          (cc-call expr free-vars)))))
    (else expr)))  ;; self-evaluating

;; ----------------------------------------------------------------------------
;; Helpers for closure conversion.

(define (debug txt val)
  (display "# ")
  (display txt)
  (display ": ")
  (write val)
  (newline))

(define (cc-lambda expr free-vars)
  ;; expr : (lambda (params) body)
  ;; free-vars : list of current free vars

  ;; Get the params and body of the lambda
  (let* ((params (cadr expr))
         (body (cddr expr))
         ;; Compute all free vars used in the body
         ;; (lambda-free (fv body))
         ;; If the free vars are in the params, remove them they are NOT free
        (free-vars* (fv expr))    ;; list of free vars to be cc'd
        (_ (debug "free-vars*" free-vars*))
        (params* (cons '$self params))                  ;; add $self to the params
        (body* (cc-list body free-vars*))               ;; call cc on the body
        (env   (if (null? free-vars*)                   ;; no free vars
                    0
                    (build-cc-env
                     (map (lambda (v) (cc v free-vars)) ;; for each free var call cc on it
                          free-vars*))))
                      ;; test (should also work)
        (lam (cons 'lambda (cons params* body*))))
      ;; Build the rib
      (list4 '$rib lam env 1)))

(define (cc-begin expr free-vars)
  (cons 'begin (cc-list (cdr expr) free-vars)))

(define (cc-if expr free-vars)
  ;; (if cc-test cc-then cc-else) (cons hell cont...)
  (list4 'if
         (cc (cadr expr) free-vars)                ;; test
         (cc (caddr expr) free-vars)         ;; then
         (cc (cadddr expr) free-vars)))  ;; else

(define (cc-set! expr free-vars)
  ;; (set! var cc-val) (cons hell cont...)
  (let ((var (cadr expr))
        (val (caddr expr)))
    (list3 '$field0-set! var (cc val free-vars))))

(define (cc-let expr free-vars)
  ;; expr : (let ((var1 val1) (var2 val2) ...) body) => (let (bindings) body)
  ;; free-vars : list of current free vars

  ;; Get the bindings and body of the let
  (let* ((bindings    (cadr expr))
         (body        (caddr expr))
         (bindings*
            ;; For each pair in the bindings, get the var name and call cc on the val
            (map (lambda (binding)
                   (list2 (car binding) (cc (cadr binding) free-vars)))
              bindings))
          ;; Update the free vars list (remove the bound var names)
         (free-vars* (difference free-vars (map car bindings))))
    ;; Make the let => (let (bindings*) body)
    (list3 'let bindings* (cc body free-vars*))))

(define (cc-symbol expr free-vars)
  (if (memv expr free-vars)
      ;; If the symbol is in the free vars, get its index and return the rib
      (let ((index (list-index expr free-vars)))
        (if index
            (get-free-var index (length free-vars))
            expr))
      ;; else just return the symbol
      expr))

(define (cc-call expr free-vars)
  (begin (display "# cc-call: ") (write expr) (newline))
  (let* ((fn-code (cc (car expr) free-vars))
         (arg-codes (cc-list (cdr expr) free-vars)))
    (cond
      ((symbol? fn-code)
       (cons fn-code arg-codes))

      ((and (pair? fn-code)
            (eqv? (car fn-code) '$rib))
      ;  `(,fn-code ,@arg-codes)
        (cons fn-code arg-codes))

      (else
       (cons fn-code arg-codes)))))

(define (cc-list exprs free-vars)
  (if (pair? exprs)
      (cons (cc (car exprs) free-vars)
            (cc-list (cdr exprs) free-vars))
      '()))

(define (list-index element lst)
  (let loop ((lst lst) (index 0))
    (cond ((null? lst) #f)
          ((and (not (eqv? (car lst) #f))
                (eqv? element (car lst))) index)
          (else (loop (cdr lst) (+ index 1))))))

;; Build the env rib
(define (build-env vars)
  (cond ((null? vars) 0) ;; if var list is empty, no env
        ((null? (cdr vars)) (car vars)) ;; if only one var, return it
        ((null? (cddr vars))
                (list4 '$rib (car vars) (cadr vars) 0)) ;; if two vars+, build rib

        ;; build-env on the rest of the vars
        (else
          (list4 '$rib (car vars) (cadr vars) (build-env (cddr vars))))))


;; Build cc env, if 0 or 1 fv return, else build the env
(define (build-cc-env free-vals)
  ;; free-vals : list of current free var values (0, var, ribs)
  (if (or (null? free-vals) (null? (cdr free-vals)))            ;; if no free vars or only one
      (if (null? free-vals) 0 (car free-vals))                  ;; return 0 or the var
      (build-env free-vals)))                                   ;; else build the env (call)


(define (get-free-var i n)
  (let ((expr '($field1 $self)))
    (if (< n 2)
        expr
        (let loop ((i i) (n n) (expr expr))
          (if (< i 2)
              (cons (if (= i 0) '$field0 '$field1)
                    (cons expr '()))
              (let ((last (cons '$field2 (cons expr '()))))
                (if (and (= i 2) (= n 3))
                    last
                    (loop (- i 2) (- n 2) last))))))))

;;;----------------------------------------------------------------------------

;; Compiler front-end.

(define (extract-exports program)
  ;; By default all symbols are exported when the program contains
  ;; no (export ...) form.
  (let loop ((lst program) (rev-exprs '()) (exports #f))
    (if (pair? lst)
        (let ((first (car lst)))
          (if (and (pair? first) (eqv? (car first) 'export))
              (loop (cdr lst)
                    rev-exprs
                    (union (cdr first) (or exports '())))
              (loop (cdr lst)
                    (cons first rev-exprs)
                    exports)))
        (cons (reverse rev-exprs) exports))))

(define (exports->alist exports)
  (if (pair? exports)
      (map (lambda (x)
             (if (symbol? x)
                 (cons x x)
                 (cons (car x) (cadr x))))
           exports)
      exports))

; (define file-to-read (open-input-file "test2.scm"))

(define (read-all)
; (let ((x (read file-to-read)))

  (let ((x (read)))
    (if (eof-object? x)
        '()
        (cons x (read-all)))))

(define (front-end)
  (let* ((program
          (union (build-primitives) (read-all)))
         (exprs-and-exports
          (extract-exports program))
         (exprs
          (car exprs-and-exports))
         (_
          (if debug?
              (begin
                (display "# exprs = ") (write exprs) (newline))))
         (exports
          (cdr exprs-and-exports))
         (exports*
          (and exports (exports->alist exports)))
         (_
          (if debug?
              (begin
                (display "# exports = ") (write exports*) (newline))))
         (expansion1
          (expand-begin (if (pair? exprs) exprs (cons #f '()))))
         (_
          (if debug?
              (begin
                (display "# expansion1 = ") (write expansion1) (newline))))
         (live-global-vars
          (liveness-analysis expansion1 exports*))
         (_
          (if debug?
              (for-each (lambda (x)
                          (display "# live-global ")
                          (write (car x))
                          (display " ")
                          (write (cdr x))
                          (newline))
                        live-global-vars)))
         (expansion2
          (gve expansion1 '() live-global-vars))
         (_
          (if debug?
              (begin
                (display "# expansion2 = ") (write expansion2) (newline))))
         (expansion3
          (mve expansion2 '()))
         (_
          (if debug?
              (begin
                (display "# expansion3 = ") (write expansion3) (newline))))
         (expansion4
          (cc expansion3 '()))
         (_
          (if debug?
              (begin
                (display "# expansion4 = ") (write expansion4) (newline)))))
    ; (step)
    (cons
     expansion4
     (cons
      (or exports*
          (map (lambda (v)
                 (let ((var (car v)))
                   (cons var var)))
               live-global-vars))
      live-global-vars))))

;;;----------------------------------------------------------------------------

;; Compiler entry point.

;; The compiler reads the source code on stdin and
;; outputs the x86 code on stdout.  The program source
;; code must be prefixed by the runtime library's source code.
;;
;; A Scheme file can be combined with the library and compiled to
;; x86 code with this command:
;;
;;   $ echo '(display "hello!\n")' > hello.scm
;;   $ cat lib/max.scm hello.scm | gsi rsc-x86.bundle.scm > code.s
;;
;; Alternatively, the rsc shell script can be used to automate
;; the creation of a complete executable target program:
;;
;;   $ ./rsc -t x86 -l max -c "gsi rsc-x86.bundle.scm" -o hello.scm.s hello.scm
;;   $ gcc hello.scm.s
;;   $ ./a.out
;;   hello!

(back-end (front-end))

;;;----------------------------------------------------------------------------
