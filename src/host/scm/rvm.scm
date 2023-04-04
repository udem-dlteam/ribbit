; @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
(define input ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y") ;; RVM code that prints HELLO!
; )@@

(cond-expand
  (gambit
   ;;(define-cond-expand-feature debug)
   (declare (standard-bindings) (block) (not safe)))
  (else))

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (_rib? x) (vector? x))
(define (_rib x y z) (vector x y z))
(define (_field0 x) (vector-ref x 0))
(define (_field1 x) (vector-ref x 1))
(define (_field2 x) (vector-ref x 2))
(define (_field0-set! x y) (vector-set! x 0 y))
(define (_field1-set! x y) (vector-set! x 1 y))
(define (_field2-set! x y) (vector-set! x 2 y))

(define (instance? type)
  (lambda (x) (and (_rib? x) (eqv? (_field2 x) type))))

(define _pair? (instance? pair-type))
(define (_cons car cdr) (_rib car cdr pair-type))
(define (_car pair) (_field0 pair))
(define (_cdr pair) (_field1 pair))
(define (_set-car! pair x) (_field0-set! pair x))

(define (_list->string lst) (_rib lst (_length lst) string-type))

(define (_string->uninterned-symbol str) (_rib _false str symbol-type))

(define _false (_rib 0 0 singleton-type))
(define _true  (_rib 0 0 singleton-type))
(define _nil   (_rib 0 0 singleton-type))

(define (_list-tail lst i)
  (if (< 0 i)
      (_list-tail (_cdr lst) (- i 1))
      lst))

(define (_length lst)
  (if (_pair? lst)
      (+ 1 (_length (_cdr lst)))
      0))

(define pos 0)

(define (get-byte)
  (let ((x (char->integer (string-ref input pos))))
    (set! pos (+ pos 1))
    x))

(define (decode)

  (define eb/2 46) ;; half of encoding base (92)

  (define (get-code)
    (let ((x (- (get-byte) 35)))
      (if (< x 0) 57 x)))

  (define (get-int n)
    (let ((x (get-code))
          (y (* n eb/2)))
      (if (< x eb/2)
          (+ y x)
          (get-int (+ y (- x eb/2))))))

  (define (build-symtbl)

    (define (add-symbol chars symtbl)
      (_cons (_string->uninterned-symbol (_list->string chars))
             symtbl))

    (let loop1 ((n (get-int 0)) (symtbl _nil))
      (if (< 0 n)
          (loop1 (- n 1) (add-symbol _nil symtbl))
          (let loop2 ((symtbl symtbl))
            (let loop3 ((chars _nil))
              (let ((x (get-byte)))
                (if (= x 44) ;; #\, separates symbols
                    (loop2 (add-symbol chars symtbl))
                    (if (= x 59) ;; #\; terminates symbol list
                        (add-symbol chars symtbl)
                        (loop3 (_cons x chars))))))))))

  (let ((symtbl (build-symtbl)))

    (define (decode-loop stack)

      (define (sym n)
        (_car (_list-tail symtbl n)))

      (define (add-instruction op opnd stack)
;;        (pp (list (vector-ref '#(jump/call set get const if) op) opnd))
        (_set-car! stack (_rib op opnd (_car stack)))
        (decode-loop stack))

      (let ((x (get-code)))
        (let loop ((op 0) (n x))
          (let ((d (vector-ref '#(20 30 0 10 11 4) op)))
            (if (< (+ 2 d) n)
                (loop (+ op 1) (- n (+ d 3)))
                (if (< 90 x)
                    (add-instruction 4 ;; if
                                     (_car stack)
                                     (_cdr stack))
                    (let ((stack (if (= op 0) (_cons 0 stack) stack))
                          (opnd (if (< n d)
                                    (if (< op 3)
                                        (sym n)
                                        n)
                                    (if (= n d)
                                        (get-int 0)
                                        (sym (get-int (- (- n d) 1)))))))
                      (if (< 4 op)
                          (let ((proc (_rib
                                       (_rib opnd 0 (_car stack))
                                       _nil
                                       procedure-type))
                                (stack (_cdr stack)))
                            (if (_rib? stack)
                                (add-instruction 3 ;; const-proc
                                                 proc
                                                 stack)
                                proc))
                          (add-instruction (if (< 0 op) (- op 1) 0)
                                           opnd
                                           stack)))))))))

    (let ((main-proc (decode-loop 0)))

      ;; set predefined globals (always 4 first in the symbol table)

      (define (set-global val)
        (_field0-set! (_car symtbl) val)
        (set! symtbl (_cdr symtbl)))

      (set-global (_rib 0 symtbl procedure-type)) ;; rib  = primitive 0
      (set-global _false) ;; false  = #f
      (set-global _true)  ;; true   = #t
      (set-global _nil)   ;; nil    = ()

      main-proc)))

(cond-expand
  (debug

   (define tracing #f)
   (define step-count 0)
   (define start-tracing 0)
   (define next-stamp 0)

   (define (trace-instruction name opnd)
     (display name)
     (if opnd
         (begin
           (display " ")
           (show opnd)))
     (newline))

   (define (show obj)
     (if (not (_rib? obj))
         (display obj)
         (let ((type (_field2 obj)))
           (if (= type 4)
               (begin (display "#") (show (_field0 obj)))
               (case type
                 ((0)
                  (display "(")
                  (show (_field0 obj))
                  (let ((obj
                         (let loop ((n 1) (obj (_field1 obj)))
                           (if (and (_rib? obj) (= (_field2 obj) 0))
                               (if (> n 4)
                                   (begin
                                     (display " ...")
                                     _nil)
                                   (begin
                                     (display " ")
                                     (show (_field0 obj))
                                     (loop (+ n 1) (_field1 obj))))
                               obj))))
                    (if (not (eqv? obj _nil))
                        (begin
                          (display " . ")
                          (show obj)))
                    (display ")")))
                 ((1)
                  (if (_rib? (_field0 obj))
                      (begin
                        (display "#<procedure nparams=")
                        (display (_field0 (_field0 obj)))
                        (display ">"))
                      (begin
                        (display "#<primitive ")
                        (display (_field0 obj))
                        (display ">"))))
                 ((2)
                  (let ((obj (_field1 obj)))
                    (if (and (_rib? obj)
                             (= (_field2 obj) 3)
                             (> (_field1 obj) 0))
                        (let loop ((obj (_field0 obj)))
                          (if (and (_rib? obj) (= (_field2 obj) 0))
                              (begin
                                (display (integer->char (_field0 obj)))
                                (loop (_field1 obj)))))
                        (begin
                          (display "#<symbol ")
                          (show obj)
                          (display ">")))))
                 ((3)
                  (display "\"")
                  (let loop ((obj (_field0 obj)))
                    (if (and (_rib? obj) (= (_field2 obj) 0))
                        (let ((c (_field0 obj)))
                          (case c
                            ((10) (display "\\n"))
                            ((13) (display "\\r"))
                            ((9)  (display "\\t"))
                            ((92) (display "\\\\"))
                            ((34) (display "\\\""))
                            (else (display (integer->char c))))
                          (loop (_field1 obj)))
                        (display "\""))))
                 ((5)
                  (cond ((eqv? obj _false)
                         (display "#f"))
                        ((eqv? obj _true)
                         (display "#t"))
                        ((eqv? obj _nil)
                         (display "()"))
                        (else
                         (display "[")
                         (show (_field0 obj))
                         (display ",")
                         (show (_field1 obj))
                         (display ",")
                         (show (_field2 obj))
                         (display "]"))))
                 (else
                  (display "[")
                  (show (_field0 obj))
                  (display ",")
                  (show (_field1 obj))
                  (display ",")
                  (show (_field2 obj))
                  (display "]")))))))

   (define (start-step stack)
     (set! step-count (+ step-count 1))
     (if (>= step-count start-tracing) (set! tracing #t))
     (if (not tracing)
         (if (>= step-count next-stamp)
             (begin
               (set! next-stamp (exact (floor (+ (* next-stamp 1.01) 1))))
               (display "@")
               (display step-count)
               (newline)))
         (begin
           (display "@")
           (display step-count)
           (display " STACK = (")
           (let loop ((s stack) (sep ""))
             (if (eqv? (_field2 s) 0)
                 (begin
                   (display sep)
                   (show (_field0 s))
                   (loop (_field1 s) " "))
                 (begin
                   (display ")")
                   (newline))))))))

  (else))

(define (get-cont stack)
  (let loop ((stack stack))
    (if (_rib? (_field2 stack)) stack (loop (_cdr stack)))))

(define (get-var stack opnd)
  (_field0 (if (_rib? opnd) opnd (_list-tail stack opnd))))

(define (set-var stack opnd val)
  (_field0-set! (if (_rib? opnd) opnd (_list-tail stack opnd)) val))

(define (run pc stack)
  (cond-expand (debug (start-step stack)) (else #f))
  (let ((instr (_field0 pc))
        (opnd (_field1 pc))
        (next (_field2 pc)))
    (case instr

      ((0) ;; jump/call
       (cond-expand
         (debug
          (if tracing
              (trace-instruction (if (eqv? 0 next) "jump" "call") opnd)))
         (else
          #f))
       (let* ((proc (get-var stack opnd))
              (code (_field0 proc))
              ; @@(feature arity-check (use rest-param)
              (ncall (_car stack))
              (stack (_cdr stack))
              ; )@@
              )
         (if (_rib? code)

             ;; calling a lambda
             (let ((new-cont (_rib 0 proc 0))
                   (nargs (arithmetic-shift (_field0 code) -1))
                   (vari  (bitwise-and (_field0 code) 1)))
               ;; @@(feature arity-check
               (if (or (and (eqv? vari 0)
                            (not (eqv? nargs ncall)))
                       (and (eqv? vari 1)
                            (> nargs ncall)))
                 (error "*** Arrity check failled"))
               ;; )@@
               ;; @@(feature rest-param (use arity-check)
               (if (eqv? vari 1)
                 (let rest-loop ((rest _nil)
                                 (i (- ncall nargs))
                                 (_stack stack))
                   (if (< 0 i)
                     (rest-loop
                       (_cons (_car _stack) rest)
                       (- i 1)
                       (_cdr _stack)
                       )
                     (begin 
                       (set! stack (_cons rest _stack))
                       (set! nargs (+ 1 nargs))))
                   )
                 )
               ;; )@@
               (let loop ((nargs nargs)
                          (new-stack new-cont)
                          (stack stack))
                 (if (< 0 nargs)
                     (loop (- nargs 1)
                           (_cons (_car stack) new-stack)
                           (_cdr stack))
                     (begin
                       (if (_rib? next) ;; non-tail call?
                           (begin
                             (_field0-set! new-cont stack)
                             (_field2-set! new-cont next))
                           (let ((k (get-cont stack)))
                             (_field0-set! new-cont (_field0 k))
                             (_field2-set! new-cont (_field2 k))))
                       (run (_field2 code)
                            new-stack)))))

             ;; calling a primitive
             (let ((stack ((vector-ref primitives code) stack)))
               (run (if (_rib? next) ;; non-tail call?
                        next
                        (let ((cont (get-cont stack)))
                          (_field1-set! stack (_field0 cont))
                          (_field2 cont)))
                    stack)))))

      ((1) ;; set
       (cond-expand
         (debug
          (if tracing
              (trace-instruction "set" opnd)))
         (else
          #f))
       (set-var stack opnd (_car stack))
       (run next
            (_cdr stack)))

      ((2) ;; get
       (cond-expand
         (debug
          (if tracing
              (trace-instruction "get" opnd)))
         (else
          #f))
       (run next
            (_cons (get-var stack opnd) stack)))

      ((3) ;; const
       (cond-expand
         (debug
          (if tracing
              (trace-instruction "const" opnd)))
         (else
          #f))
       (run next
            (_cons opnd stack)))

      ((4) ;; if
       (cond-expand
         (debug
          (if tracing
              (trace-instruction "if" #f)))
         (else
          #f))
       (run (if (eqv? (_car stack) _false) next opnd)
            (_cdr stack)))
      (else ;; halt
       (cond-expand
         (debug
          (if tracing
              (trace-instruction "halt" #f)))
         (else
          #f))
       #f))))

(define (prim0 f)
  (lambda (stack)
    (_cons (f) stack)))

(define (prim1 f)
  (lambda (stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
      (_cons (f x) stack))))

(define (prim2 f)
  (lambda (stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y) stack))))

(define (prim3 f)
  (lambda (stack)
    (let* ((z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y z) stack))))

(define (boolean x)
  (if x _true _false))

(define primitives

  (vector 
    ;; @@(primitives (gen body)
    (prim3 _rib)             ;; @@(primitive (rib a b c))@@
    (prim1 (lambda (x) x))   ;; @@(primitive (id x))@@
    _cdr                     ;; @@(primitive (arg1 a b))@@
    (prim2 (lambda (y x) x)) ;; @@(primitive (arg2 a b))@@

    ;; @@(primitive (close rib)
    (lambda (stack) ;; 4
      (let* ((x (_car stack)) (stack (_cdr stack)))
        (_cons (_rib (_field0 x) stack procedure-type) stack)))
    ;; )@@

    (prim1 (lambda (x) (boolean (_rib? x))))    ;; @@(primitive (rib? rib))@@
    (prim1 _field0)                             ;; @@(primitive (field0 rib))@@
    (prim1 _field1)                             ;; @@(primitive (field1 rib))@@
    (prim1 _field2)                             ;; @@(primitive (field2 rib))@@
    (prim2 (lambda (x y) (_field0-set! x y) y)) ;; @@(primitive (field0-set! rib v))@@
    (prim2 (lambda (x y) (_field1-set! x y) y)) ;; @@(primitive (field1-set! rib v))@@
    (prim2 (lambda (x y) (_field2-set! x y) y)) ;; @@(primitive (field2-set! rib v))@@
    (prim2 (lambda (x y) (boolean (eqv? x y)))) ;; @@(primitive (eqv? x y))@@
    (prim2 (lambda (x y) (boolean (< x y))))    ;; @@(primitive (< x y))@@
    (prim2 +)                                   ;; @@(primitive (+ a b))@@
    (prim2 -)                                   ;; @@(primitive (- a b))@@
    (prim2 *)                                   ;; @@(primitive (* a b))@@
    (prim2 quotient)                            ;; @@(primitive (quotient a b))@@

    ;; @@(primitive (getchar)
    (prim0 (lambda () ;; 18
             (if (< pos (string-length input))
               (get-byte)
               (let ((c (read-char)))
                 (if (char? c) (char->integer c) -1)))))
    ;; )@@

    ;; @@(primitive (putchar x)
    (prim1 (lambda (x) ;; 19
             (write-char (integer->char x))
             x))
    ;; )@@ 

    ;; @@(primitive (exit x)
    (prim1 (lambda (x) ;; 20
             (exit x)))
    ;; )@@
  ;; )@@
))

(let ((x (decode)))
  (run (_field2 (_field0 x)) ;; instruction stream of main procedure
       (_rib 0 0 (_rib 5 0 0)))) ;; primordial continuation = halt
