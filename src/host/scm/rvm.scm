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

(define (_rib? obj) (vector? obj))
(define (_rib field0 field1 field2) (vector field0 field1 field2))
(define (_field0 rib) (vector-ref rib 0))
(define (_field1 rib) (vector-ref rib 1))
(define (_field2 rib) (vector-ref rib 2))
(define (_field0-set! rib x) (vector-set! rib 0 x))
(define (_field1-set! rib x) (vector-set! rib 1 x))
(define (_field2-set! rib x) (vector-set! rib 2 x))

(define (instance? type)
  (lambda (obj) (and (_rib? obj) (eqv? (_field2 obj) type))))

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
;;        (pp (list (vector-ref #(jump/call set get const if) op) opnd))
        (_set-car! stack (_rib op opnd (_car stack)))
        (decode-loop stack))

      (let ((x (get-code)))
        (let loop ((op 0) (n x))
          (let ((d (vector-ref #(20 30 0 10 11 4) op)))
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
;;(pp (list 'pos= pos))
;;(pp (list 'symtbl= (convert symtbl)))

      (set-global (_rib 0 symtbl procedure-type)) ;; rib  = primitive 0
      (set-global _false) ;; false  = #f
      (set-global _true)  ;; true   = #t
      (set-global _nil)   ;; nil    = ()

      main-proc)))

(cond-expand
  (debug

   (define (trace-instruction name opnd stack)
     (display "--- ")
     (display name)
     (display " ")
     (if opnd
         (begin
           (write (convert opnd))
           (display " ")))
     (write (show-stack stack))
     (newline))

   (define (convert obj)
     (cond ((eqv? obj _false)
            #f)
           ((eqv? obj _true)
            #t)
           ((eqv? obj _nil)
            (list))
           ((not (_rib? obj))
            obj)
           ((eqv? (_field2 obj) pair-type)
            (cons (convert (_field0 obj)) (convert (_field1 obj))))
           ((eqv? (_field2 obj) procedure-type)
            '<procedure>)
           ((eqv? (_field2 obj) symbol-type)
            (string->symbol (convert (_field1 obj))))
           ((eqv? (_field2 obj) string-type)
            (list->string (map integer->char (convert (_field0 obj)))))
           ((eqv? (_field2 obj) vector-type)
            (list->vector (convert (_field0 obj))))
           (else
            '<unknown>)))
           
   (define (show-stack stack)
     (if (_pair? stack)
         (cons (convert (_car stack))
               (show-stack (_cdr stack)))
         (if (eqv? 0 stack)
             (list)
             (list 'stack: (show-stack (_field0 stack))
                   'pc: (convert (_field2 stack)))))))
  (else
   (define (trace-instruction name opnd stack)
     0)))

(define (run pc stack)

  (define (get-cont stack)
    (let loop ((stack stack))
      (if (_rib? (_field2 stack)) stack (loop (_cdr stack)))))

  (define (get-var opnd)
    (_field0 (if (_rib? opnd) opnd (_list-tail stack opnd))))

  (define (set-var opnd val)
    (_field0-set! (if (_rib? opnd) opnd (_list-tail stack opnd)) val))

  (let ((instr (_field0 pc))
        (opnd (_field1 pc))
        (next (_field2 pc)))
    (case instr

      ((0) ;; jump/call
       (trace-instruction (if (eqv? 0 next) "jump" "call") opnd stack)
       (let* ((proc (get-var opnd))
              (code (_field0 proc)))
         (if (_rib? code)

             ;; calling a lambda
             (let ((new-cont (_rib 0 proc 0)))
               (let loop ((nargs (_field0 code))
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
       (trace-instruction "set" opnd stack)
       (set-var opnd (_car stack))
       (run next
            (_cdr stack)))

      ((2) ;; get
       (trace-instruction "get" opnd stack)
       (run next
            (_cons (get-var opnd) stack)))

      ((3) ;; const
       (trace-instruction "const" opnd stack)
       (run next
            (_cons opnd stack)))

      ((4) ;; if
       (trace-instruction "if" #f stack)
       (run (if (eqv? (_car stack) _false) next opnd)
            (_cdr stack)))

      ((5) ;; halt
       (trace-instruction "halt" #f stack))

      (else
       (display "*** unexpected instruction ")
       (write instr)
       (newline)))))

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
  (vector (prim3 _rib)             ;; 0
          (prim1 (lambda (x) x))   ;; 1
          _cdr                     ;; 2
          (prim2 (lambda (x y) x)) ;; 3

          (lambda (stack) ;; 4
            (let* ((x (_car stack)) (stack (_cdr stack)))
              (_cons (_rib (_field0 x) stack procedure-type) stack)))

          (prim1 (lambda (x) (boolean (_rib? x)))) ;; 5
          (prim1 _field0) ;; 6
          (prim1 _field1) ;; 7
          (prim1 _field2) ;; 8
          (prim2 (lambda (x y) (_field0-set! x y) y)) ;; 9
          (prim2 (lambda (x y) (_field1-set! x y) y)) ;; 10
          (prim2 (lambda (x y) (_field2-set! x y) y)) ;; 11
          (prim2 (lambda (x y) (boolean (eqv? x y)))) ;; 12
          (prim2 (lambda (x y) (boolean (< x y)))) ;; 13
          (prim2 +) ;; 14
          (prim2 -) ;; 15
          (prim2 *) ;; 16
          (prim2 quotient) ;; 17

          (prim0 (lambda () ;; 18
                   (if (< pos (string-length input))
                       (get-byte)
                       (let ((c (read-char)))
                         (if (char? c) (char->integer c) -1)))))

          (prim1 (lambda (x) ;; 19
                   (write-char (integer->char x))
                   x))))

(define (start)
  (let ((main-proc (decode)))
    (run (_field2 (_field0 main-proc)) ;; instruction stream of main procedure
         (_rib 0 0 (_rib 5 0 0))))) ;; primordial continuation = halt

(start)
