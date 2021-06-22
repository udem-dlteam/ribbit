#!/usr/bin/env gsi

;;; Small-Scheme compiler.

(include "uvm-compat.scm")
(declare (not proper-tail-calls))
(include "lib1.scm")

;;;----------------------------------------------------------------------------

(define primitives
  (vector 'identity
          'arg1
          'arg2
          'close
          'cons
          'clump?
          'field0
          'field1
          'field2
          'field0-set!
          'field1-set!
          'field2-set!
          'eq?
          '<
          '+
          '-
          '*
          'quotient
          'getchar
          'putchar))

#;(begin
(global-var-set! 'identity    (make-procedure 0 '()))
(global-var-set! 'arg1        (make-procedure 1 '()))
(global-var-set! 'arg2        (make-procedure 2 '()))
(global-var-set! 'cons        (make-procedure 3 '()))
(global-var-set! 'clump?      (make-procedure 4 '()))
(global-var-set! 'field0      (make-procedure 5 '()))
(global-var-set! 'field1      (make-procedure 6 '()))
(global-var-set! 'field2      (make-procedure 7 '()))
(global-var-set! 'field0-set! (make-procedure 8 '()))
(global-var-set! 'field1-set! (make-procedure 9 '()))
(global-var-set! 'field2-set! (make-procedure 10 '()))
(global-var-set! 'eq?         (make-procedure 11 '()))
(global-var-set! '<           (make-procedure 12 '()))
(global-var-set! '+           (make-procedure 13 '()))
(global-var-set! '-           (make-procedure 14 '()))
(global-var-set! '*           (make-procedure 15 '()))
(global-var-set! 'quotient    (make-procedure 16 '()))
(global-var-set! 'getchar     (make-procedure 17 '()))
(global-var-set! 'putchar     (make-procedure 18 '()))
)

;;;----------------------------------------------------------------------------

;; The uVM interpreter.

(define (uvm->host x)
  (cond ((string? x)
         (let loop ((lst (string->list x)) (rev (list)))
           (if (pair? lst)
               (loop (cdr lst) (##cons (##integer->char (car lst)) rev))
               (##list->string (##reverse rev)))))
        ((symbol? x)
         (##string->symbol (uvm->host (symbol->string x))))
        ((pair? x)
         (##cons (uvm->host (car x)) (uvm->host (cdr x))))
        ((eq? x '#f)
         #f)
        ((eq? x '#t)
         #t)
        ((eq? x '())
         (list))
        (else
         x)))
        
(define jump-op  (##quote jump))
(define call-op  (##quote call))
(define set-op   (##quote set))
(define get-op   (##quote get))
(define const-op (##quote const))
(define if-op    (##quote if))

(define (oper pc) (field0 pc))
(define (opnd pc) (field1 pc))
(define (next pc) (field2 pc))

(define (push stack x) (clump x stack 0))
(define (pop stack) (field1 stack))
(define (top stack) (field0 stack))
(define (top-set! stack x) (field0-set! stack x))

(define (prim0 stack fn)
  (push stack (fn)))

(define (prim1 stack fn)
  (top-set! stack (fn (top stack)))
  stack)

(define (prim2 stack fn)
  (let* ((y (top stack)) (s (pop stack)))
    (top-set! s (fn (top s) y))
    s))

(define (prim3 stack fn)
  (let* ((z (top stack)) (s (pop stack)) (y (top s)) (s (pop s)))
    (top-set! s (fn (top s) y z))
    s))

(define (prim-cons stack) ;; reuses stack clump
  (let* ((y (top stack)) (s (pop stack)) (x (top s)))
    (field0-set! stack x)
    (field1-set! stack y)
    (field2-set! stack 0)
    (field0-set! s stack)
    s))

(define (prim-close stack)
  (top-set! stack (make-procedure (field0 (top stack)) (pop stack)))
  stack)

(define (primitive stack code)
  (let ((prim (vector-ref primitives code)))
    (pp `(prim: ,(uvm->host prim)))
    (cond
     ((eq? prim 'identity)    stack)
     ((eq? prim 'arg1)        (pop stack))
     ((eq? prim 'arg2)        (top-set! (pop stack) (top stack)) (pop stack))
     ((eq? prim 'close)       (prim-close stack))
     ((eq? prim 'cons)        (prim-cons stack))
     ((eq? prim 'clump?)      (prim1 stack clump?))
     ((eq? prim 'field0)      (prim1 stack field0))
     ((eq? prim 'field1)      (prim1 stack field1))
     ((eq? prim 'field2)      (prim1 stack field2))
     ((eq? prim 'field0-set!) (prim2 stack field0-set!))
     ((eq? prim 'field1-set!) (prim2 stack field1-set!))
     ((eq? prim 'field2-set!) (prim2 stack field2-set!))
     ((eq? prim 'eq?)         (prim2 stack eq?))
     ((eq? prim '<)           (prim2 stack <))
     ((eq? prim '+)           (prim2 stack +))
     ((eq? prim '-)           (prim2 stack -))
     ((eq? prim '*)           (prim2 stack *))
     ((eq? prim 'quotient)    (prim2 stack quotient))
     ((eq? prim 'getchar)     (prim0 stack getchar))
     ((eq? prim 'putchar)     (prim1 stack putchar))
     (else                    (error "unknown primitive" prim)))))

(define (run pc stack)
  (let ((op (oper pc)))
;;    (pp `(op: ,op stack: ,(uvm->host stack)))
    (cond ((eq? op const-op)
           (run (next pc)
                (push stack (opnd pc))))
          ((eq? op get-op)
           (run (next pc)
                (push stack (ref stack (opnd pc)))))
          ((eq? op set-op)
           (set stack (opnd pc) (top stack))
           (run (next pc)
                stack))
          ((eq? op if-op)
           (let ((s (pop stack)))
             (pp `(if test: ,(top stack)))
             (if (top stack)
                 (run (opnd pc) s)
                 (run (next pc) s))))
          ((eq? op call-op)
           (let ((proc (ref stack (opnd pc))))
             (pp `(call proc: ,proc))
             (let ((code (procedure-code proc)))
               (pp `(code: ,code))
               (if (clump? code)
                   ...
                   (run (next pc)
                        (primitive stack code))))))
          ((eq? op jump-op)
           (let ((proc (ref stack (opnd pc))))
             (pp `(jump proc: ,proc))
             (let ((code (procedure-code proc)))
               (pp `(code: ,code))
               (if (clump? code)
                   ...
                   (return (primitive stack code))))))
          (else
           (error "unknown operation" op)))))

(define (return stack)
  (pp `(return: ,(uvm->host (car stack)) stack: ,(uvm->host stack)))
  stack)

(define (ref stack var)
  (if (clump? var)
      (global-var-ref var)
      (list-ref stack var)))
  
(define (set stack var val)
  (if (clump? var)
      (global-var-set! var val)
      (list-set! stack var val)))
  
;;;----------------------------------------------------------------------------

;; The compiler from SScheme to uVM code.

(define (make-ctx cte live exports) (clump cte live exports))

(define (ctx-cte ctx) (field0 ctx))
(define (ctx-live ctx) (field1 ctx))
(define (ctx-exports ctx) (field2 ctx))

(define (ctx-cte-set ctx x) (clump x (ctx-live ctx) (ctx-exports ctx)))
(define (ctx-live-set ctx x) (clump (ctx-cte ctx) x (ctx-exports ctx)))
(define (ctx-exports-set ctx x) (clump (ctx-cte ctx) (ctx-live ctx) x))

(define (comp ctx expr cont)

  (cond ((symbol? expr)
         (let ((v (lookup expr (ctx-cte ctx) 0)))
           (let ((g (live? expr (ctx-live ctx))))
             (if (and g (constant? g)) ;; constant propagated?
                 (clump const-op (cadr (cadr g)) cont)
                 (clump get-op v cont)))))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eq? first 'quote)
                  (clump const-op (cadr expr) cont))

                 ((eq? first 'set!)
                  (let ((var (cadr expr)))
                    (let ((val (caddr expr)))
                      (let ((v (lookup var (ctx-cte ctx) 1)))
                        (if (eq? v var) ;; global?
                            (let ((g (live? var (ctx-live ctx))))
                              (if g
                                  (if (constant? g)
                                      (begin
                                        (pp `(*** constant propagation of ,(uvm->host var) = ,(uvm->host (cadr g)))
                                            (current-error-port))
                                        (gen-noop cont))
                                      (comp ctx val (gen-assign v cont)))
                                  (begin
                                    (pp `(*** removed dead assignment to ,(uvm->host var))
                                        (current-error-port))
                                    (gen-noop cont))))
                            (comp ctx val (gen-assign v cont)))))))

                 ((eq? first 'if)
                  (let ((cont-false (comp ctx (cadddr expr) cont)))
                    (let ((cont-true (comp ctx (caddr expr) cont)))
                      (let ((cont-test (clump if-op cont-true cont-false)))
                        (comp ctx (cadr expr) cont-test)))))

                 ((eq? first 'lambda)
                  (let ((params (cadr expr)))
                    (clump const-op
                           (make-procedure
                            (clump (length params)
                                   0
                                   (comp-begin (ctx-cte-set
                                                ctx
                                                (extend (reverse params)
                                                        (ctx-cte ctx)))
                                               (cddr expr)
                                               tail))
                            '())
                           (if (null? (ctx-cte ctx))
                               cont
                               (gen-call 'close cont)))))

                 ((eq? first 'let)
                  (let ((binding (car (cadr expr))))
                    (comp ctx
                          (cadr binding)
                          (comp-begin (ctx-cte-set
                                       ctx
                                       (cons (car binding) (ctx-cte ctx)))
                                      (cdr (cdr expr))
                                      (if (eq? cont tail)
                                          cont
                                          (clump call-op 'arg2 cont))))))

                 ((eq? first 'begin)
                  (comp-begin ctx (cdr expr) cont))

                 (else
                  (comp-list ctx
                             (cdr expr)
                             (lambda (ctx)
                               (gen-call (lookup (car expr) (ctx-cte ctx) 0)
                                         cont)))))))

        (else
         ;; self-evaluating
         (clump const-op expr cont))))

(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst result)
  (if (pair? lst)
      (reverse-aux (cdr lst) (cons (car lst) result))
      result))

(define (gen-call v cont)
  (if (eq? cont tail)
      (clump jump-op v 0)
      (clump call-op v cont)))

(define (gen-assign v cont)
  (clump set-op v (gen-noop cont)))

(define (gen-noop cont)
  (if (and (clump? cont) ;; starts with pop?
           (eq? (field0 cont) call-op)
           (eq? (field1 cont) 'arg1))
      (field2 cont) ;; remove pop
      (clump const-op 0 cont))) ;; add dummy value for set!

(define (comp-begin ctx exprs cont)
  (comp ctx
        (car exprs)
        (if (pair? (cdr exprs))
            (clump call-op 'arg1 (comp-begin ctx (cdr exprs) cont))
            cont)))

(define (comp-list ctx exprs k)
  (if (pair? exprs)
      (comp ctx
            (car exprs)
            (comp-list (ctx-cte-set ctx (cons '#f (ctx-cte ctx)))
                       (cdr exprs)
                       k))
      (k ctx)))

(define (lookup var cte i)
  (if (pair? cte)
      (if (eq? (car cte) var)
          i
          (lookup var (cdr cte) (+ i 1)))
      var))

(define (extend vars cte)
  (if (pair? vars)
      (cons (car vars) (extend (cdr vars) cte))
      cte))

(define tail (clump jump-op 'identity 0))

;;;----------------------------------------------------------------------------

(define (comp-program exprs)
  (if (pair? exprs)
      (let ((first (car exprs)))
        (if (and (pair? first) (eq? (car first) 'export))
            (comp-program* (cdr exprs) (exports->alist (cdr first)))
            (comp-program* exprs '())))
      (comp-program* exprs '())))

(define (comp-program* exprs exports)
  (let ((expansion (expand-begin exprs)))
    (let ((live (liveness-analysis expansion exports)))
;;      (pp `(***expansion: ,(uvm->host expansion)))
;;      (pp (uvm->host live))
      (make-procedure
       (clump 0 ;; 0 parameters
              0
              (comp (make-ctx '() live exports)
                    expansion
                    tail))
       '()))))

(define (exports->alist exports)
  (if (pair? exports)
      (let ((x (car exports)))
        (cons (if (symbol? x)
                  (cons x x)
                  (cons (car x) (cadr x)))
              (exports->alist (cdr exports))))
      null))

;;;----------------------------------------------------------------------------

;; Expansion of derived forms, like "define", "cond", "and", "or".

(define (expand expr)

  (cond ((symbol? expr)
         expr)

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eq? first 'quote)
                  (expand-constant (cadr expr)))

                 ((eq? first 'set!)
                  (let ((var (cadr expr)))
                    (cons 'set!
                          (cons var
                                (cons (expand (caddr expr))
                                      '())))))

                 ((eq? first 'if)
                  (cons 'if
                        (cons (expand (cadr expr))
                              (cons (expand (caddr expr))
                                    (cons (expand (cadddr expr))
                                          '())))))

                 ((eq? first 'lambda)
                  (let ((params (cadr expr)))
                    (cons 'lambda
                          (cons params
                                (cons (expand-begin (cddr expr))
                                      '())))))

                 ((eq? first 'let)
                  (let ((binding (car (cadr expr))))
                    (cons 'let
                          (cons (cons (cons (car binding)
                                            (cons (expand (cadr binding))
                                                  '()))
                                      '())
                                (cons (expand-begin (cddr expr))
                                      '())))))

                 ((eq? first 'begin)
                  (expand-begin (cdr expr)))

                 ((eq? first 'define)
                  (let ((pattern (cadr expr)))
                    (if (pair? pattern)
                        (cons 'set!
                              (cons (car pattern)
                                    (cons (expand (cons 'lambda
                                                        (cons (cdr pattern)
                                                              (cddr expr))))
                                          '())))
                        (cons 'set!
                              (cons pattern
                                    (cons (expand (caddr expr))
                                          '()))))))

                 ((eq? first 'and)
                  (expand (if (pair? (cdr expr))
                              (if (pair? (cddr expr))
                                  (cons 'if
                                        (cons (cadr expr)
                                              (cons (cons 'and
                                                          (cddr expr))
                                                    (cons '#f
                                                          '()))))
                                  (cadr expr))
                              '#t)))

                 ((eq? first 'or)
                  (expand (if (pair? (cdr expr))
                              (if (pair? (cddr expr))
                                  (cons 'if
                                        (cons (cadr expr)
                                              (cons '#t ;; not correct but OK
                                                    (cons (cons 'or
                                                                (cddr expr))
                                                          '()))))
                                  (cadr expr))
                              '#f)))

                 ((eq? first 'cond)
                  (expand (if (pair? (cdr expr))
                              (if (eq? 'else (car (cadr expr)))
                                  (cons 'begin (cdr (cadr expr)))
                                  (cons 'if
                                        (cons (car (cadr expr))
                                              (cons (cons 'begin
                                                          (cdr (cadr expr)))
                                                    (cons (cons 'cond
                                                                (cddr expr))
                                                          '())))))
                              '#f)))

                 (else
                  (expand-list expr)))))

        (else
         (expand-constant expr))))

(define (expand-constant x)
  (cond ((eq? x '#f)
         'false)
        ((eq? x '#t)
         'true)
        ((eq? x '())
         'null)
        (else
         (cons 'quote (cons x '())))))
        
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
          (if (and (pair? expr)
                   (eq? (car expr) 'begin))
              (expand-begin* (cdr expr) r)
              (cons (expand expr) r))))
      rest))

(define (expand-list exprs)
  (if (pair? exprs)
      (cons (expand (car exprs))
            (expand-list (cdr exprs)))
      '()))

;;;----------------------------------------------------------------------------

;; Analyse global variable liveness.

(define (liveness-analysis expr exports)
  (let loop ((live-globals (exports->live exports)))
;;    (pp (uvm->host live-globals))
    (reset-defs live-globals)
    (let ((x (liveness expr live-globals)))
      (if (eq? x live-globals)
          live-globals
          (loop x)))))

(define (exports->live exports)
  (if (pair? exports)
      (cons (cons (car (car exports)) '())
            (exports->live (cdr exports)))
      '()))

(define (reset-defs lst)
  (let loop ((lst lst))
    (if (pair? lst)
        (begin
          (set-cdr! (car lst) '())
          (loop (cdr lst)))
        '#f)))

(define (live? var lst)
  (if (pair? lst)
      (let ((x (car lst)))
        (if (eq? var (car x))
            x
            (live? var (cdr lst))))
      '#f))

(define (constant? g)
  (and (pair? (cdr g))
       (null? (cddr g))
       (pair? (cadr g))
       (eq? 'quote (car (cadr g)))))

(define (in? var cte)
  (not (eq? var (lookup var cte 0))))

(define (liveness expr live-globals)

  (define (add-live var)
    (if (live? var live-globals)
        '#f
        (let ((g (cons var '())))
          (set! live-globals (cons g live-globals))
          g)))

  (define (liveness expr cte top?)

    (cond ((symbol? expr)
           (if (in? expr cte) ;; local var?
               '#f
               (add-live expr))) ;; mark the global variable as "live"

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eq? first 'quote)
                    '#f)

                   ((eq? first 'set!)
                    (let ((var (cadr expr)))
                      (let ((val (caddr expr)))
                        (if (in? var cte) ;; local var?
                            (liveness val cte '#f)
                            (let ((g (live? var live-globals))) ;; variable live?
                              (if g
                                  (begin
                                    (set-cdr! g (cons val (cdr g)))
                                    (liveness val cte '#f))
                                  '#f))))))

                   ((eq? first 'if)
                    (liveness (cadr expr) cte '#f)
                    (liveness (caddr expr) cte '#f)
                    (liveness (cadddr expr) cte '#f))

                   ((eq? first 'let)
                    (let ((binding (car (cadr expr))))
                      (liveness (cadr binding) cte '#f)
                      (liveness (caddr expr) (cons (car binding) cte) '#f)))

                   ((eq? first 'begin)
                    (liveness-list (cdr expr) cte))

                   ((eq? first 'lambda)
                    (let ((params (cadr expr)))
                      (liveness (caddr expr) (extend (reverse params) cte) '#f)))

                   (else
                    (liveness-list expr cte)))))

          (else
           '#f)))

  (define (liveness-list exprs cte)
    (if (pair? exprs)
        (begin
          (liveness (car exprs) cte '#f)
          (liveness-list (cdr exprs) cte))
        '#f))

  (liveness expr '() '#t)

  live-globals)

;;;----------------------------------------------------------------------------

(define-macro (quote x)
  `(##quote ,x))

(define (encode-old code)

  (define cons _cons)

  (define syms
    (list->table
     (map (lambda (sym i) (cons (uvm->host sym) i))
          (vector->list primitives)
          (iota (vector-length primitives)))))

  (define (encode-opnd o)
    (if (symbol? o)
        (let* ((sym (uvm->host o))
               (pos (##or (table-ref syms sym #f)
                          (let ((pos (table-length syms)))
                            (table-set! syms sym pos)
                            pos))))
          (list 'sym pos sym))
        (list 'int o)))

  (define (enc code rest)
    (if (clump? code)
        (let ((op (oper code)))
          (if (eq? op jump-op)
              (cons (list 'jump (encode-opnd (opnd code)))
                    rest)
              (cond ((eq? op const-op)
                     (enc (next code)
                          (let ((x (opnd code)))
                            (if (procedure? x)
                                (let ((c (procedure-code x)))
                                  (enc (next c)
                                       (cons (list 'const-proc (car c))
                                             rest)))
                                (cons (list 'const (encode-opnd (opnd code)))
                                      rest)))))
                    ((eq? op get-op)
                     (enc (next code)
                          (cons (list 'get (encode-opnd (opnd code)))
                                rest)))
                    ((eq? op set-op)
                     (enc (next code)
                          (cons (list 'set (encode-opnd (opnd code)))
                                rest)))
                    ((eq? op call-op)
                     (enc (next code)
                          (cons (list 'call (encode-opnd (opnd code)))
                                rest)))
                    ((eq? op if-op)
                     (enc (next code)
                          (enc (opnd code)
                               (cons (list 'if)
                                     rest))))
                    (else
                     (error "unknown op" op)))))
        (error "clump expected")))

  (define (create-symbol-table syms)
    (let ((st (make-vector (table-length syms))))
      (table-for-each
       (lambda (k i)
         (let ((sym (uvm->host k)))
           (vector-set! st i (_symbol->string sym))))
       syms)
      (string-append
       "["
       (append-strings
        (map object->string (vector->list st))
        ",")
       "]")))

  (let ((encoding (enc code (list))))

    (print port: (current-error-port)
           "nb symbols:   " (table-length syms) "\n"
           "symbol-table: " (create-symbol-table syms) "\n")

    encoding))

(define (decode-old encoding)

  (define eq?   _eq?)
  (define pair? _pair?)
  (define car   _car)
  (define cdr   _cdr)
  (define cadr  _cadr)
  (define caddr _caddr)
  (define char->integer  _char->integer)
  (define string->list   _string->list)
  (define symbol->string _symbol->string)

  (define-macro (if a b c) `(##if ,a ,b ,c))

  (define (decode-opnd opnd)
    (if (eq? (car opnd) 'int)
        (cadr opnd)
        (uvm-symbol (caddr opnd))))

  (define (uvm-symbol sym)
    (string->symbol
     (list->string
      (fold cons
            '()
            (reverse (map char->integer
                          (string->list (symbol->string sym))))))))

  (let loop ((probe encoding) (stack 0))
    (if (pair? probe)
        (let ((x (car probe)))

          (define (add-instr op stack)
            (add-instr* op (decode-opnd (cadr x)) stack))

          (define (add-instr* op opnd stack)
            (top-set! stack (clump op opnd (top stack)))
            (loop (cdr probe) stack))

          (let ((first (car x)))
            (cond

             ((eq? first 'const-proc)
              (let ((s (pop stack)))
                (let ((proc
                       (make-procedure
                        (clump (cadr x) 0 (top stack))
                        null)))
                  (add-instr* const-op proc s))))

             ((eq? first 'if)
              (let ((s (pop stack)))
                (add-instr* if-op (top stack) s)))

             ((eq? first 'const)
              (add-instr const-op stack))

             ((eq? first 'get)
              (add-instr get-op stack))

             ((eq? first 'set)
              (add-instr set-op stack))

             ((eq? first 'jump)
              (add-instr jump-op (push stack 0)))

             ((eq? first 'call)
              (add-instr call-op stack))

             (else
              (error "unknown encoding")))))

        (top stack))))

;;;----------------------------------------------------------------------------

(define get-int-short    10) ;; 0 <= N <= 9  are encoded with 1 byte
(define const-int-short  11) ;; 0 <= N <= 10 are encoded with 1 byte
(define const-proc-short  4) ;; 0 <= N <= 3  are encoded with 1 byte
(define jump-sym-short   20) ;; 0 <= N <= 19 are encoded with 1 byte

(define (encode proc)

  (begin
    (define cons _cons)
    (define car _car)
    (define cdr _cdr)
;;    (define < _<)
    (define = eq?)
    (define length _length))

  (let* ((eb               92) ;; encoding base (escape-less strings)
         (eb/2             (quotient eb 2))

         (call-sym-short   (- (* 2 eb/2)
                              (+ const-int-short
                                 (+ const-proc-short
                                    (+ get-int-short
                                       (+ jump-sym-short
                                          17))))))

         (jump-sym-start   0)
         (jump-int-start   (+ jump-sym-start (+ jump-sym-short 2)))
         (call-sym-start   (+ jump-int-start 1))
         (call-int-start   (+ call-sym-start (+ call-sym-short 2)))
         (set-sym-start    (+ call-int-start 1))
         (set-int-start    (+ set-sym-start 2))
         (get-int-start    (+ set-int-start 1))
         (get-sym-start    (+ get-int-start (+ get-int-short 1)))
         (const-int-start  (+ get-sym-start 2))
         (const-sym-start  (+ const-int-start (+ const-int-short 1)))
         (const-proc-start (+ const-sym-start 2))
         (if-start         (+ const-proc-start (+ const-proc-short 1)))

         (_ (pp if-start)))

    (define uvm->host identity)

    (define syms (make-table))

    (define (scan-proc proc)
      (scan (next (procedure-code proc))))

    (define (scan-opnd o pos)
      (cond ((symbol? o)
             (let ((descr
                    (##or (table-ref syms o #f)
                          (let ((descr (clump 0 0 0)))
                            (table-set! syms o descr)
                            descr))))
               (cond ((eq? pos 0)
                      (field0-set! descr (+ 1 (field0 descr))))
                     ((eq? pos 1)
                      (field1-set! descr (+ 1 (field1 descr))))
                     (else
                      (field2-set! descr (+ 1 (field2 descr)))))))
            ((procedure? o)
             (scan-proc o))))

    (define (scan code)
      (if (clump? code)

          (let ((op (oper code)))
            (if (eq? op jump-op)

                (scan-opnd (opnd code) 0) ;; 0 = jump/call

                (begin
                  (cond ((eq? op if-op)
                         (scan (opnd code)))
                        ((eq? op call-op)
                         (scan-opnd (opnd code) 0)) ;; 0 = jump/call
                        ((eq? op get-op)
                         (scan-opnd (opnd code) 1)) ;; 1 = get
                        ((eq? op const-op)
                         (scan-opnd (opnd code) 2))) ;; 2 = const
                  (scan (next code)))))

          (error "clump expected")))

    (define (encode-sym o)
      (let ((descr (table-ref syms o)))
        (field0 descr)))

    (define (encode-long1 code n stream)
      (cons code (encode-n n stream)))

    (define (encode-long2 code0 n stream)
      (let ((s (encode-n n stream)))
        (let ((x (car s)))
          (if (= x (+ eb/2 1))
              (cons (+ code0 1) (cdr s))
              (cons code0 s)))))

    (define (encode-n n stream)
      (encode-n-aux n stream stream))

    (define (encode-n-aux n stream end)
      (let ((q (quotient n eb/2)))
        (let ((r (- n (* q eb/2))))
          (let ((t (cons (if (eq? stream end) r (+ r eb/2)) stream)))
            (if (eq? q 0)
                t
                (encode-n-aux q t end))))))

    (define (enc-proc proc stream)
      (let ((code (procedure-code proc)))
        (let ((nparams (field0 code)))
          (enc (next code)
               (if (< nparams
                      const-proc-short)
                   (cons (+ const-proc-start
                            nparams)
                         stream)
                   (encode-long1 (+ const-proc-start
                                    const-proc-short)
                                 nparams
                                 stream))))))

    (define (number? x) (##if (integer? x) true false))

    (define (enc code stream)
      (if (clump? code)
          (let ((op (oper code)))
            (if (eq? op jump-op)

                (let ((o (opnd code)))
                  (cond ((symbol? o)
                         (let ((x (encode-sym o)))
                           (if (< x jump-sym-short)
                               (cons (+ jump-sym-start x)
                                     stream)
                               (encode-long2 (+ jump-sym-start
                                                jump-sym-short)
                                             x
                                             stream))))
                        ((number? o)
                         (encode-long1 jump-int-start
                                       o
                                       stream))
                        (else
                         (error "can't encode jump" o))))

                (cond ((eq? op const-op)
                       (enc (next code)
                            (let ((o (opnd code)))
                              (cond ((number? o)
                                     (if (< o const-int-short)
                                         (cons (+ const-int-start o)
                                               stream)
                                         (encode-long1 (+ const-int-start
                                                          const-int-short)
                                                       o
                                                       stream)))
                                    ((symbol? o)
                                     (encode-long2 const-sym-start
                                                   (encode-sym o)
                                                   stream))
                                    ((procedure? o)
                                     (enc-proc o stream))
                                    (else
                                     (error "can't encode const" o))))))

                      ((eq? op get-op)
                       (enc (next code)
                            (let ((o (opnd code)))
                              (cond ((number? o)
                                     (if (< o get-int-short)
                                         (cons (+ get-int-start o)
                                               stream)
                                         (encode-long1 (+ get-int-start
                                                          get-int-short)
                                                       o
                                                       stream)))
                                    ((symbol? o)
                                     (encode-long2 get-sym-start
                                                   (encode-sym o)
                                                   stream))
                                    (else
                                     (error "can't encode get" o))))))

                      ((eq? op set-op)
                       (enc (next code)
                            (let ((o (opnd code)))
                              (cond ((symbol? o)
                                     (encode-long2 set-sym-start
                                                   (encode-sym o)
                                                   stream))
                                    ((number? o)
                                     (encode-long1 set-int-start
                                                   o
                                                   stream))
                                    (else
                                     (error "can't encode set" o))))))

                      ((eq? op call-op)
                       (enc (next code)
                            (let ((o (opnd code)))
                              (cond ((symbol? o)
                                     (let ((x (encode-sym o)))
                                       (if (< x call-sym-short)
                                           (cons (+ call-sym-start x)
                                                 stream)
                                           (encode-long2 (+ call-sym-start
                                                            call-sym-short)
                                                         x
                                                         stream))))
                                    ((number? o)
                                     (encode-long1 call-int-start
                                                   o
                                                   stream))
                                    (else
                                     (error "can't encode call" o))))))

                      ((eq? op if-op)
                       (enc (next code)
                            (enc (opnd code)
                                 (cons if-start
                                       stream))))

                      (else
                       (error "unknown op" op)))))
          (error "clump expected")))

    (scan-proc proc)

    (let ((lst
           (list-sort
            (lambda (a b)
              (let ((descr-a (cdr a))
                    (descr-b (cdr b)))
                (##if (eq? (field0 descr-a) (field0 descr-b))
                    (if (eq? (field1 descr-a) (field1 descr-b))
                        (> (field2 descr-a) (field2 descr-b))
                        (> (field1 descr-a) (field1 descr-b)))
                    (> (field0 descr-a) (field0 descr-b)))))
            (table->list syms))))

      (for-each
       (lambda (s i)
         (let ((descr (table-ref syms (car s))))
           (field0-set! descr i)))
       lst
       (iota (_length lst)))

      (let ((stream (enc-proc proc (list))))
        (cons stream (map car lst))))))

(define (decode stream-symbols)

  (begin
    (define cons _cons)
    (define car _car)
    (define cdr _cdr)
;;    (define < _<)
    (define list-ref _list-ref))

  (let* ((eb               92) ;; encoding base (escape-less strings)
         (eb/2             (quotient eb 2))

         (call-sym-short   (- (* 2 eb/2)
                              (+ const-int-short
                                 (+ const-proc-short
                                    (+ get-int-short
                                       (+ jump-sym-short
                                          17))))))

         (jump-sym-start   0)
         (jump-int-start   (+ jump-sym-start (+ jump-sym-short 2)))
         (call-sym-start   (+ jump-int-start 1))
         (call-int-start   (+ call-sym-start (+ call-sym-short 2)))
         (set-sym-start    (+ call-int-start 1))
         (set-int-start    (+ set-sym-start 2))
         (get-int-start    (+ set-int-start 1))
         (get-sym-start    (+ get-int-start (+ get-int-short 1)))
         (const-int-start  (+ get-sym-start 2))
         (const-sym-start  (+ const-int-start (+ const-int-short 1)))
         (const-proc-start (+ const-sym-start 2))
         (if-start         (+ const-proc-start (+ const-proc-short 1)))

         (_ (pp if-start))

         (stream (car stream-symbols))
         (symbols (cdr stream-symbols)))

    (define stack 0)

    (define (decode-int n stream)
      (let ((x (car stream)))
        (let ((y (* n eb/2)))
          (if (< x eb/2)
              (cons (+ y x) (cdr stream))
              (decode-int (+ y (- x eb/2)) (cdr stream))))))

    (define (decode-sym n stream)
      (let ((int-stream (decode-int n stream)))
        (cons (sym (car int-stream))
              (cdr int-stream))))

    (define (sym n)
      (list-ref symbols n))

    (define (dec stream)
      (let ((x (car stream)))

        (define (add op opnd)
          (add* op opnd (cdr stream)))

        (define (add* op opnd stream)
          (field0-set! stack (clump op opnd (field0 stack)))
          (dec stream))

        (define (add-const-int n)
          (if (< n const-int-short)
              (add const-op n)
              (let ((int-stream (decode-int 0 (cdr stream))))
                (add* const-op (car int-stream) (cdr int-stream)))))

        (define (add-const-sym n)
          (let ((sym-stream (decode-sym n (cdr stream))))
            (add* const-op (car sym-stream) (cdr sym-stream))))

        (define (add-const-proc n)
          (if (< n const-proc-short)
              (add*-const-proc n (cdr stream))
              (let ((int-stream (decode-int 0 (cdr stream))))
                (add*-const-proc (car int-stream) (cdr int-stream)))))

        (define (add*-const-proc n stream)
          (let ((proc (make-procedure
                       (clump n 0 (field0 stack))
                       null)))
            (set! stack (field1 stack))
            (if (eq? stack 0)
                proc
                (add* const-op proc stream))))

        (cond ((< x call-sym-start) ;; jump
               (set! stack (clump 0 stack 0))
               (let ((n (- x jump-sym-start)))
                 (if (< n jump-sym-short)
                     (add jump-op (sym n))
                     (let ((n (- n jump-sym-short)))
                       (if (< n 2)
                           (let ((sym-stream
                                  (decode-sym n (cdr stream))))
                             (add* jump-op (car sym-stream) (cdr sym-stream)))
                           (let ((int-stream
                                  (decode-int 0 (cdr stream))))
                             (add* jump-op (car int-stream) (cdr int-stream))))))))

              ((< x set-sym-start) ;; call
               (let ((n (- x call-sym-start)))
                 (if (< n call-sym-short)
                     (add call-op (sym n))
                     (let ((n (- n call-sym-short)))
                       (if (< n 2)
                           (let ((sym-stream
                                  (decode-sym n (cdr stream))))
                             (add* call-op (car sym-stream) (cdr sym-stream)))
                           (let ((int-stream
                                  (decode-int 0 (cdr stream))))
                             (add* call-op (car int-stream) (cdr int-stream))))))))

              ((< x get-int-start) ;; set
               (let ((n (- x set-sym-start)))
                 (if (< n 2)
                     (let ((sym-stream
                            (decode-sym n (cdr stream))))
                       (add* set-op (car sym-stream) (cdr sym-stream)))
                     (let ((int-stream
                            (decode-int 0 (cdr stream))))
                       (add* set-op (car int-stream) (cdr int-stream))))))

              ((< x const-int-start) ;; get
               (let ((n (- x get-int-start)))
                 (if (< n get-int-short)
                     (add get-op n)
                     (let ((n (- n get-int-short)))
                       (if (< n 1)
                           (let ((int-stream
                                  (decode-int 0 (cdr stream))))
                             (add* get-op (car int-stream) (cdr int-stream)))
                           (let ((sym-stream
                                  (decode-sym 0 (cdr stream))))
                             (add* get-op (car sym-stream) (cdr sym-stream))))))))

              ((< x if-start) ;; const
               (cond ((< x const-sym-start)
                      (add-const-int (- x const-int-start)))
                     ((< x const-proc-start)
                      (add-const-sym (- x const-sym-start)))
                     (else
                      (add-const-proc (- x const-proc-start)))))

              (else
               (let ((code (field0 stack)))
                 (set! stack (field1 stack))
                 (add if-op code))))))

;;    (pp `(stream= ,stream))
;;    (pp symbols)
    (dec stream)))

;;;----------------------------------------------------------------------------

(define (read-program)
  (let ((x (read)))
    (if (eq? x (- 0 1))
        (list)
        (cons x (read-program)))))

(define (read-from-string str)
  (with-input-from-string str read-program))

(define (read-from-file path)
  (with-input-from-file path read-program))

(define (comp-file path)
  (let ((program (read-from-file path)))
    (let ((proc (comp-program program)))
      (let ((stream-symbols (encode proc)))
        (let ((stream (_car stream-symbols)))
;;          (pp stream)
          (let ((symbols (_cdr stream-symbols)))
            (pp (_length stream))
            (let ((encoded-program
                   (string-append
                    (append-strings
                     (map (lambda (s) (_symbol->string (uvm->host s)))
                          (_cdr stream-symbols))
                     ",")
                    " "
                    (_list->string
                     (map (lambda (n)
                            (let ((c (+ n 35)))
                              (_integer->char (if (= c 92) 33 c))))
                          stream)))))
              (pp encoded-program)
              (pp (string-length encoded-program))
              (let ((p (decode stream-symbols)))
;;                (pp proc)
;;                (pp p)
                (pp (_equal? proc p))
                ))))))))

(define (main path)
  (comp-file path))

;;;----------------------------------------------------------------------------
