#!/usr/bin/env gsi

;;; Small-Scheme compiler.

(include "uvm-compat.scm")

(include "lib1.scm")

;;;----------------------------------------------------------------------------

(define primitives
  (vector $identity
          $arg1
          $arg2
          $close
          $cons
          $clump?
          $field0
          $field1
          $field2
          $field0-set!
          $field1-set!
          $field2-set!
          $eq?
          $<
          $+
          $-
          $*
          $quotient
          $getchar
          $putchar))


#;(begin
(global-var-set! $identity    (make-procedure 0 null))
(global-var-set! $arg1        (make-procedure 1 null))
(global-var-set! $arg2        (make-procedure 2 null))
(global-var-set! $cons        (make-procedure 3 null))
(global-var-set! $clump?      (make-procedure 4 null))
(global-var-set! $field0      (make-procedure 5 null))
(global-var-set! $field1      (make-procedure 6 null))
(global-var-set! $field2      (make-procedure 7 null))
(global-var-set! $field0-set! (make-procedure 8 null))
(global-var-set! $field1-set! (make-procedure 9 null))
(global-var-set! $field2-set! (make-procedure 10 null))
(global-var-set! $eq?         (make-procedure 11 null))
(global-var-set! $<           (make-procedure 12 null))
(global-var-set! $+           (make-procedure 13 null))
(global-var-set! $-           (make-procedure 14 null))
(global-var-set! $*           (make-procedure 15 null))
(global-var-set! $quotient    (make-procedure 16 null))
(global-var-set! $getchar     (make-procedure 17 null))
(global-var-set! $putchar     (make-procedure 18 null))
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
        ((eq? x false)
         #f)
        ((eq? x true)
         #t)
        ((eq? x null)
         (list))
        (else
         x)))

(define entry-op 'entry)
(define const-op 'const)
(define get-op   'get)
(define set-op   'set)
(define if-op    'if)
(define jump-op  'jump)
(define call-op  'call)

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
  (let* ((y (top stack))
         (s (pop stack))
         (x (top s)))
    (field0-set! stack x)
    (field1-set! stack y)
    (field2-set! stack 0)
    (field0-set! s stack)
    s))

(define (primitive stack fn)
;;  (pp `(fn: ,(uvm->host fn)))
  (cond
   ((eq? fn $identity)    stack)
   ((eq? fn $arg1)        (pop stack))
   ((eq? fn $arg2)        (top-set! (pop stack) (top stack)) (pop stack))
   ((eq? fn $close)       (prim-close stack))
   ((eq? fn $cons)        (prim-cons stack))
   ((eq? fn $clump?)      (prim1 stack clump?))
   ((eq? fn $field0)      (prim1 stack field0))
   ((eq? fn $field1)      (prim1 stack field1))
   ((eq? fn $field2)      (prim1 stack field2))
   ((eq? fn $field0-set!) (prim2 stack field0-set!))
   ((eq? fn $field1-set!) (prim2 stack field1-set!))
   ((eq? fn $field2-set!) (prim2 stack field2-set!))
   ((eq? fn $eq?)         (prim2 stack eq?))
   ((eq? fn $<)           (prim2 stack <))
   ((eq? fn $+)           (prim2 stack +))
   ((eq? fn $-)           (prim2 stack -))
   ((eq? fn $*)           (prim2 stack *))
   ((eq? fn $quotient)    (prim2 stack quotient))
   ((eq? fn $getchar)     (prim0 stack getchar))
   ((eq? fn $putchar)     (prim1 stack putchar))
   (else                  (error "unknown primitive" fn))))

(define (run pc stack)
  (let ((op (oper pc)))
;;    (pp `(op: ,op stack: ,(uvm->host stack)))
    (cond ((eq? op entry-op)
           (run (next pc) stack)) ;; ignore
          ((eq? op const-op)
           (run (next pc) (push stack (opnd pc))))
          ((eq? op get-op)
           (run (next pc) (push stack (ref stack (opnd pc)))))
          ((eq? op call-op)
           (let ((proc (ref stack (opnd pc))))
             (pp `(call proc: ,proc))
             (let ((code (procedure-code proc)))
               (pp `(code: ,code))
               (if (clump? code)
                   ...
                   (run (next pc) (primitive stack (vector-ref primitives code)))))))
          ((eq? op jump-op)
           (let ((proc (ref stack (opnd pc))))
             (pp `(jump proc: ,proc))
             (let ((code (procedure-code proc)))
               (pp `(code: ,code))
               (if (clump? code)
                   ...
                   (return (primitive stack (vector-ref primitives code)))))))
          ((eq? op if-op)
           (let ((s (pop stack)))
             (pp `(if test: ,(top stack)))
             (if (top stack)
                 (run (opnd pc) s)
                 (run (next pc) s))))
          (else
           (error "unknown operation" op)))))

(define (return stack)
  (pp `(return: ,(uvm->host (car stack)) stack: ,(uvm->host stack)))
  stack)

(define (ref stack var)
  (if (clump? var)
      (global-var-ref var)
      (list-ref stack var)))

;;;----------------------------------------------------------------------------

;; The compiler from SScheme to uVM code.

(define (comp expr cte cont)
  (cond ((symbol? expr)
         (clump get-op (lookup expr cte 0) cont))
        ((pair? expr)
         (let ((first (car expr)))
           (cond ((eq? first $quote)
                  (clump const-op (cadr expr) cont))
                 ((eq? first $set!)
                  (comp-assign (cadr expr)
                               (caddr expr)
                               cte
                               cont))
                 ((eq? first $define)
                  (let ((pattern (cadr expr)))
                    (if (pair? pattern)
                        (comp-assign (car pattern)
                                     (cons $lambda
                                           (cons (cdr pattern)
                                                 (cddr expr)))
                                     cte
                                     cont)
                        (comp-assign pattern
                                     (caddr expr)
                                     cte
                                     cont))))
                 ((eq? first $if)
                  (let ((cont-false (comp (cadddr expr) cte cont)))
                    (let ((cont-true (comp (caddr expr) cte cont)))
                      (let ((cont-test (clump if-op cont-true cont-false)))
                        (comp (cadr expr) cte cont-test)))))
                 ((eq? first $and)
                  (comp (if (pair? (cdr expr))
                            (if (pair? (cddr expr))
                                (cons $if
                                      (cons (cadr expr)
                                            (cons (cons $and
                                                        (cddr expr))
                                                  (cons $false
                                                        null))))
                                (cadr expr))
                            $true)
                        cte
                        cont))
                 ((eq? first $or)
                  (comp (if (pair? (cdr expr))
                            (if (pair? (cddr expr))
                                (cons $if
                                      (cons (cadr expr)
                                            (cons $true ;; not correct but OK
                                                  (cons (cons $or
                                                              (cddr expr))
                                                        null))))
                                (cadr expr))
                            $false)
                        cte
                        cont))
                 ((eq? first $cond)
                  (comp (if (pair? (cdr expr))
                            (if (eq? $else (car (cadr expr)))
                                (cons $begin (cdr (cadr expr)))
                                (cons $if
                                      (cons (car (cadr expr))
                                            (cons (cons $begin
                                                        (cdr (cadr expr)))
                                                  (cons (cons $cond
                                                              (cddr expr))
                                                        null)))))
                            $false)
                        cte
                        cont))
                 ((eq? first $let)
                  (let ((binding (car (cadr expr))))
                    (comp (cadr binding)
                          cte
                          (comp-begin (cdr (cdr expr))
                                      (cons (car binding) cte)
                                      (if (eq? cont tail)
                                          cont
                                          (clump call-op $arg2 cont))))))
                 ((eq? first $begin)
                  (comp-begin (cdr expr) cte cont))
                 ((eq? first $lambda)
                  (let ((params (cadr expr)))
                    (clump const-op
                           (make-procedure
                            (clump (length params)
                                   0
                                   (comp-begin (cddr expr)
                                               (extend params cte)
                                               tail))
                            null)
                           (if (null? cte)
                               cont
                               (comp-call $close cont)))))
                 (else
                  (comp-list (cdr expr)
                             cte
                             (lambda (cte)
                               (comp-call (lookup (car expr) cte 0) cont)))))))
        (else
         ;; self-evaluating
         (clump const-op expr cont))))

(define (comp-call v cont)
  (if (eq? cont tail)
      (clump jump-op v 0)
      (clump call-op v cont)))

(define (comp-assign var expr cte cont)
  (comp expr
        cte
        (clump set-op (lookup var cte 1) cont)))

(define (comp-begin exprs cte cont)
  (comp (car exprs)
        cte
        (if (pair? (cdr exprs))
            (clump call-op $arg1 (comp-begin (cdr exprs) cte cont))
            cont)))

(define (comp-list exprs cte k)
  (if (pair? exprs)
      (comp (car exprs)
            cte
            (comp-list (cdr exprs)
                       (cons false cte)
                       k))
      (k cte)))

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

(define tail (clump jump-op $identity null))

;;;----------------------------------------------------------------------------

(define (comp-program exprs)
  (comp-begin exprs null tail))

;;;----------------------------------------------------------------------------

(define-macro (quote x)
  `(##quote ,x))

(define (encode code)

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

  (let ((encoding (enc code (list))))

    #;
    (print port: (current-error-port)
           "nb symbols: " (table-length syms) "\n")

    encoding))

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
    (let ((code (comp-program program)))
      (let ((encoding (encode code)))
        (for-each pp encoding)))))

(define (main path)
  (comp-file path))

;;;----------------------------------------------------------------------------
