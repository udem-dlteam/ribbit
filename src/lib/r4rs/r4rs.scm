;; R4RS as a library for ribbit.

(##include "./types.scm")
(##include "./bool.scm")
(##include "./io.scm")
(##include "./pair-list.scm")
(##include "./numbers.scm")
(##include "./strings.scm")
(##include "./vectors.scm")

;;;----------------------------------------------------------------------------

;; Symbols (R4RS section 6.4).

(define global-var-ref field0)
(define global-var-set! field0-set!)

;;;----------------------------------------------------------------------------


;; Characters (R4RS section 6.6).

(define char? integer?)

(define char=? eqv?)
(define char<? <)
(define char>? >)
(define char<=? <=)
(define char>=? >=)

;;(define char-ci=? eqv?)
;;(define char-ci<? <)
;;(define char-ci>? >)
;;(define char-ci<=? <=)
;;(define char-ci>=? >=)

;;(define (char-alphabetic? c) ...)
;;(define (char-numeric? c) ...)
;;(define (char-whitespace? c) ...)
;;(define (char-upper-case? c) ...)
;;(define (char-lower-case? c) ...)

(define char->integer id)
(define integer->char id)

;;(define (char-upcase c) ...)
;;(define (char-downcase c) ...)

;;;----------------------------------------------------------------------------

;; Control features (R4RS section 6.9).

(define (make-procedure code env) (rib code env procedure-type))
(define procedure-code field0)
(define procedure-env field1)

;;(define (apply proc . args) ...)

(define (map proc lst)
  (if (pair? lst)
      (cons (proc (car lst)) (map proc (cdr lst)))
      '()))

(define (for-each proc lst)
  (if (pair? lst)
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))
      #f))

;; First-class continuations.

(define (call/cc receiver)
  (let ((c (field1 (field1 (close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (field1 (field1 (close #f)))))
                  (field0-set! c2 (field0 c)) ;; set "stack" field
                  (field2-set! c2 (field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

;;;----------------------------------------------------------------------------

;; Compiler from Ribbit Scheme to RVM code.

(define jump/call-op 0)
(define set-op       1)
(define get-op       2)
(define const-op     3)
(define if-op        4)

(define (add-nb-args nb tail)
  (if ##feature-arity-check
    (rib const-op
         nb
         tail)
    tail))

(define (comp cte expr cont)
  (cond ((symbol? expr)
         (rib get-op (lookup expr cte 0) cont))

        ((pair? expr)
         (let ((first (car expr)))
           (cond ((eqv? first 'quote)
                  (rib const-op (cadr expr) cont))

                 ((or (eqv? first 'set!) (eqv? first 'define))
                  (comp cte
                        (caddr expr)
                        (gen-assign (lookup (cadr expr) cte 1)
                                    cont)))

                 ((eqv? first 'if)
                  (comp cte
                        (cadr expr)
                        (rib if-op
                             (comp cte (caddr expr) cont)
                             (comp cte (cadddr expr) cont))))

                 ((eqv? first 'lambda)
                  (let ((params (cadr expr)))
                    (rib const-op
                         (make-procedure
                          (rib (* 2 (length params))
                               0
;;                               #; ;; support for single expression in body
;;                               (comp (extend params
;;                                             (cons #f
;;                                                   (cons #f
;;                                                         cte)))
;;                                     (caddr expr)
;;                                     tail)
                               ;#; ;; support for multiple expressions in body
                               (comp-begin (extend params
                                                   (cons #f
                                                         (cons #f
                                                               cte)))
                                           (cddr expr)
                                           tail))
                          '())
                         (if (null? cte)
                             cont
                             (add-nb-args
                               1
                               (gen-call 'close cont))))))

;#; ;; support for begin special form
                 ((eqv? first 'begin)
                  (comp-begin cte (cdr expr) cont))

;#; ;; support for single armed let special form
                 ((eqv? first 'let)
                  (let ((binding (car (cadr expr))))
                    (comp-bind cte
                               (car binding)
                               (cadr binding)
;;                               #; ;; support for single expression in body
;;                               (caddr expr)
                               ;#; ;; support for multiple expressions in body
                               (cddr expr)
                               cont)))

;#; ;; support for and special form
                 ((eqv? first 'and)
                  (comp cte
                        (if (pair? (cdr expr))
                            (let ((second (cadr expr)))
                              (if (pair? (cddr expr))
                                  (build-if second
                                            (cons 'and (cddr expr))
                                            #f)
                                  second))
                            #t)
                        cont))

;#; ;; support for or special form
                 ((eqv? first 'or)
                  (comp cte
                        (if (pair? (cdr expr))
                            (let ((second (cadr expr)))
                              (if (pair? (cddr expr))
                                  (list 'let
                                         (list (list '_ second))
                                         (build-if '_
                                                   '_
                                                   (cons 'or (cddr expr))))
                                  second))
                            #f)
                        cont))

;#; ;; support for cond special form
                 ((eqv? first 'cond)
                  (comp cte
                        (if (pair? (cdr expr))
                            (if (eqv? 'else (car (cadr expr)))
                                (cons 'begin (cdr (cadr expr)))
                                (build-if (car (cadr expr))
                                          (cons 'begin (cdr (cadr expr)))
                                          (cons 'cond (cddr expr))))
                            #f)
                        cont))

                 (else
;;                  #; ;; support for calls with only variable in operator position
;;                  (comp-call cte
;;                             (cdr expr)
;;                             (cons first cont))
                  ;#; ;; support for calls with any expression in operator position
                  (let ((args (cdr expr)))
                    (if (symbol? first)
                        (comp-call cte
                                   args
                                   (length args)
                                   (cons first cont))
                        (comp-bind cte
                                   '_
                                   first
;;                                   #; ;; support for single expression in body
;;                                   (cons '_ args)
                                   ;#; ;; support for multiple expressions in body
                                   (list (cons '_ args))
                                   cont)))))))

        (else
         ;; self-evaluating
         (rib const-op expr cont))))

;#; ;; support for and, or, cond special forms
(define (build-if a b c) (cons 'if (list a b c)))

(define (comp-bind cte var expr body cont)
  (comp cte
        expr
;;        #; ;; support for single expression in body
;;        (comp (cons var cte)
;;              body
;;              (if (eqv? cont tail)
;;                  cont
;;                  (rib jump/call-op ;; call
;;                       'arg2
;;                       cont)))
        ;#; ;; support for multiple expressions in body
        (comp-begin (cons var cte)
                    body
                    (if (eqv? cont tail)
                        cont
                        (add-nb-args
                          2
                          (rib jump/call-op ;; call
                               'arg2
                                cont))))))

(define (comp-begin cte exprs cont)
  (comp cte
        (car exprs)
        (if (pair? (cdr exprs))
          (add-nb-args
            2
            (rib jump/call-op ;; call
                 'arg1
                 (comp-begin cte (cdr exprs) cont)))
            cont)))

(define (gen-call v cont)
  (if (eqv? cont tail)
      (rib jump/call-op v 0)      ;; jump
      (rib jump/call-op v cont))) ;; call

(define (gen-assign v cont)
  (rib set-op v (gen-noop cont)))

(define (gen-noop cont)
  (if (and (rib? cont) ;; starts with pop?
           (eqv? (field0 cont) jump/call-op) ;; call?
           (eqv? (field1 cont) 'arg1)
           (rib? (field2 cont)))
      (field2 cont) ;; remove pop
      (rib const-op 0 cont))) ;; add dummy value for set!

(define (comp-call cte exprs nb-args var-cont)
  (if (pair? exprs)
      (comp cte
            (car exprs)
            (comp-call (cons #f cte)
                       (cdr exprs)
                       nb-args
                       var-cont))
      (let ((var (car var-cont)))
        (let ((cont (cdr var-cont)))
          (let ((v (lookup var cte 0)))
            (add-nb-args
              nb-args
              (gen-call (if (and (integer? v) ##feature-arity-check) (+ 1 v) v) cont)))))))

(define (lookup var cte i)
  (if (pair? cte)
      (if (eqv? (car cte) var)
          i
          (lookup var (cdr cte) (+ i 1)))
      var))

(define (extend vars cte)
  (if (pair? vars)
      (cons (car vars) (extend (cdr vars) cte))
      cte))

(define tail (add-nb-args 1 (rib jump/call-op 'id 0))) ;; jump

(define (display-rib rib)
  (display "[")
  (if (rib? (field0 rib))
    (display-rib (field0 rib))
    (display (field0 rib)))
  (display " ")
  (if (rib? (field1 rib))
    (display-rib (field1 rib))
    (display (field1 rib)))
  (display " ")
  (if (rib? (field2 rib))
    (display-rib (field2 rib))
    (display (field2 rib)))
  (display "]"))


(define (compile expr) ;; converts an s-expression to a procedure
  (let ((foo (comp '() expr tail)))
    (make-procedure (rib 0 0 foo) '())))

(define (eval expr)
  ((compile expr)))

(define (repl)
  (putchar2 62 32) ;; #\> and space
  (let ((expr (read)))
    (if (eof-object? expr)
        (newline)
        (begin
          (write (eval expr))
          (newline)
          (repl)))))

(define (fold func base lst)
  (if (pair? lst)
    (fold func (func (car lst) base) (cdr lst))
    acc))

(define (error msg info)
  (display msg)
  (display " ")
  (write info)
  (newline)
  (exit 1))

;;;----------------------------------------------------------------------------
