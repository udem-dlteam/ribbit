(##include-once "./bool.scm")
(##include-once "./types.scm")
(##include-once "./pair-list.scm")
(##include-once "./io.scm")

(##include-once "./control.scm")

(cond-expand
  ((host js)
   (define-primitive
     (welcome-msg)
     "() => (console.log(`
              ____________________
             |                    |
             | Welcome to Ribbit! |
             |                    |
    λ        | - Rib the Frog     |
  @. .@  --- |____________________|
 (-----)
( >___< )
^^ ~~~ ^^`), true),"))

  ((host py)
   (define-primitive
     (welcome-msg)
     "lambda: push(print('''
              ____________________
             |                    |
             | Welcome to Ribbit! |
             |                    |
    λ        | - Rib the Frog     |
  @. .@  --- |____________________|
 (-----)
( >___< )
^^ ~~~ ^^''')),"))

  ((host c)
   (define-primitive
     (welcome-msg)
     "printf(\"\\
              ____________________\\n\\
             |                    |\\n\\
             | Welcome to Ribbit! |\\n\\
             |                    |\\n\\
    λ        | - Rib the Frog     |\\n\\
  @. .@  --- |____________________|\\n\\
 (-----)\\n\\
( >___< )\\n\\
^^ ~~~ ^^\\n\");
   push2(NIL, PAIR_TAG);
   break;")))

;; Compiler from Ribbit Scheme to RVM code.

(define eval@jump/call-op 0)
(define eval@set-op       1)
(define eval@get-op       2)
(define eval@const-op     3)
(define eval@if-op        4)


(define eval@pair-type      0)
(define eval@procedure-type 1)
(define eval@symbol-type    2)
(define eval@string-type    3)
(define eval@vector-type    4)
(define eval@singleton-type 5)

(define (eval@add-nb-args nb tail)
  (##rib eval@const-op
       nb
       tail))

(define (eval@improper-length lst)
  (if (pair? lst)
    (##+ 1 (eval@improper-length (##field1 lst)))
    0))

(define (eval@improper-list->list lst tail)
  (if (pair? lst)
    (eval@improper-list->list (##field1 lst) (cons (##field0 lst) tail))
    (reverse (cons lst tail))))

(define (last-item lst)
  (if (pair? lst)
    (last-item (##field1 lst))
    lst))


(define (eval@comp cte expr cont)
  (cond ((symbol? expr)
         (##rib eval@get-op (eval@lookup expr cte 0) cont))

        ((pair? expr)
         (let ((first (##field0 expr)))
           (cond ((##eqv? first 'quote)
                  (##rib eval@const-op (cadr expr) cont))

                 ((##eqv? first 'quasiquote)
                  (eval@comp cte
                             (eval@expand-qq (cadr expr))
                             cont))

                 ((or (##eqv? first 'set!) (##eqv? first 'define))
                  (let ((pattern (cadr expr)))
                    (if (pair? pattern)
                      (eval@comp cte
                                 (cons 'lambda (cons (##field1 pattern) (cddr expr)))
                                 (eval@gen-assign (eval@lookup (##field0 pattern) cte 1)
                                                  cont))
                      (eval@comp cte
                                 (caddr expr)
                                 (eval@gen-assign (eval@lookup pattern cte 1)
                                                  cont)))))

                 ((##eqv? first 'if)
                  (eval@comp
                    cte
                    (cadr expr)
                    (##rib
                     eval@if-op
                     (eval@comp cte (caddr expr) cont)
                     (if (null? (cdddr expr))
                       (eval@comp cte 0 cont) ;; push some dummy value
                       (eval@comp cte (cadddr expr) cont)))))

                 ((##eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (variadic (or (symbol? params) (not (null? (last-item params)))))
                         (nb-params (if variadic (##+ 1 (##* 2 (eval@improper-length params))) (##* 2 (length params))))
                         (params
                           (if variadic
                             (eval@improper-list->list params '())
                             params)))
                    (##rib eval@const-op
                     (eval@make-procedure
                       (##rib nb-params
                        0
                        (eval@comp-begin
                          (eval@extend params
                                       (cons #f
                                             (cons #f
                                                   cte)))
                          (cddr expr)
                          eval@tail))
                       '())
                     (if (null? cte)
                       cont
                       (if-feature
                         prim-no-arity
                         (eval@gen-call '##close cont)
                         (eval@add-nb-args
                           1
                           (eval@gen-call '##close cont)))))))

                 ;#; ;; support for begin special form
                 ((##eqv? first 'begin)
                  (eval@comp-begin cte (##field1 expr) cont))

                 ;#; ;; support for let special form
                 ((##eqv? first 'let)
                  (let ((bindings (cadr expr)))
                    (eval@comp-bind
                      cte
                      (map car bindings)
                      (map cadr bindings)
                      (cddr expr)
                      cte
                      cont)))

                 ;#; ;; support for single armed let special form
                 ((##eqv? first 'letrec)
                  (let ((bindings (cadr expr)))
                    (eval@comp
                      cte
                      (cons 'let
                            (cons (map (lambda (binding)
                                         (list (##field0 binding) #f))
                                       bindings)
                                  (append (map (lambda (binding)
                                                 (list 'set! (##field0 binding) (cadr binding)))
                                               bindings)
                                          (cddr expr))))
                      cont)))

                 ;#; ;; support for and special form
                 ((##eqv? first 'and)
                  (eval@comp
                    cte
                    (if (pair? (##field1 expr))
                      (let ((second (cadr expr)))
                        (if (pair? (cddr expr))
                          (eval@build-if
                            second
                            (cons 'and (cddr expr))
                            #f)
                          second))
                      #t)
                    cont))

                 ;#; ;; support for or special form
                 ((##eqv? first 'or)
                  (eval@comp cte
                        (if (pair? (##field1 expr))
                          (let ((second (cadr expr)))
                            (if (pair? (cddr expr))
                              (list 'let
                                    (list (list '_ second))
                                    (eval@build-if
                                      '_
                                      '_
                                      (cons 'or (cddr expr))))
                              second))
                          #f)
                        cont))

                 ;#; ;; support for cond special form
                 ((##eqv? first 'cond)
                  (eval@comp cte
                        (if (pair? (##field1 expr))
                          (if (##eqv? (caadr expr) 'else)
                            (cons 'begin (cdadr expr))
                            (eval@build-if
                              (caadr expr)
                              (cons 'begin (cdadr expr))
                              (cons 'cond (cddr expr))))
                          #f)
                        cont))

                 ((##eqv? first 'case)
                  (let ((key (##field0 (##field1 expr))))
                    (let ((clauses (##field1 (##field1 expr))))
                      (eval@comp
                        cte
                        (if (pair? clauses)
                          (let ((clause (##field0 clauses)))
                            (if (##eqv? (##field0 clause) 'else)
                              (cons 'begin (##field1 clause))
                              (eval@build-if
                                (cons 'memv (cons key (list (list 'quote (##field0 clause)))))
                                (cons 'begin (##field1 clause))
                                (cons 'case (cons key (##field1 clauses))))))
                          #f)
                        cont))))

                 (else
                   ;;                  #; ;; support for calls with only variable in operator position
                   ;;                  (eval@comp-call cte
                   ;;                             (##field1 expr)
                   ;;                             (cons first cont))
                   ;#; ;; support for calls with any expression in operator position
                   (let ((args (##field1 expr)))
                     (if (symbol? first)
                       (begin
                         ;(if-feature
                         ;(not no-err)
                         ;(if (not (procedure? (eval first)))
                         ;  (crash (string-append
                         ;           "Cannot call: "
                         ;           (symbol->string first)))))

                         (eval@comp-call
                           cte
                           args
                           (length args)
                           (cons first cont)))
                       (eval@comp-bind cte
                                       '(_)
                                       (list first)
                                       ;;                                   #; ;; support for single expression in body
                                       ;;                                   (cons '_ args)
                                       ;#; ;; support for multiple expressions in body
                                       (list (cons '_ args))
                                       cte
                                       cont)))))))

        (else
          ;; self-evaluating
          (##rib eval@const-op expr cont))))

;#; ;; support for and, or, cond special forms
(define (eval@build-if a b c) (cons 'if (list a b c)))

(define (eval@expand-constant expr)
  (##qq-list 'quote expr))

(define (eval@expand-qq expr)
  (let parse ((x expr) (depth 1))
    (cond
      ((not (pair? x))
       (if (vector? x)
         (##qq-list '##qq-list->vector (parse (##field0 x) depth))
         (eval@expand-constant x)))
      ((##eqv? (##field0 x) 'unquote)
       (if (##eqv? depth 1)
         (cadr x)
         (##qq-list '##qq-cons (eval@expand-constant 'unquote) (parse (##field1 x) (##- depth 1)))))
      ((and (pair? (##field0 x)) (##eqv? (caar x) 'unquote-splicing))
       (if (##eqv? depth 1)
         (##qq-list '##qq-append (cadar x) (parse (##field1 x) depth))
         (##qq-list '##qq-cons (##qq-list '##qq-cons (eval@expand-constant 'unquote-splicing) (parse (cdar x) (##- depth 1))) (parse (##field1 x) depth))))
      ((##eqv? (##field0 x) 'quasiquote)
       (##qq-list '##qq-cons (eval@expand-constant 'quasiquote) (parse (##field1 x) (##+ depth 1))))
      (else
        (##qq-list '##qq-cons (parse (##field0 x) depth) (parse (##field1 x) depth))))))

(define (eval@comp-bind cte vars exprs body body-cte cont)
  (if (pair? vars)
    (let ((var (##field0 vars))
          (expr (##field0 exprs)))
      (eval@comp
        cte
        expr
        ;#; ;; support for multiple expressions in body
        (eval@comp-bind
          (cons #f cte)
          (##field1 vars)
          (##field1 exprs)
          body
          (cons var body-cte)
          (if (##eqv? cont eval@tail)
            cont
            (if-feature
              prim-no-arity
              (##rib eval@jump/call-op ;; call
               '##arg2
               cont)
              (eval@add-nb-args
                2
                (##rib eval@jump/call-op ;; call
                 '##arg2
                 cont)))))))
    (eval@comp-begin
      body-cte
      body
      cont)))

(define (eval@comp-begin cte exprs cont)
  (eval@comp cte
        (##field0 exprs)
        (if (pair? (##field1 exprs))
          (if-feature
            prim-no-arity
            (##rib eval@jump/call-op ;; call
             '##arg1
             (eval@comp-begin cte (##field1 exprs) cont))
            (eval@add-nb-args
              2
              (##rib eval@jump/call-op ;; call
               '##arg1
               (eval@comp-begin cte (##field1 exprs) cont))))
            cont)))

(define (eval@gen-call v cont)
  (if (##eqv? cont eval@tail)
      (##rib eval@jump/call-op v 0)      ;; jump
      (##rib eval@jump/call-op v cont))) ;; call

(define (eval@gen-assign v cont)
  (##rib eval@set-op v
       (if (and (##rib? cont) ;; starts with pop?
                (##eqv? (##field0 cont) eval@jump/call-op) ;; call?
                (##eqv? (##field1 cont) '##arg1)
                (##rib? (##field2 cont)))
         (##field2 cont) ;; remove pop
         (##rib eval@const-op 0 cont))))

;; (define (gen-noop cont)
;;   (if (and (rib? cont) ;; starts with pop?
;;            (eqv? (field0 cont) eval@jump/call-op) ;; call?
;;            (eqv? (field1 cont) 'arg1)
;;            (rib? (field2 cont)))
;;       (field2 cont) ;; remove pop
;;       (##rib const-op 0 cont))) ;; add dummy value for set!

(define (eval@comp-call cte exprs nb-args var-cont)
  (if (pair? exprs)
    (eval@comp
      cte
      (##field0 exprs)
      (eval@comp-call (cons #f cte)
                      (##field1 exprs)
                      nb-args
                      var-cont))
    (let ((var (##field0 var-cont)))
      (let ((cont (##field1 var-cont)))
        (let ((v (eval@lookup var cte 0)))
          ;; should be unecessary because there shouldn't be any primitive called this way
          ;; (display v)
          ;; (newline)
          ;; (if-feature
          ;;   prim-no-arity
          ;;   (if (##rib? (##field0 (##field0 var)))
          ;;     (eval@add-nb-args
          ;;       nb-args
          ;;       (eval@gen-call (if (integer? v) (##+ 1 v) v) cont))
          ;;     (eval@gen-call (if (integer? v) (##+ 1 v) v) cont))
            (eval@add-nb-args
              nb-args
              (eval@gen-call (if (integer? v) (##+ 1 v) v) cont)))))))

(define (eval@lookup var cte i)
  (if (pair? cte)
      (if (##eqv? (##field0 cte) var)
          i
          (eval@lookup var (##field1 cte) (##+ i 1)))
      var))

(define (eval@extend vars cte)
  (if (pair? vars)
      (cons (##field0 vars) (eval@extend (##field1 vars) cte))
      cte))

(define eval@tail
  (if-feature
    prim-no-arity
    (##rib eval@jump/call-op '##id 0)
    (eval@add-nb-args 1 (##rib eval@jump/call-op '##id 0)))) ;; jump

(define (eval@make-procedure code env) (##rib code env eval@procedure-type))

(define (eval expr)
  ((eval@make-procedure (##rib 0 0 (eval@comp '() expr eval@tail)) '())))

(define (repl)
  (if-feature
    (not quiet)
    (if-feature
      frog-talk
      (display "\n @...@\n(-----) > ")
      (display "> ")))
  (let ((expr (read)))
    (if (eof-object? expr)
      (newline)
      (begin
        (write (eval expr))
        (newline)
        (repl)))))


;; ---------------------- LOAD ---------------------- ;;

(define (load filename)
  (let ((port (open-input-file filename)))
    (let loop ((expr (read port)))
      (if (eof-object? expr)
        (begin
          (close-input-port port)
          '())
        (begin
          (eval expr)
          (loop (read port)))))))

