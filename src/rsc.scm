#!/usr/bin/env gsi

;;!#;; satisfy guile

;;; Ribbit Scheme compiler.

;;;----------------------------------------------------------------------------

;; Compatibility layer.

;; Tested with Gambit v4.7.5 and above, Guile 3.0.7, Chicken 5.2.0 and Kawa 3.1

(cond-expand

 ((and chicken compiling)

  (declare
   (block)
   (fixnum-arithmetic)
   (usual-integrations)))

 (else))

(cond-expand

  (gambit

   (define (shell-cmd command)
     (shell-command command))

   (define (del-file path)
     (delete-file path)))

  (guile

   (define (shell-cmd command)
     (system command))

   (define (del-file path)
     (delete-file path)))

  (chicken

   (import (chicken process) (chicken file))

   (define (shell-cmd command)
     (system command))

   (define (del-file path)
     (delete-file path)))

  (kawa

   (define (shell-cmd command)
     (system command))

   (define (del-file path)
     (delete-file path)))

  (else

   (define (shell-cmd command)
     #f)

   (define (del-file path)
     #f)))

(cond-expand

  ((or gambit
       guile
       chicken
       kawa)

   (define (pipe-through program output)
     (let ((tmpin  "rsc.tmpin")
           (tmpout "rsc.tmpout"))
       (call-with-output-file
           tmpin
         (lambda (port) (display output port)))
       (shell-cmd (string-append
                   program
                   (string-append
                    " < "
                    (string-append
                     tmpin
                     (string-append " > " tmpout)))))
       (let ((out
              (call-with-input-file
                  tmpout
                (lambda (port) (read-line port #f)))))
         (del-file tmpin)
         (del-file tmpout)
         out))))

  (else

   (define (pipe-through program output)
     (display "*** Minification is not supported with this Scheme system\n")
     (display "*** so the generated code was not minified.\n")
     (display "*** You might want to try running ")
     (display program)
     (display " manually.\n")
     output)))

(cond-expand

  (ribbit

   (define (cmd-line)
     (cons "" '())))

  (chicken

   (import (chicken process-context))

   (define (cmd-line)
     (cons (program-name) (command-line-arguments))))

  (else

   (define (cmd-line)
     (command-line))))

(cond-expand

  (else

   ;; It seems "exit" is pretty universal but we put it in a
   ;; cond-expand in case some Scheme implementation does it
   ;; differently.

   (define (exit-program-normally)
     (exit 0))

   (define (exit-program-abnormally)
     (exit 1))))

(cond-expand

  (gambit

   (define (with-output-to-str thunk)
     (with-output-to-string "" thunk)))

  (chicken

   (import (chicken port))

   (define (with-output-to-str thunk)
     (with-output-to-string thunk)))

  (kawa

   (define (with-output-to-str thunk)
     (call-with-output-string
      (lambda (port)
        (parameterize ((current-output-port port))
                      (thunk))))))

  (else

   (define (with-output-to-str thunk)
     (with-output-to-string thunk))))

(cond-expand

 (gambit (begin))

 (kawa

  (import (rnrs hashtables))

  (define (make-table)
    (make-hashtable symbol-hash symbol=?))

  (define (table-ref table key default)
    (hashtable-ref table key default))

  (define (table-set! table key value)
    (hashtable-set! table key value))

  (define (table-length table)
    (hashtable-size table))

  (define (table->list table)
    (let-values (((keys entries) (hashtable-entries table)))
      (vector->list (vector-map cons keys entries)))))

 (else

     (define (make-table)
       (cons '() '()))

   (define (table-ref table key default)
     (let ((x (assoc key (car table))))
       (if x
           (cdr x)
           default)))

   (define (table-set! table key value)
     (let ((x (assoc key (car table))))
       (if x
           (set-cdr! x value)
           (set-car! table
                     (cons (cons key value) (car table))))))

   (define (table-length table)
     (length (car table)))

   (define (table->list table)
     (car table))))

(cond-expand

  ((or gambit chicken)

   (define (symbol->str symbol)
     (symbol->string symbol))

   (define (str->uninterned-symbol string)
     (string->uninterned-symbol string)))

  (kawa

   (define (symbol->str symbol)
     (symbol->string symbol))

   (define (str->uninterned-symbol string)
     (symbol string #f)))

  (else

   (define uninterned-symbols (make-table))

   (define (str->uninterned-symbol string)
     (let* ((name
             (string-append "@@@" ;; use a "unique" prefix
                            (number->string
                             (table-length uninterned-symbols))))
            (sym
             (string->symbol name)))
       (table-set! uninterned-symbols sym string) ;; remember "real" name
       sym))

   (define (symbol->str symbol)
     (table-ref uninterned-symbols symbol (symbol->string symbol)))))

(cond-expand

 (gambit

  (define (rsc-path-extension path)
    (path-extension path))

  (define (rsc-path-directory path)
    (path-directory path)))

 (chicken

  (import (chicken pathname))

  (define (rsc-path-extension path)
    (let ((ext (pathname-extension path)))
      (if ext (string-append "." ext) "")))

  (define (rsc-path-directory path)
    (let ((dir (pathname-directory path)))
      (if dir dir "")))

  (define (path-expand path dir)
    (make-pathname dir path)))

 (kawa

  (define (rsc-path-extension path)
    (let ((ext (path-extension path)))
      (if ext (string-append "." ext) "")))

  (define (rsc-path-directory path)
    (path-directory path))

  (define (path-expand path::string dir::string)
    (if (= (string-length dir) 0)
        path
        (let ((p (java.nio.file.Path:of dir path)))
          (p:toString)))))

 (else

   (define (rsc-path-extension path)
     (let loop ((i (- (string-length path) 1)))
       (if (< i 0)
           ""
           (if (= (char->integer (string-ref path i)) 46) ;; #\.
               (substring path i (string-length path))
               (loop (- i 1))))))

   (define (rsc-path-directory path)
     (let loop ((i (- (string-length path) 1)))
       (if (< i 0)
           "./"
           (if (= (char->integer (string-ref path i)) 47) ;; #\/
               (substring path 0 (+ i 1))
               (loop (- i 1))))))

   (define (path-expand path dir)
     (if (= (string-length dir) 0)
         path
         (if (= (char->integer (string-ref dir (- (string-length dir) 1))) 47) ;; #\/
             (string-append dir path)
             (string-append dir (string-append "/" path)))))))

(cond-expand

 (gambit (begin))

 (else

   (define (read-line port sep)
     (let loop ((rev-chars '()))
       (let ((c (read-char port)))
         (if (or (eof-object? c) (eqv? c sep))
             (list->string (reverse rev-chars))
             (loop (cons c rev-chars))))))

   (define (pp obj)
     (write obj)
     (newline))))

(cond-expand

  ((and gambit (or enable-bignum disable-bignum))) ;; recent Gambit?

  (chicken

   (import (chicken sort))

   (define (list-sort! compare list)
     (sort! list compare))

   (define (list-sort compare list)
     (sort list compare)))

  (else

   (define (list-sort! compare list)

     ;; Stable mergesort algorithm

     (define (sort list len)
       (if (= len 1)
           (begin
             (set-cdr! list '())
             list)
           (let ((len1 (quotient len 2)))
             (let loop ((n len1) (tail list))
               (if (> n 0)
                   (loop (- n 1) (cdr tail))
                   (let ((x (sort tail (- len len1))))
                     (merge (sort list len1) x)))))))

     (define (merge list1 list2)
       (if (pair? list1)
           (if (pair? list2)
               (let ((x1 (car list1))
                     (x2 (car list2)))
                 (if (compare x2 x1)
                     (merge-loop list2 list2 list1 (cdr list2))
                     (merge-loop list1 list1 (cdr list1) list2)))
               list1)
           list2))

     (define (merge-loop result prev list1 list2)
       (if (pair? list1)
           (if (pair? list2)
               (let ((x1 (car list1))
                     (x2 (car list2)))
                 (if (compare x2 x1)
                     (begin
                       (set-cdr! prev list2)
                       (merge-loop result list2 list1 (cdr list2)))
                     (begin
                       (set-cdr! prev list1)
                       (merge-loop result list1 (cdr list1) list2))))
               (begin
                 (set-cdr! prev list1)
                 result))
           (begin
             (set-cdr! prev list2)
             result)))

     (let ((len (length list)))
       (if (= 0 len)
           '()
           (sort list len))))

   (define (list-sort compare list)
     (list-sort! compare (append list '())))))

(cond-expand

 (gambit (begin))

 (chicken

  (define (script-file)
    (program-name))

  (define (executable-path)
    (executable-pathname)))

 (else
   (define (script-file)
     (car (cmd-line)))

   (define (executable-path)
     "")))

(cond-expand

  ((and gambit ;; hack to detect recent Gambit version
        (or enable-sharp-dot disable-sharp-dot)))

  (chicken

   (import (chicken string))

   (define (string-concatenate string-list separator)
     (string-intersperse string-list separator)))

  (kawa

   (define (string-concatenate string-list separator)
     (string-join string-list separator)))

  (else

   (define (string-concatenate string-list separator)
     (if (pair? string-list)
         (let ((rev-string-list (reverse string-list))
               (sep (string->list separator)))
           (let loop ((lst (cdr rev-string-list))
                      (result (string->list (car rev-string-list))))
             (if (pair? lst)
                 (loop (cdr lst)
                       (append (string->list (car lst))
                               (append sep
                                       result)))
                 (list->string result))))
         ""))))

;;;----------------------------------------------------------------------------

(define predefined '(rib false true nil)) ;; predefined symbols

(define primitives '(
(rib         0) ;; predefined by RVM (must be first and 0)
(id          1)
(arg1        2)
(arg2        3)
(close       4)
(rib?        5)
(field0      6)
(field1      7)
(field2      8)
(field0-set! 9)
(field1-set! 10)
(field2-set! 11)
(eqv?        12)
(<           13)
(+           14)
(-           15)
(*           16)
(quotient    17)
(getchar     18)
(putchar     19)
(exit        20)
))

(define jump/call-op 'jump/call)
(define set-op       'set)
(define get-op       'get)
(define const-op     'const)
(define if-op        'if)

;;;----------------------------------------------------------------------------

(cond-expand

  (ribbit

   (define procedure2? procedure?))

  (else

   (define pair-type      0)
   (define procedure-type 1)
   (define symbol-type    2)
   (define string-type    3)
   (define vector-type    4)
   (define singleton-type 5)

   (define (instance? o type) (and (rib? o) (eqv? (field2 o) type)))

   (define (rib field0 field1 field2)
     (let ((r (make-vector 3)))
       (vector-set! r 0 field0)
       (vector-set! r 1 field1)
       (vector-set! r 2 field2)
       r))

   (define (rib? o) (vector? o))
   (define (field0 o) (vector-ref o 0))
   (define (field1 o) (vector-ref o 1))
   (define (field2 o) (vector-ref o 2))
   (define (field0-set! o x) (vector-set! o 0 x) o)
   (define (field1-set! o x) (vector-set! o 1 x) o)
   (define (field2-set! o x) (vector-set! o 2 x) o)

   (define (procedure2? o) (instance? o procedure-type))
   (define (make-procedure code env) (rib code env procedure-type))
   (define (procedure-code proc) (field0 proc))
   (define (procedure-env proc) (field1 proc))))

(define (oper pc) (field0 pc))
(define (opnd pc) (field1 pc))
(define (next pc) (field2 pc))

;;;----------------------------------------------------------------------------

;; The compiler from Ribbit Scheme to RVM code.

(define (make-ctx cte live exports) (rib cte (cons live '()) exports))

(define (ctx-cte ctx) (field0 ctx))
(define (ctx-live ctx) (car (field1 ctx)))
(define (ctx-exports ctx) (field2 ctx))

(define (ctx-cte-set ctx x)
  (rib x (field1 ctx) (field2 ctx)))

(define (ctx-live-set! ctx x)
  (set-car! (field1 ctx) x))

(define (comp ctx expr cont)

  (cond ((symbol? expr)
         (let ((v (lookup expr (ctx-cte ctx) 0)))
           (if (eqv? v expr) ;; global?
               (let ((g (live? expr (ctx-live ctx))))
                 (if (and g (constant? g)) ;; constant propagated?
                     (rib const-op (cadr (cadr g)) cont)
                     (rib get-op v cont)))
               (rib get-op v cont))))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (rib const-op (cadr expr) cont))

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (let ((val (caddr expr)))
                      (let ((v (lookup var (ctx-cte ctx) 1)))
                        (if (eqv? v var) ;; global?
                            (let ((g (live? var (ctx-live ctx))))
                              (if g
                                  (if (and (constant? g)
                                           (not (assoc var (ctx-exports ctx))))
                                      (begin
;;                                        (pp `(*** constant propagation of ,var = ,(cadr g))
;;                                             (current-error-port))
                                        (gen-noop cont))
                                      (comp ctx val (gen-assign v cont)))
                                  (begin
;;                                    (pp `(*** removed dead assignment to ,var)
;;                                         (current-error-port))
                                    (gen-noop cont))))
                            (comp ctx val (gen-assign v cont)))))))

                 ((eqv? first 'if)
                  (let ((cont-false (comp ctx (cadddr expr) cont)))
                    (let ((cont-true (comp ctx (caddr expr) cont)))
                      (let ((cont-test (rib if-op cont-true cont-false)))
                        (comp ctx (cadr expr) cont-test)))))

                 ((eqv? first 'lambda)
                  (let ((params (cadr expr)))
                    (rib const-op
                         (make-procedure
                          (rib (length params)
                               0
                               (comp-begin (ctx-cte-set
                                            ctx
                                            (extend params
                                                    (cons #f
                                                          (cons #f
                                                                (ctx-cte ctx)))))
                                           (cddr expr)
                                           tail))
                          '())
                         (if (null? (ctx-cte ctx))
                             cont
                             (gen-call (use-symbol ctx 'close) cont)))))

                 ((eqv? first 'begin)
                  (comp-begin ctx (cdr expr) cont))

                 ((eqv? first 'let)
                  (let ((bindings (cadr expr)))
                    (let ((body (cddr expr)))
                      (comp-bind ctx
                                 (map car bindings)
                                 (map cadr bindings)
                                 body
                                 cont))))

                 (else
                  (let ((args (cdr expr)))
                    (if (symbol? first)
                        (comp-call ctx
                                   args
                                   (lambda (ctx)
                                     (let ((v (lookup first (ctx-cte ctx) 0)))
                                       (gen-call v cont))))
                        (comp-bind ctx
                                   '(_)
                                   (cons first '())
                                   (cons (cons '_ args) '())
                                   cont)))))))

        (else
         ;; self-evaluating
         (rib const-op expr cont))))

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

(define (comp-bind ctx vars exprs body cont)
  (comp-bind* ctx vars exprs ctx body cont))

(define (comp-bind* ctx vars exprs body-ctx body cont)
  (if (pair? vars)
      (let ((var (car vars))
            (expr (car exprs)))
        (comp ctx
              expr
              (comp-bind* (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
                          (cdr vars)
                          (cdr exprs)
                          (ctx-cte-set body-ctx (cons var (ctx-cte body-ctx)))
                          body
                          (gen-unbind ctx cont))))
      (comp-begin body-ctx
                  body
                  cont)))

(define (gen-unbind ctx cont)
  (if (eqv? cont tail)
      cont
      (rib jump/call-op ;; call
           (use-symbol ctx 'arg2)
           cont)))

(define (use-symbol ctx sym)
  (ctx-live-set! ctx (add-live sym (ctx-live ctx)))
  sym)

(define (comp-begin ctx exprs cont)
  (comp ctx
        (car exprs)
        (if (pair? (cdr exprs))
            (rib jump/call-op ;; call
                 (use-symbol ctx 'arg1)
                 (comp-begin ctx (cdr exprs) cont))
            cont)))

(define (comp-call ctx exprs k)
  (if (pair? exprs)
      (comp ctx
            (car exprs)
            (comp-call (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
                       (cdr exprs)
                       k))
      (k ctx)))

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

(define tail (rib jump/call-op 'id 0)) ;; jump

;;;----------------------------------------------------------------------------

(define (extract-exports program)
  ;; By default all symbols are exported when the program contains
  ;; no (export ...) form.
  (let loop ((lst program) (rev-exprs '()) (exports #f))
    (if (pair? lst)
        (let ((first (car lst)))
          (if (and (pair? first) (eqv? (car first) 'export))
              (loop (cdr lst)
                    rev-exprs
                    (append (cdr first) (or exports '())))
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

(define (comp-exprs-with-exports exprs exports)
  (let* ((expansion
          (expand-begin exprs))
         (live
          (liveness-analysis expansion exports))
         (exports
          (or exports
              (map (lambda (v)
                     (let ((var (car v)))
                       (cons var var)))
                   live))))
    (cons
     (make-procedure
      (rib 0 ;; 0 parameters
           0
           (comp (make-ctx '() live exports)
                 expansion
                 tail))
      '())
     exports)))

(define (compile-program verbosity program)
  (let* ((exprs-and-exports
          (extract-exports program))
         (exprs
          (car exprs-and-exports))
         (exports
          (cdr exprs-and-exports))
         (proc-and-exports
          (comp-exprs-with-exports
           (if (pair? exprs) exprs (cons #f '()))
           (exports->alist exports))))
    (if (>= verbosity 2)
        (begin
          (display "*** RVM code:\n")
          (pp (car proc-and-exports))))
    (if (>= verbosity 3)
        (begin
          (display "*** exports:\n")
          (pp (cdr proc-and-exports))))
    proc-and-exports))

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
                                 (append (map (lambda (binding)
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
                             (cons (cons '_
                                         (cons (cadr expr)
                                               '()))
                                   '())
                             (cons
                              (cons 'if
                                    (cons '_
                                          (cons '_
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

(define (expand-constant x)
  (cons 'quote (cons x '())))

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
        (expand-body-done defs '(0)))))

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
    (if (assoc 'symtbl live)
        (liveness-analysis-aux expr exports)
        live)))

(define (liveness-analysis-aux expr exports)
  (let loop ((live-globals
              (add-live 'arg1 ;; TODO: these should not be forced live...
                        (add-live 'arg2
                                  (add-live 'close
                                            (add-live 'id
                                                      (exports->live
                                                       (or exports '()))))))))
    (reset-defs live-globals)
    (let ((x (liveness expr live-globals (not exports))))
      (if (eqv? x live-globals)
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
        #f)))

(define (add-live var live-globals)
  (if (live? var live-globals)
      live-globals
      (let ((g (cons var '())))
        (cons g live-globals))))

(define (live? var lst)
  (if (pair? lst)
      (let ((x (car lst)))
        (if (eqv? var (car x))
            x
            (live? var (cdr lst))))
      #f))

(define (constant? g)
  (and (pair? (cdr g))
       (null? (cddr g))
       (pair? (cadr g))
       (eqv? 'quote (car (cadr g)))))

(define (in? var cte)
  (not (eqv? var (lookup var cte 0))))

(define (liveness expr live-globals export-all?)

  (define (add var)
    (set! live-globals (add-live var live-globals)))

  (define (add-val val)
    (cond ((symbol? val)
           (add val))
          ((pair? val)
           (add-val (car val))
           (add-val (cdr val)))
          ((vector? val)
           (for-each add-val (vector->list val)))))

  (define (liveness expr cte top?)

    (cond ((symbol? expr)
           (if (in? expr cte) ;; local var?
               #f
               (add expr))) ;; mark the global variable as "live"

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eqv? first 'quote)
                    (let ((val (cadr expr)))
                      (add-val val)))

                   ((eqv? first 'set!)
                    (let ((var (cadr expr)))
                      (let ((val (caddr expr)))
                        (if (in? var cte) ;; local var?
                            (liveness val cte #f)
                            (begin
                              (if export-all? (add var))
                              (let ((g (live? var live-globals))) ;; variable live?
                                (if g
                                    (begin
                                      (set-cdr! g (cons val (cdr g)))
                                      (liveness val cte #f))
                                    #f)))))))

                   ((eqv? first 'if)
                    (liveness (cadr expr) cte #f)
                    (liveness (caddr expr) cte #f)
                    (liveness (cadddr expr) cte #f))

                   ((eqv? first 'let)
                    (let ((bindings (cadr expr)))
                      (liveness-list (map cadr bindings) cte)
                      (liveness (caddr expr) (append (map car bindings) cte) #f)))

                   ((eqv? first 'begin)
                    (liveness-list (cdr expr) cte))

                   ((eqv? first 'lambda)
                    (let ((params (cadr expr)))
                      (liveness (caddr expr) (extend params cte) #f)))

                   (else
                    (liveness-list expr cte)))))

          (else
           #f)))

  (define (liveness-list exprs cte)
    (if (pair? exprs)
        (begin
          (liveness (car exprs) cte #f)
          (liveness-list (cdr exprs) cte))
        #f))

  (liveness expr '() #t)

  live-globals)

;;;----------------------------------------------------------------------------

;; RVM code encoding.

(define eb 92) ;; encoding base (strings have 92 characters that are not escaped and not space)
;;(define eb 256)
(define eb/2 (quotient eb 2))

(define get-int-short    10) ;; 0 <= N <= 9  are encoded with 1 byte
(define const-int-short  11) ;; 0 <= N <= 10 are encoded with 1 byte
(define const-proc-short  4) ;; 0 <= N <= 3  are encoded with 1 byte
(define jump-sym-short   20) ;; 0 <= N <= 19 are encoded with 1 byte

(define call-sym-short   (- eb ;; use rest to encode calls to globals
                            (+ const-int-short
                               (+ const-proc-short
                                  (+ get-int-short
                                     (+ jump-sym-short
                                        17))))))

(define jump-start       0)
(define jump-int-start   (+ jump-start jump-sym-short))
(define jump-sym-start   (+ jump-int-start 1))
(define call-start       (+ jump-sym-start 2))
(define call-int-start   (+ call-start call-sym-short))
(define call-sym-start   (+ call-int-start 1))
(define set-start        (+ call-sym-start 2))
(define set-int-start    (+ set-start 0))
(define set-sym-start    (+ set-int-start 1))
(define get-start        (+ set-sym-start 2))
(define get-int-start    (+ get-start get-int-short))
(define get-sym-start    (+ get-int-start 1))
(define const-start      (+ get-sym-start 2))
(define const-int-start  (+ const-start const-int-short))
(define const-sym-start  (+ const-int-start 1))
(define const-proc-start (+ const-sym-start 2))
(define if-start         (+ const-proc-start (+ const-proc-short 1)))

(define (encode proc exports)

  (define syms (make-table))

  (define built-constants '())

  (define (build-constant o tail)
    (cond ((or (memv o '(#f #t ()))
               (assq o built-constants))
           (let ((v (constant-global-var o)))
             (rib get-op
                  (scan-opnd v 1)
                  tail)))
          ((symbol? o)
           (rib const-op
                (scan-opnd o 2)
                tail))
          ((number? o)
           (if (< o 0)
               (rib const-op
                    0
                    (rib const-op
                         (- 0 o)
                         (rib jump/call-op
                              (scan-opnd '- 0)
                              tail)))
               (rib const-op
                    o
                    tail)))
          ((pair? o)
           (build-constant (car o)
                           (build-constant (cdr o)
                                           (rib const-op
                                                pair-type
                                                (rib jump/call-op
                                                     (scan-opnd 'rib 0)
                                                     tail)))))
          ((string? o)
           (let ((chars (map char->integer (string->list o))))
             (build-constant chars
                             (build-constant (length chars)
                                             (rib const-op
                                                  string-type
                                                  (rib jump/call-op
                                                       (scan-opnd 'rib 0)
                                                       tail))))))
          ((vector? o)
           (let ((elems (vector->list o)))
             (build-constant elems
                             (build-constant (length elems)
                                             (rib const-op
                                                  vector-type
                                                  (rib jump/call-op
                                                       (scan-opnd 'rib 0)
                                                       tail))))))
          (else
           (error "can't build constant" o))))

  (define (build-constant-in-global-var o v)
    (let ((code (build-constant o 0)))
      (set! built-constants (cons (cons o (cons v code)) built-constants))
      v))

  (define (add-init-primitives tail)

    (define (prim-code sym tail)
      (rib const-op
           (cadr (assq sym primitives)) ;; get index
           (rib const-op
                0
                (rib const-op
                     procedure-type
                     (rib jump/call-op
                          (scan-opnd 'rib 0)
                          (rib set-op
                               (scan-opnd sym 3)
                               tail))))))

    (let loop ((lst (cdr primitives)) ;; skip rib primitive that is predefined
               (tail tail))
      (if (pair? lst)
          (loop (cdr lst)
                (let* ((sym (car (car lst)))
                       (descr (table-ref syms sym #f)))
                  (if (and descr
                           (or (< 0 (field0 descr))
                               (< 0 (field1 descr))
                               (< 0 (field2 descr))))
                      (prim-code sym tail)
                      tail)))
          tail)))

  (define (append-code code tail)
    (if (eqv? code 0)
        tail
        (rib (field0 code) (field1 code) (append-code (field2 code) tail))))

  (define (add-init-constants tail)
    (let loop ((lst built-constants) (tail tail))
      (if (pair? lst)
          (let* ((x (car lst))
                 (o (car x))
                 (v (cadr x))
                 (code (cddr x)))
            (loop (cdr lst)
                  (append-code code (rib set-op v tail))))
          tail)))

  (define (add-init-code! proc)
    (let ((code (field0 proc)))
      (field2-set! code
                   (add-init-primitives
                    (add-init-constants
                     (field2 code))))))

  (define constant-counter 0)

  (define (constant-global-var o)
    (cond ((eqv? o #f)
           'false)
          ((eqv? o #t)
           'true)
          ((eqv? o '())
           'nil)
          (else
           (let ((x (assq o built-constants)))
             (if x
                 (cadr x)
                 (let ((v (string->symbol
                           (string-append "_"
                                          (number->string constant-counter)))))
                   (set! constant-counter (+ constant-counter 1))
                   (build-constant-in-global-var o v)
                   (scan-opnd v 3)
                   v))))))

  (define (use-in-call sym)
    (scan-opnd sym 0)
    sym)

  (define (scan-proc proc)
    (scan (next (procedure-code proc))))

  (define (scan-opnd o pos)
    (scan-opnd-aux o pos)
    o)

  (define (scan-opnd-aux o pos)
    (cond ((symbol? o)
           (let ((descr
                  (or (table-ref syms o #f)
                      (let ((descr (rib 0 0 0)))
                        (table-set! syms o descr)
                        descr))))
             (cond ((= pos 0)
                    (field0-set! descr (+ 1 (field0 descr))))
                   ((= pos 1)
                    (field1-set! descr (+ 1 (field1 descr))))
                   ((= pos 2)
                    (field2-set! descr (+ 1 (field2 descr)))))))
          ((procedure2? o)
           (scan-proc o))))

  (define (scan code)
    (if (rib? code)
        (begin
          (scan-instr code)
          (scan (next code)))))

  (define (scan-instr code)
    (let ((op (oper code))
          (o (opnd code)))
      (cond ((eqv? op if-op)
             (scan o))
            ((eqv? op jump/call-op)
             (scan-opnd o 0)) ;; 0 = jump/call
            ((eqv? op get-op)
             (scan-opnd o 1)) ;; 1 = get
            ((eqv? op const-op)
             (if (or (symbol? o)
                     (procedure2? o)
                     (and (number? o) (>= o 0)))
                 (scan-opnd o 2) ;; 2 = const
                 (let ((v (constant-global-var o)))
                   (field0-set! code get-op)
                   (field1-set! code v)
                   (scan-opnd v 1)))) ;; 1 = get
            ((eqv? op set-op)
             (scan-opnd o 3))))) ;; 3 = set

  (define (encode-sym o)
    (let ((descr (table-ref syms o #f)))
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
        (let ((t (cons (if (eqv? stream end) r (+ r eb/2)) stream)))
          (if (= q 0)
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

  (define (number? x) (integer? x))

  (define (enc code stream)
    (if (rib? code)
        (let ((op (oper code)))
          (cond ((eqv? op jump/call-op)
                 (if (eqv? 0 (next code)) ;; jump?

                     (let ((o (opnd code)))
                       (cond ((number? o)
                              (encode-long1 jump-int-start
                                            o
                                            stream))
                             ((symbol? o)
                              (let ((x (encode-sym o)))
                                (if (< x jump-sym-short)
                                    (cons (+ jump-start x)
                                          stream)
                                    (encode-long2 jump-sym-start
                                                  x
                                                  stream))))
                             (else
                              (error "can't encode jump" o))))

                     (enc (next code)
                          (let ((o (opnd code)))
                            (cond ((number? o)
                                   (encode-long1 call-int-start
                                                 o
                                                 stream))
                                  ((symbol? o)
                                   (let ((x (encode-sym o)))
                                     (if (< x call-sym-short)
                                         (cons (+ call-start x)
                                               stream)
                                         (encode-long2 call-sym-start
                                                       x
                                                       stream))))
                                  (else
                                   (error "can't encode call" o)))))))

                ((eqv? op set-op)
                 (enc (next code)
                      (let ((o (opnd code)))
                        (cond ((number? o)
                               (encode-long1 set-int-start
                                             o
                                             stream))
                              ((symbol? o)
                               (encode-long2 set-sym-start
                                             (encode-sym o)
                                             stream))
                              (else
                               (error "can't encode set" o))))))

                ((eqv? op get-op)
                 (enc (next code)
                      (let ((o (opnd code)))
                        (cond ((number? o)
                               (if (< o get-int-short)
                                   (cons (+ get-start o)
                                         stream)
                                   (encode-long1 get-int-start
                                                 o
                                                 stream)))
                              ((symbol? o)
                               (encode-long2 get-sym-start
                                             (encode-sym o)
                                             stream))
                              (else
                               (error "can't encode get" o))))))

                ((eqv? op const-op)
                 (enc (next code)
                      (let ((o (opnd code)))
                        (cond ((number? o)
                               (if (< o const-int-short)
                                   (cons (+ const-start o)
                                         stream)
                                   (encode-long1 const-int-start
                                                 o
                                                 stream)))
                              ((symbol? o)
                               (encode-long2 const-sym-start
                                             (encode-sym o)
                                             stream))
                              ((procedure2? o)
                               (enc-proc o stream))
                              (else
                               (error "can't encode const" o))))))

                ((eqv? op if-op)
                 (enc (next code)
                      (enc (opnd code)
                           (cons if-start
                                 stream))))

                (else
                 (error "unknown op" op))))
        (error "rib expected" '())))

  (define (ordering sym-descr)
    (let ((sym (car sym-descr)))
      (let ((pos (member sym predefined)))
        (if pos
            (+ 9999999 (length pos))
            (let ((descr (cdr sym-descr)))
              (field0 descr))))))

  (for-each (lambda (sym) (scan-opnd sym 3)) predefined)

  (scan-proc proc)

  (add-init-code! proc)

  (let ((lst
         (list-sort
          (lambda (a b)
            (< (ordering b) (ordering a)))
          (table->list syms))))

    (let loop1 ((i 0) (lst lst) (symbols '()))
      (if (and (pair? lst) (< i call-sym-short))
          (let ((s (car lst)))
            (let ((sym (car s)))
              (let ((descr (cdr s)))
                (let ((x (assq sym exports)))
                  (let ((symbol (if x (cdr x) (str->uninterned-symbol ""))))
                    (field0-set! descr i)
                    (loop1 (+ i 1) (cdr lst) (cons symbol symbols)))))))
          (let loop2 ((i i) (lst2 lst) (symbols symbols))
            (if (pair? lst2)
                (let ((s (car lst2)))
                  (let ((sym (car s)))
                    (let ((x (assq sym exports)))
                      (if x
                          (let ((symbol (cdr x)))
                            (let ((descr (cdr s)))
                              (field0-set! descr i)
                              (loop2 (+ i 1) (cdr lst2) (cons symbol symbols))))
                          (loop2 i (cdr lst2) symbols)))))
                (let loop3 ((i i) (lst3 lst) (symbols symbols))
                  (if (pair? lst3)
                      (let ((s (car lst3)))
                        (let ((sym (car s)))
                          (let ((x (assq sym exports)))
                            (if x
                                (loop3 i (cdr lst3) symbols)
                                (let ((symbol (str->uninterned-symbol "")))
                                  (let ((descr (cdr s)))
                                    (field0-set! descr i)
                                    (loop3 (+ i 1) (cdr lst3) (cons symbol symbols))))))))
                      (let loop4 ((symbols* symbols))
                        (if (and (pair? symbols*)
                                 (string=? (symbol->str (car symbols*)) ""))
                            (loop4 (cdr symbols*))

                            (let ((stream
                                   (enc-proc proc '())))
                              (string-append
                               (stream->string
                                (encode-n (- (length symbols)
                                             (length symbols*))
                                          '()))
                               (string-append
                                (string-concatenate
                                 (map (lambda (s)
                                        (let ((str (symbol->str s)))
                                          (list->string
                                           (reverse (string->list str)))))
                                      symbols*)
                                 ",")
                                (string-append
                                 ";"
                                 (stream->string stream)))))))))))))))

(define (stream->string stream)
  (list->string
   (map (lambda (n)
          (let ((c (+ n 35)))
            (integer->char (if (= c 92) 33 c))))
        stream)))

(define (string->codes string)
  (map char->integer (string->list string)))

;;;----------------------------------------------------------------------------

;; Source code reading.

(define (root-dir)
  (rsc-path-directory (or (script-file) (executable-path))))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
        '()
        (cons x (read-all)))))

(define (read-from-file path)
  (with-input-from-file path read-all))

(define (read-library lib-path)
  (read-from-file
   (if (equal? (rsc-path-extension lib-path) "")
       (path-expand (string-append lib-path ".scm")
                    (path-expand "lib" (root-dir)))
       lib-path)))

(define (read-program lib-path src-path)
  (append (read-library lib-path)
          (if (equal? src-path "-")
              (read-all)
              (read-from-file src-path))))

;;;----------------------------------------------------------------------------

;; Target code generation.

(define (string-from-file path)
  (call-with-input-file path (lambda (port) (read-line port #f))))

(define (generate-code target verbosity input-path minify? proc-and-exports)
  (let* ((proc
          (car proc-and-exports))
         (exports
          (cdr proc-and-exports))
         (encoded-program
          (encode proc exports))
         (vm-source
          (if (equal? target "rvm")
              ""
              (string-from-file
               (path-expand (string-append
                             "host/"
                             (string-append
                              target
                              (string-append "/rvm." target)))
                            (root-dir)))))
         (input
          (string-append encoded-program
                         (if input-path
                             (string-from-file input-path)
                             ""))))

    (if (>= verbosity 1)
        (begin
          (display "*** RVM code length: ")
          (display (string-length input))
          (display " bytes\n")))

    (let* ((target-code-before-minification
            (if (equal? target "rvm")
                input
                (let ((sample
                       ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y")) ;; RVM code that prints HELLO!
                  (string-replace
                   (string-replace
                    (string-replace
                     (string-replace
                      vm-source
                      sample
                      input)
                     (rvm-code-to-bytes sample " ")
                     (rvm-code-to-bytes input " "))
                    (rvm-code-to-bytes sample ",")
                    (rvm-code-to-bytes input ","))
                   "RVM code that prints HELLO!"
                   "RVM code of the program"))))
           (target-code
            (if (or (not minify?) (equal? target "rvm"))
                target-code-before-minification
                (pipe-through
                 (path-expand
                  (string-append
                   "host/"
                   (string-append target "/minify"))
                  (root-dir))
                 target-code-before-minification))))
      target-code)))

(define (rvm-code-to-bytes rvm-code sep)
  (string-concatenate
   (map (lambda (c) (number->string (char->integer c)))
        (string->list rvm-code))
   sep))

(define (string-replace str pattern replacement)
  (let ((len-pattern (string-length pattern))
        (len-replacement (string-length replacement)))
    (let loop1 ((i 0) (j 0) (out '()))
      (if (<= (+ j len-pattern) (string-length str))
          (let loop2 ((k (- len-pattern 1)))
            (if (< k 0)
                (let ((end (+ j len-pattern)))
                  (loop1 end
                         end
                         (cons replacement (cons (substring str i j) out))))
                (if (char=? (string-ref str (+ j k)) (string-ref pattern k))
                    (loop2 (- k 1))
                    (loop1 i
                           (+ j 1)
                           out))))
          (string-concatenate
           (reverse (cons (substring str i (string-length str)) out))
           "")))))

(define (write-target-code output-path target-code)
  (if (equal? output-path "-")
      (display target-code)
      (with-output-to-file
          output-path
        (lambda ()
          (display target-code)))))

;;;----------------------------------------------------------------------------

;; Compiler entry points.

(define (pipeline-compiler)

  ;; This version of the compiler reads the source code on stdin and
  ;; outputs the compacted RVM code on stdout.  The program source
  ;; code must be prefixed by the runtime library's source code.
  ;;
  ;; A Scheme file can be combined with the library and compiled to
  ;; RVM code with this command:
  ;;
  ;;   $ echo '(display "hello!\n")' > hello.scm
  ;;   $ cat lib/max.scm hello.scm | gsi rsc.scm > code.rvm
  ;;
  ;; Alternatively, the rsc shell script can be used to automate
  ;; the creation of a complete executable target program:
  ;;
  ;;   $ ./rsc -t py -l max hello.scm
  ;;   $ python3 hello.scm.py
  ;;   hello!

  (display
   (generate-code
    "rvm"  ;; target
    0      ;; verbosity
    #f     ;; input-path
    #f     ;; minify?
    (compile-program
     0 ;; verbosity
     (read-all)))))

(cond-expand

  (ribbit  ;; Ribbit does not have access to the command line...

   (pipeline-compiler))

  (else

   (define (fancy-compiler src-path
                           output-path
                           target
                           input-path
                           lib-path
                           minify?
                           verbosity)

     ;; This version of the compiler reads the program and runtime library
     ;; source code from files and it supports various options.  It can
     ;; merge the compacted RVM code with the implementation of the RVM
     ;; for a specific target and minify the resulting target code.

     (write-target-code
      output-path
      (generate-code
       target
       verbosity
       input-path
       minify?
       (compile-program
        verbosity
        (read-program lib-path src-path)))))

   (define (parse-cmd-line args)
     (if (null? (cdr args))

         (pipeline-compiler)

         (let ((verbosity 0)
               (target "rvm")
               (input-path #f)
               (output-path #f)
               (lib-path "default")
               (src-path #f)
               (minify? #f))

           (let loop ((args (cdr args)))
             (if (pair? args)
                 (let ((arg (car args))
                       (rest (cdr args)))
                   (cond ((and (pair? rest) (member arg '("-t" "--target")))
                          (set! target (car rest))
                          (loop (cdr rest)))
                         ((and (pair? rest) (member arg '("-i" "--input")))
                          (set! input-path (car rest))
                          (loop (cdr rest)))
                         ((and (pair? rest) (member arg '("-o" "--output")))
                          (set! output-path (car rest))
                          (loop (cdr rest)))
                         ((and (pair? rest) (member arg '("-l" "--library")))
                          (set! lib-path (car rest))
                          (loop (cdr rest)))
                         ((and (pair? rest) (member arg '("-m" "--minify")))
                          (set! minify? #t)
                          (loop rest))
                         ((member arg '("-v" "--v"))
                          (set! verbosity (+ verbosity 1))
                          (loop rest))
                         ((member arg '("-vv" "--vv"))
                          (set! verbosity (+ verbosity 2))
                          (loop rest))
                         ((member arg '("-vvv" "--vvv"))
                          (set! verbosity (+ verbosity 3))
                          (loop rest))
                         ((member arg '("-q")) ;; silently ignore Chicken's -q option
                          (loop rest))
                         (else
                          (if (and (>= (string-length arg) 2)
                                   (string=? (substring arg 0 1) "-"))
                              (begin
                                (display "*** ignoring option ")
                                (display arg)
                                (newline)
                                (loop rest))
                              (begin
                                (set! src-path arg)
                                (loop rest))))))))

           (if (not src-path)

               (begin
                 (display "*** a Scheme source file must be specified\n")
                 (exit-program-abnormally))

               (fancy-compiler
                src-path
                (or output-path
                    (if (or (equal? src-path "-") (equal? target "rvm"))
                        "-"
                        (string-append
                         src-path
                         (string-append "." target))))
                target
                input-path
                lib-path
                minify?
                verbosity)))))

   (parse-cmd-line (cmd-line))

   (exit-program-normally)))

;;;----------------------------------------------------------------------------
