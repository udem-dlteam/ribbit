#!/usr/bin/env gsi

;;!#;; satisfy guile

;;; Ribbit Scheme compiler.

;;;----------------------------------------------------------------------------

;; Compatibility layer.

;; Tested with Gambit v4.7.5 and above, Guile 3.0.7 and Chicken 5.2.0.

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

  (else

   (define (shell-cmd command)
     #f)

   (define (del-file path)
     #f)))

(cond-expand

  ((or gambit
       guile
       chicken)

   (define (pipe-through program output)
     (let ((tmpin  "rsc.tmpin")
           (tmpout "rsc.tmpout"))
       (call-with-output-file
           tmpin
         (lambda (port) (display output port)))
       (shell-cmd (string-append program " < " tmpin " > " tmpout))
       (let ((out
              (call-with-input-file
                  tmpout
                (lambda (port) (read-line port #f)))))
         (del-file tmpin)
         (del-file tmpout)
         out))))

  (else

   (define (pipe-through program output)
     (println "*** Minification is not supported with this Scheme system")
     (println "*** so the generated code was not minified.")
     (println "*** You might want to try running " program " manually.")
     output)))

(cond-expand

  (chicken

   (import (chicken process-context))

   (define (cmd-line)
     (command-line-arguments))

   (define (exit-program-normally)
     (exit 0)))

  (else

   (define (cmd-line)
     (command-line))

   (define (exit-program-normally)
     (exit 0))))

(cond-expand

  (gambit

   (define (with-output-to-str thunk)
     (with-output-to-string "" thunk)))

  (chicken

   (import (chicken port))

   (define (with-output-to-str thunk)
     (with-output-to-string thunk)))

  (else

   (define (with-output-to-str thunk)
     (with-output-to-string thunk))))

(cond-expand

  (gambit

   (define (symbol->str symbol)
     (symbol->string symbol))

   (define (str->uninterned-symbol string)
     (string->uninterned-symbol string)))

  (else

   (define (make-table)
     (vector '()))

   (define (table-ref table key . default)
     (let ((x (assoc key (vector-ref table 0))))
       (if x
           (cdr x)
           (if (pair? default)
               (car default)
               (error "Unbound key")))))

   (define (table-set! table key value)
     (let ((x (assoc key (vector-ref table 0))))
       (if x
           (set-cdr! x value)
           (vector-set! table
                        0
                        (cons (cons key value) (vector-ref table 0))))))

   (define (table-length table)
     (length (vector-ref table 0)))

   (define (table->list table)
     (vector-ref table 0))

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
     (table-ref uninterned-symbols symbol (symbol->string symbol)))

   (define (path-extension path)
     (let loop ((i (- (string-length path) 1)))
       (if (< i 0)
           ""
           (if (char=? (string-ref path i) #\.)
               (substring path i (string-length path))
               (loop (- i 1))))))

   (define (path-directory path)
     (let loop ((i (- (string-length path) 1)))
       (if (< i 0)
           "./"
           (if (char=? (string-ref path i) #\/)
               (substring path 0 (+ i 1))
               (loop (- i 1))))))

   (define (path-expand path . dir)
     (if (pair? dir)
         (let ((d (car dir)))
           (if (= (string-length d) 0)
               path
               (if (char=? (string-ref d (- (string-length d) 1)) #\/)
                   (string-append d path)
                   (string-append d "/" path))))
         path))

   (define (read-line port . separator)
     (let ((sep (if (pair? separator) (car separator) #\newline)))
       (let loop ((rev-chars '()))
         (let ((c (read-char port)))
           (if (or (eof-object? c) (eqv? c sep))
               (list->string (reverse rev-chars))
               (loop (cons c rev-chars)))))))

   (define (println . rest)
     (for-each display rest)
     (newline))

   (define (pp obj)
     (write obj)
     (newline))))

(cond-expand

  ((and gambit (or enable-bignum disable-bignum))) ;; recent Gambit?

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
     (list-sort! compare (append list '())))

   (define (script-file)
     (car (cmd-line)))

   (define (executable-path)
     "")))

(cond-expand

  ((and gambit ;; hack to detect recent Gambit version
        (or enable-sharp-dot disable-sharp-dot)))

  (else

   (define (string-concatenate string-list . separator)
     (if (pair? separator)
         (if (pair? string-list)
             (string-append
              (car string-list)
              (apply string-append
                     (map (lambda (s) (string-append (car separator) s))
                          (cdr string-list))))
             "")
         (apply string-append string-list)))))

;;;----------------------------------------------------------------------------

(define predefined '(rib false true nil)) ;; predefined symbols

(define primitives '(
(rib         . 0) ;; predefined by RVM (must be first and 0)
(id          . 1)
(arg1        . 2)
(arg2        . 3)
(close       . 4)
(rib?        . 5)
(field0      . 6)
(field1      . 7)
(field2      . 8)
(field0-set! . 9)
(field1-set! . 10)
(field2-set! . 11)
(eqv?        . 12)
(<           . 13)
(+           . 14)
(-           . 15)
(*           . 16)
(quotient    . 17)
(getchar     . 18)
(putchar     . 19)
))

(define jump/call-op 'jump/call)
(define set-op       'set)
(define get-op       'get)
(define const-op     'const)
(define if-op        'if)

;;;----------------------------------------------------------------------------

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (instance? o type) (and (rib? o) (eqv? (field2 o) type)))

(define (rib field0 field1 field2) (vector field0 field1 field2))
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
(define (procedure-env proc) (field1 proc))

(define (oper pc) (field0 pc))
(define (opnd pc) (field1 pc))
(define (next pc) (field2 pc))

;;;----------------------------------------------------------------------------

;; The compiler from Ribbit Scheme to RVM code.

(define (make-ctx cte live exports) (rib cte (list live) exports))

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
           (let ((g (live? expr (ctx-live ctx))))
             (if (and g (constant? g)) ;; constant propagated?
                 (rib const-op (cadr (cadr g)) cont)
                 (rib get-op v cont)))))

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
                                  (if (constant? g)
                                      (begin
                                        '(pp `(*** constant propagation of ,var = ,(cadr g))
                                             (current-error-port))
                                        (gen-noop cont))
                                      (comp ctx val (gen-assign v cont)))
                                  (begin
                                    '(pp `(*** removed dead assignment to ,var)
                                         (current-error-port))
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
                  (let ((binding (car (cadr expr))))
                    (let ((body (cddr expr)))
                      (comp-bind ctx
                                 (car binding)
                                 (cadr binding)
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
                                   '_
                                   first
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

(define (comp-bind ctx var expr body cont)
  (comp ctx
        expr
        (comp-begin (ctx-cte-set ctx (cons var (ctx-cte ctx)))
                    body
                    (if (eqv? cont tail)
                        cont
                        (rib jump/call-op ;; call
                             (use-symbol ctx 'arg2)
                             cont)))))

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
  (let loop ((lst program) (rev-exprs '()) (exports '()))
    (if (pair? lst)
        (let ((first (car lst)))
          (if (and (pair? first) (eqv? (car first) 'export))
              (loop (cdr lst)
                    rev-exprs
                    (append (cdr first) exports))
              (loop (cdr lst)
                    (cons first rev-exprs)
                    exports)))
        (cons (reverse rev-exprs) exports))))

(define (exports->alist exports)
  (if (pair? exports)
      (let ((x (car exports)))
        (cons (if (symbol? x)
                  (cons x x)
                  (cons (car x) (cadr x)))
              (exports->alist (cdr exports))))
      '()))

(define (comp-exprs-with-exports exprs exports)
  (let* ((expansion (expand-begin exprs))
         (live (liveness-analysis expansion exports)))
    (cons
     (make-procedure
      (rib 0 ;; 0 parameters
           0
           (comp (make-ctx '() live exports)
                 expansion
                 tail))
      '())
     (or exports
         (map (lambda (v)
                (let ((var (car v)))
                  (cons var var)))
              live)))))

(define (compile-program verbosity program)
  (let* ((exprs-and-exports
          (extract-exports program))
         (exprs
          (car exprs-and-exports))
         (exports
          (cdr exprs-and-exports))
         (proc-and-exports
          (comp-exprs-with-exports
           (if (pair? exprs) exprs (list #f))
           (exports->alist exports))))
    (if (>= verbosity 2)
        (begin
          (println "*** RVM code:")
          (pp (car proc-and-exports))))
    (if (>= verbosity 3)
        (begin
          (println "*** exports:")
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
                                    (cons (expand-expr (cadddr expr))
                                          '())))))

                 ((eqv? first 'lambda)
                  (let ((params (cadr expr)))
                    (cons 'lambda
                          (cons params
                                (cons (expand-begin (cddr expr))
                                      '())))))

                 ((eqv? first 'let)
                  (let ((binding (car (cadr expr))))
                    (cons 'let
                          (cons (cons (cons (car binding)
                                            (cons (expand-expr (cadr binding))
                                                  '()))
                                      '())
                                (cons (expand-begin (cddr expr))
                                      '())))))

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

  (define (liveness expr cte top?)

    (cond ((symbol? expr)
           (if (in? expr cte) ;; local var?
               #f
               (add expr))) ;; mark the global variable as "live"

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eqv? first 'quote)
                    (let ((val (cadr expr)))
                      (if (symbol? val)
                          (add val))
                      #f))

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
                    (let ((binding (car (cadr expr))))
                      (liveness (cadr binding) cte #f)
                      (liveness (caddr expr) (cons (car binding) cte) #f)))

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
           (cdr (assq sym primitives)) ;; get index
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
           (scan-proc o)))
    o)

  (define (scan code)
    (if (rib? code)
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
                 (scan-opnd o 3))) ;; 3 = set
          (scan (next code)))))

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
        (error "rib expected")))

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
                               (string-concatenate
                                (map (lambda (s)
                                       (let ((str (symbol->str s)))
                                         (list->string
                                          (reverse (string->list str)))))
                                     symbols*)
                                ",")
                               ";"
                               (stream->string stream)))))))))))))

(define (stream->string stream)
  (list->string
   (map (lambda (n)
          (let ((c (+ n 35)))
            (integer->char (if (= c 92) 33 c))))
        stream)))

;;;----------------------------------------------------------------------------

;; Source code reading.

(define (root-dir)
  (path-directory (or (script-file) (executable-path))))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
        '()
        (cons x (read-all)))))

(define (read-from-file path)
  (with-input-from-file path read-all))

(define (read-library lib-path)
  (read-from-file
   (if (equal? (path-extension lib-path) "")
       (path-expand (string-append lib-path ".scm")
                    (path-expand "lib" (root-dir)))
       lib-path)))

(define (read-program lib-path src-path)
  (append (read-library lib-path)
          (read-from-file src-path)))

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
          (if (equal? target "none")
              ""
              (string-from-file
               (path-expand (string-append "host/" target "/rvm." target)
                            (root-dir)))))
         (input
          (string-append encoded-program
                         (if input-path
                             (string-from-file input-path)
                             ""))))

    (if (>= verbosity 1)
        (println "*** RVM code length: " (string-length input) " bytes"))

    (let* ((target-code-before-minification
            (with-output-to-str
             (lambda ()
               (cond ((equal? target "none")
                      (display input)
                      (newline))
                     ((equal? target "scm")
                      (write `(define input ,input))
                      (newline))
                     ((equal? target "go")
                      (display "const Input = ")
                      (write input)
                      (newline))
                     (else
                      (cond ((equal? target "c")
                             (display "char *")))
                      (display "input = ")
                      (write input)
                      (cond ((not (equal? target "py"))
                             (display ";")))
                      (newline)))
               (display vm-source))))
           (target-code
            (if (or (not minify?) (equal? target "none"))
                target-code-before-minification
                (pipe-through
                 (path-expand
                  (string-append "host/" target "/minify")
                  (root-dir))
                 target-code-before-minification))))
      target-code)))

(define (write-target-code output-path target-code)
  (if (equal? output-path "-")
      (display target-code)
      (with-output-to-file
          output-path
        (lambda ()
          (display target-code)))))

;;;----------------------------------------------------------------------------

;; Compiler's main.

(define (compiler-main src-path
                       output-path
                       target
                       input-path
                       lib-path
                       minify?
                       verbosity)
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

;;;----------------------------------------------------------------------------

;; Compiler's command line processing.

(define (parse-cmd-line args)
  (let ((verbosity 0)
        (target "none")
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
                    (if (and (>= (string-length arg) 1)
                             (string=? (substring arg 0 1) "-"))
                      (begin
                        (println "*** ignoring option " arg)
                        (loop rest))
                      (begin
                        (set! src-path arg)
                        (loop rest))))))))

    (if (not src-path)

        (begin
          (println "*** a Scheme source file must be specified")
          (exit 1))

        (compiler-main src-path
                       (or output-path
                           (if (equal? target "none")
                               "-"
                               (string-append src-path "." target)))
                       target
                       input-path
                       lib-path
                       minify?
                       verbosity))))

(parse-cmd-line (cmd-line))

(exit-program-normally)

;;;----------------------------------------------------------------------------
