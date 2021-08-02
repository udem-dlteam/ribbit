#!/usr/bin/env gsi

;;; Small-Scheme compiler.

;;;----------------------------------------------------------------------------

(define primitives
  '(clump
    identity
    arg1
    arg2
    clump?
    field0
    field1
    field2
    field0-set!
    field1-set!
    field2-set!
    eq?
    <
    +
    -
    *
    quotient
    putchar
    getchar
    close))

(define predefined
  '(symtbl false true null clump))

(define jump-op  (##quote jump))
(define call-op  (##quote call))
(define set-op   (##quote set))
(define get-op   (##quote get))
(define const-op (##quote const))
(define if-op    (##quote if))

;;;----------------------------------------------------------------------------

(define (instance? o type) (and (clump? o) (eq? (field2 o) type)))

(define (clump field0 field1 field2) (vector field0 field1 field2))
(define (clump? o) (vector? o))
(define (field0 o) (vector-ref o 0))
(define (field1 o) (vector-ref o 1))
(define (field2 o) (vector-ref o 2))
(define (field0-set! o x) (vector-set! o 0 x) o)
(define (field1-set! o x) (vector-set! o 1 x) o)
(define (field2-set! o x) (vector-set! o 2 x) o)

(define (procedure2? o) (instance? o 1))
(define (make-procedure code env) (clump code env 1))
(define (procedure-code proc) (field0 proc))
(define (procedure-env proc) (field1 proc))

(define (oper pc) (field0 pc))
(define (opnd pc) (field1 pc))
(define (next pc) (field2 pc))

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
                                        '(pp `(*** constant propagation of ,var = ,(cadr g))
                                            (current-error-port))
                                        (gen-noop cont))
                                      (comp ctx val (gen-assign v cont)))
                                  (begin
                                    '(pp `(*** removed dead assignment to ,var)
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
                                                (extend params
                                                        (cons #f
                                                              (cons #f
                                                                    (ctx-cte ctx)))))
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
            (comp-list (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
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
            (comp-program* exprs #f)))
      (comp-program* exprs #f)))

(define (comp-program* exprs exports)
  (let ((expansion (expand-begin exprs)))
    (let ((live (liveness-analysis expansion exports)))
;;      (pp `(***expansion: ,(uvm->host expansion)))
;;      (pp (uvm->host live))
      (cons
       (make-procedure
        (clump 0 ;; 0 parameters
               0
               (comp (make-ctx '() live exports)
                     expansion
                     tail))
        '())
       (or exports
           (map (lambda (v)
                  (let ((var (car v)))
                    (cons var var)))
                live))))))

(define (exports->alist exports)
  (if (pair? exports)
      (let ((x (car exports)))
        (cons (if (symbol? x)
                  (cons x x)
                  (cons (car x) (cadr x)))
              (exports->alist (cdr exports))))
      '()))

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
                                                    (cons #f
                                                          '()))))
                                  (cadr expr))
                              #t)))

                 ((eq? first 'or)
                  (expand (if (pair? (cdr expr))
                              (if (pair? (cddr expr))
                                  (cons 'if
                                        (cons (cadr expr)
                                              (cons #t ;; not correct but OK
                                                    (cons (cons 'or
                                                                (cddr expr))
                                                          '()))))
                                  (cadr expr))
                              #f)))

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
                              #f)))

                 (else
                  (expand-list expr)))))

        (else
         (expand-constant expr))))

(define (expand-constant x)
  (cond ((eq? x #f)  'false)
        ((eq? x #t)  'true)
        ((eq? x '()) 'null)
        (else        (cons 'quote (cons x '())))))
        
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
  (let loop ((live-globals (exports->live (or exports '()))))
    (reset-defs live-globals)
    (let ((x (liveness expr live-globals (not exports))))
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
        #f)))

(define (live? var lst)
  (if (pair? lst)
      (let ((x (car lst)))
        (if (eq? var (car x))
            x
            (live? var (cdr lst))))
      #f))

(define (constant? g)
  (and (pair? (cdr g))
       (null? (cddr g))
       (pair? (cadr g))
       (eq? 'quote (car (cadr g)))))

(define (in? var cte)
  (not (eq? var (lookup var cte 0))))

(define (liveness expr live-globals export-all?)

  (define (add-live var)
    (if (live? var live-globals)
        #f
        (let ((g (cons var '())))
          (set! live-globals (cons g live-globals))
          g)))

  (define (liveness expr cte top?)

    (cond ((symbol? expr)
           (if (in? expr cte) ;; local var?
               #f
               (add-live expr))) ;; mark the global variable as "live"

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eq? first 'quote)
                    #f)

                   ((eq? first 'set!)
                    (let ((var (cadr expr)))
                      (let ((val (caddr expr)))
                        (if (in? var cte) ;; local var?
                            (liveness val cte #f)
                            (begin
                              (if export-all? (add-live var))
                              (let ((g (live? var live-globals))) ;; variable live?
                                (if g
                                    (begin
                                      (set-cdr! g (cons val (cdr g)))
                                      (liveness val cte #f))
                                    #f)))))))

                   ((eq? first 'if)
                    (liveness (cadr expr) cte #f)
                    (liveness (caddr expr) cte #f)
                    (liveness (cadddr expr) cte #f))

                   ((eq? first 'let)
                    (let ((binding (car (cadr expr))))
                      (liveness (cadr binding) cte #f)
                      (liveness (caddr expr) (cons (car binding) cte) #f)))

                   ((eq? first 'begin)
                    (liveness-list (cdr expr) cte))

                   ((eq? first 'lambda)
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

(define decoder
  (vector jump-sym-short     ;; (+ 1 2) INT SYM
          call-sym-short     ;; (+ 1 2) INT SYM
          0                  ;; (+ 1 2) INT SYM
          get-int-short      ;; (+ 1 2) INT SYM
          const-int-short    ;; (+ 1 2) INT SYM
          const-proc-short)) ;; (+ 1 1) INT IF

(define ops ;; TODO: remove after debugging
  (vector jump-op
          call-op
          set-op
          get-op
          const-op
          if-op))

(define (encode proc exports)

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
             (cond ((= pos 0)
                    (field0-set! descr (+ 1 (field0 descr))))
                   ((= pos 1)
                    (field1-set! descr (+ 1 (field1 descr))))
                   ((= pos 2)
                    (field2-set! descr (+ 1 (field2 descr)))))))
          ((procedure2? o)
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
                       (scan-opnd (opnd code) 2)) ;; 2 = const
                      ((eq? op set-op)
                       (scan-opnd (opnd code) 3))) ;; 3 = set
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
    (if (clump? code)
        (let ((op (oper code)))
          (if (eq? op jump-op)

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

              (cond ((eq? op call-op)
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
                                   (error "can't encode call" o))))))

                    ((eq? op const-op)
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

                    ((eq? op set-op)
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

                    ((eq? op get-op)
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

                    ((eq? op if-op)
                     (enc (next code)
                          (enc (opnd code)
                               (cons if-start
                                     stream))))

                    (else
                     (error "unknown op" op)))))
        (error "clump expected")))

  (define (ordering sym-descr)
    (let ((sym (car sym-descr)))
      (let ((pos (member sym predefined)))
        (if pos
            (+ 9999999 (length pos))
            (let ((descr (cdr sym-descr)))
              (field0 descr))))))

  (for-each (lambda (sym) (scan-opnd sym 3)) predefined)

  (scan-proc proc)

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
                  (let ((symbol (if x (cdr x) (string->uninterned-symbol ""))))
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
                                (let ((symbol (string->uninterned-symbol "")))
                                  (let ((descr (cdr s)))
                                    (field0-set! descr i)
                                    (loop3 (+ i 1) (cdr lst3) (cons symbol symbols))))))))
                      (let loop4 ((symbols* symbols))
                        (if (and (pair? symbols*)
                                 (string=? (symbol->string (car symbols*)) ""))
                            (loop4 (cdr symbols*))

                            (let ((stream
                                   (enc-proc proc '())))
                              (string-append
                               (stream->string
                                (encode-n (- (length symbols)
                                             (length symbols*))
                                          '()))
                               (append-strings
                                (map (lambda (s)
                                       (let ((str (symbol->string s)))
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

(define (decode stream)

  (define pos 0)

  (define (rd-char)
    (let ((x (char->integer (string-ref stream pos))))
      (set! pos (+ pos 1))
      x))

  (define (rd-code)
    (let ((x (- (rd-char) 35)))
      (if (< x 0) 57 x)))

  (define (rd-int n)
    (let ((x (rd-code)))
      (let ((y (* n eb/2)))
        (if (< x eb/2)
            (+ y x)
            (rd-int (+ y (- x eb/2)))))))

  (define (build-symbols)
    (let loop0 ((n (rd-int 0)) (symbols '()))
      (if (> n 0)
          (loop0 (- n 1) (cons (string->uninterned-symbol "") symbols))
          (let loop1 ((symbols symbols))
            (let loop2 ((accum '()))

              (define (add-symbol)
                (cons (string->uninterned-symbol (list->string accum)) symbols))

              (let ((c (rd-char)))
                (if (= c 44) ;; #\, separates symbols
                    (loop1 (add-symbol))
                    (if (= c 59) ;; #\; terminates symbol list
                        (add-symbol)
                        (loop2 (cons (integer->char c) accum))))))))))

  (let ((symbols (build-symbols)))

    (define stack 0)

    (define (sym n)
      (list-ref symbols n))

    (define (dec)
      (let ((x (rd-code)))
        (let loop ((op 0) (n x))
          (let ((d (vector-ref decoder op)))
            (if (< (+ d 2) n)
                (loop (+ op 1) (- n (+ d 3)))
                (let ((_op (vector-ref ops op))) ;; TODO: replace with identity
                  (if (= op 0) (set! stack (clump 0 stack 0)) #f)
                  (if (< n d)        ;; short?
                      (if (< op 3)   ;; for jump/call/set short is sym
                          (add _op (sym n))
                          (if (< op 5) ;; for get/const short is int
                              (add _op n)
                              (add-const-proc n)))
                      (if (= x if-start)
                          (let ((code (field0 stack)))
                            (set! stack (field1 stack))
                            (add if-op code))
                          (let ((opnd
                                 (if (= n d) ;; int?
                                     (rd-int 0)
                                     (sym (rd-int (- n (+ d 1)))))))
                            (if (< op 5)
                                (add _op opnd)
                                (add-const-proc opnd)))))))))))

    (define (add-const-proc n)
      (let ((proc (make-procedure
                   (clump n 0 (field0 stack))
                   '())))
        (set! stack (field1 stack))
        (if (eq? stack 0)
            proc
            (add const-op proc))))

    (define (add op opnd)
      (field0-set! stack (clump op opnd (field0 stack)))
      (dec))

;;    (pp symbols)

    (dec)))

;;;----------------------------------------------------------------------------

(define (read-program)
  (let ((x (read)))
    (if (eof-object? x)
        '()
        (cons x (read-program)))))

(define (read-from-string str)
  (with-input-from-string str read-program))

(define (read-from-file path)
  (with-input-from-file path read-program))

(define (string-from-file path)
  (call-with-input-file path (lambda (port) (read-line port #f))))

(define (ssc-main src-path output-path target input-path verbosity)
  (let ((program (read-from-file src-path)))
    (let ((prog-exports (comp-program program)))
      (let ((proc (car prog-exports)))
        (let ((exports (cdr prog-exports)))
          (if (>= verbosity 1)
              (begin
                (println "*** uVM code:")
                (pp proc)))
          (if (>= verbosity 2)
              (begin
                (println "*** exports:")
                (pp exports)))
          (let ((encoded-program (encode proc exports)))
            (let ((input
                   (if input-path
                       (string-from-file input-path)
                       ""))
                  (vm-source
                   (string-from-file
                    (string-append "vm." target ".in"))))
              (with-output-to-file
                  output-path
                (lambda ()
                  (print "input = ")
                  (write (string-append encoded-program input))
                  (println (if (equal? target "js") ";" ""))
                  (print vm-source))))))))))

(define (main . args)
  (let ((verbosity 0)
        (target "js")
        (input-path #f)
        (output-path #f)
        (src-path #f))

    (let loop ((args args))
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
                  ((member arg '("-v"))
                   (set! verbosity (+ verbosity 1))
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
        (ssc-main src-path
                  (or output-path
                      (string-append src-path "." target))
                  target
                  input-path
                  verbosity))))

;;;----------------------------------------------------------------------------
