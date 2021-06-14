;;; Small-Scheme library.

;;(include "uvm-compat.scm")

;;;----------------------------------------------------------------------------

;; Implementation of Small-Scheme types using the uVM operations.

(define (instance? o type) (and (clump? o) (eq? (field2 o) type)))

(define (pair? o) (instance? o 0))
(define (clump f0 f1 f2) (let ((c (cons f0 f1))) (field2-set! c f2) c))
(define (car pair) (field0 pair))
(define (cdr pair) (field1 pair))
(define (set-car! pair x) (field0-set! pair x))
(define (set-cdr! pair x) (field1-set! pair x))
(define (cadr pair) (car (cdr pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caddr pair) (cadr (cdr pair)))
(define (cadddr pair) (caddr (cdr pair)))
(define (length x) (if (pair? x) (+ 1 (length (cdr x))) 0))

(define (procedure? o) (instance? o 1))
(define (make-procedure code env) (clump code env 1))
(define (procedure-code proc) (field0 proc))
(define (procedure-env proc) (field1 proc))

(define (string? o) (instance? o 2))
(define (list->string lst) (clump lst 0 2))
(define (string->list str) (field0 str))

(define (symbol? o) (instance? o 3))
(define (string->uninterned-symbol str) (clump str 0 3))
(define (symbol->string sym) (field0 sym))
(define (global-var-ref sym) (field1 sym))
(define (global-var-set! sym x) (field1-set! sym x))

(define false (clump 0 0 4))
(define true (clump 0 0 5))

(define null (clump 0 0 6))
(define (null? o) (eq? o null))

(define (= x y) (eq? x y))

;;;----------------------------------------------------------------------------

;; String to integer conversion.

(define (string->number str)
  (string->number-aux (string->list str) 0))

(define (string->number-aux lst n)
  (if (pair? lst)
      (let ((c (car lst)))
        (and (< 47 c)
             (< c 58)
             (string->number-aux (cdr lst) (+ (* 10 n) (- c 48)))))
      n))

;;;----------------------------------------------------------------------------

;; Symbol table.

(define symbol-table 0)

(define (string->symbol str)
  (string->symbol-aux str symbol-table))

(define (string->symbol-aux str syms)
  (if (clump? syms)
      (let ((sym (field0 syms)))
        (let ((name (field0 sym)))
          (if (equal? name str)
              sym
              (string->symbol-aux str (field1 syms)))))
      (let ((sym (string->uninterned-symbol str)))
        (set! symbol-table (clump sym symbol-table 0))
        sym)))

(define (equal? x y)
  (if (clump? x)
      (and (clump? y)
           (equal? (field0 x) (field0 y))
           (equal? (field1 x) (field1 y))
           (equal? (field2 x) (field2 y)))
      (eq? x y)))

(define $identity    'identity)
(define $arg1        'arg1)
(define $arg2        'arg2)
(define $close       'close)
(define $cons        'cons)
(define $clump?      'clump?)
(define $field0      'field0)
(define $field1      'field1)
(define $field2      'field2)
(define $field0-set! 'field0-set!)
(define $field1-set! 'field1-set!)
(define $field2-set! 'field2-set!)
(define $eq?         'eq?)
(define $<           '<)
(define $+           '+)
(define $-           '-)
(define $*           '*)
(define $quotient    'quotient)
(define $getchar     'getchar)
(define $putchar     'putchar)

(define $false       'false)
(define $true        'true)

(define $quote       'quote)
(define $set!        'set!)
(define $if          'if)
(define $lambda      'lambda)
(define $let         'let)
(define $begin       'begin)
(define $define      'define)
(define $and         'and)
(define $or          'or)
(define $cond        'cond)
(define $else        'else)

;;;----------------------------------------------------------------------------

;; Character I/O (characters are represented with integers).

(define empty (- 0 2))
(define buf (clump empty 0 0))

(define (read-char)
  (let ((c (field0 buf)))
    (if (= c empty)
        (getchar)
        (begin (field0-set! buf empty) c))))

(define (peek-char)
  (let ((c (field0 buf)))
    (if (= c empty)
        (let ((c (getchar))) (field0-set! buf c) c)
        c)))

;;;----------------------------------------------------------------------------

;; The read procedure.

(define (read)
  (let ((c (peek-char-non-whitespace)))
    (cond ((< c 0)
           c)
          ((= c 40) ;; #\(
           (read-char) ;; skip "("
           (read-list))
          ((= c 35) ;; #\#
           (read-char) ;; skip "#"
           (let ((c (peek-char)))
             (cond ((= c 102) ;; #\f
                    (read-char) ;; skip "f"
                    false)
                   ((= c 116) ;; #\t
                    (read-char) ;; skip "t"
                    true)
                   (else
                    (halt))))) ;; TODO: error?
          ((= c 39) ;; #\'
           (read-char) ;; skip "'"
           (cons $quote (cons (read) null)))
          (else
           (read-char) ;; skip first char
           (let ((s (list->string (cons c (read-symbol)))))
             (let ((n (string->number s)))
               (if n ;; or can't be used here because it isn't Scheme's or
                   n
                   (string->symbol s))))))))

(define (read-list)
  (let ((c (peek-char-non-whitespace)))
    (if (= c 41) ;; #\)
        (begin
          (read-char) ;; skip ")"
          null)
        (let ((first (read)))
          (let ((rest (read-list)))
            (cons first rest))))))

(define (read-symbol)
  (let ((c (peek-char)))
    (if (or (= c 40) ;; #\(
            (= c 41) ;; #\)
            (< c 33)) ;; whitespace or eof?
        null
        (begin
          (read-char)
          (cons c (read-symbol))))))

(define (peek-char-non-whitespace)
  (let ((c (peek-char)))
    (if (< c 0) ;; eof?
        c
        (if (< 32 c) ;; above #\space ?
            (if (= c 59) ;; #\;
                (skip-comment)
                c)
            (begin
              (read-char)
              (peek-char-non-whitespace))))))

(define (skip-comment)
  (let ((c (read-char)))
    (if (< c 0) ;; eof?
        c
        (if (= c 10) ;; #\newline
            (peek-char-non-whitespace)
            (skip-comment)))))

;;;----------------------------------------------------------------------------

;; The write procedure.

(define (write o)
  (cond ((eq? o false)
         (putchar 35)   ;; #\#
         (putchar 102)) ;; #\f
        ((eq? o true)
         (putchar 35)   ;; #\#
         (putchar 116)) ;; #\t
        ((eq? o null)
         (putchar 40)  ;; #\(
         (putchar 41)) ;; #\)
        ((pair? o)
         (putchar 40)  ;; #\(
         (write (car o))
         (write-list (cdr o))
         (putchar 41))  ;; #\)
        ((symbol? o)
         (write-chars (string->list (symbol->string o))))
        (else
         ;; must be a number
         (write-number o))))

(define (write-list lst)
  (if (pair? lst)
      (begin
        (putchar 32) ;; #\space
        (if (pair? lst)
            (begin
              (write (car lst))
              (write-list (cdr lst)))
            (begin ;; probably overkill to implement dotted pairs
              (putchar 46) ;; #\.
              (putchar 32) ;; #\space
              (write lst))))
      false))

(define (write-chars lst)
  (if (pair? lst)
      (begin
        (putchar (car lst))
        (write-chars (cdr lst)))
      false))

(define (write-number n)
  ;; only supports nonnegative integers
  (let ((q (quotient n 10)))
    (let ((d (+ 48 (- n (* q 10)))))
      (if (< 0 q)
          (begin
            (write-number q)
            (putchar d)) ;; 0..9
          (putchar d))))) ;; 0..9

(define (newline)
  (putchar 10))

;;;----------------------------------------------------------------------------

;; Compiler from Small-Scheme to uVM code.

;;(define const-op $const)
;;(define proc-op  $proc)
;;(define get-op   $get)
;;(define set-op   $set)
;;(define if-op    $if)
;;(define jump-op  $jump)
;;(define call-op  $call)
(define const-op 0)
(define proc-op  1)
(define get-op   2)
(define set-op   3)
(define if-op    4)
(define jump-op  5)
(define call-op  6)

(define (comp expr cte cont)
  (cond ((symbol? expr)
         (clump get-op (lookup expr cte 0) cont))
        ((pair? expr)
         (let ((first (car expr)))
           (cond ((eq? first $quote)
                  (clump const-op (cadr expr) cont))
                 ((eq? first $set!)
                  (comp (caddr expr)
                        cte
                        (clump set-op (lookup (cadr expr) cte 1) cont)))
                 ((eq? first $if)
                  (let ((cont-false (comp (cadddr expr) cte cont)))
                    (let ((cont-true (comp (caddr expr) cte cont)))
                      (let ((cont-test (clump if-op cont-true cont-false)))
                        (comp (cadr expr) cte cont-test)))))
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

(define (compile expr) ;; converts an s-expression to a procedure
  (make-procedure (clump 0 0 (comp expr null tail)) null))

(define (eval expr)
  (set! code (compile expr))
  (code))

(define (repl)
  (putchar 62) ;; #\>
  (putchar 32) ;; space
  (write (eval (read)))
  (newline)
  (repl))

;;(repl)

;;;----------------------------------------------------------------------------
