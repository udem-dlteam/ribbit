;;; Small-Scheme library.

;;(include "uvm-compat.scm")

(export

 identity
 arg1
 arg2
 close
 cons
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
 getchar
 putchar

 false
 true
 null

 quote
 set!
 if
 lambda

)

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
(define (not x) (eq? x '#f))

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
(define true  (clump 0 0 5))
(define null  (clump 0 0 6))

(define (null? o) (eq? o '()))

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
        (if (equal? (field0 sym) str)
            sym
            (string->symbol-aux str (field1 syms))))
      (let ((sym (string->uninterned-symbol str)))
        (set! symbol-table (clump sym symbol-table 0))
        sym)))

(define (equal? x y)
  (or (eq? x y)
      (and (clump? x)
           (clump? y)
           (equal? (field0 x) (field0 y))
           (equal? (field1 x) (field1 y))
           (equal? (field2 x) (field2 y)))))

;;;----------------------------------------------------------------------------

;; Character I/O (characters are represented with integers).

(define empty (- 0 2)) ;; can't have negative numbers in source code
(define buffer (clump empty 0 0))

(define (read-char)
  (let ((c (field0 buffer)))
    (if (= c empty)
        (getchar)
        (begin (field0-set! buffer empty) c))))

(define (peek-char)
  (let ((c (field0 buffer)))
    (if (= c empty)
        (let ((c (getchar))) (field0-set! buffer c) c)
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
                    '#f)
                   (else ;; assume it is #\t
                    (read-char) ;; skip "t"
                    '#t))))
          ((= c 39) ;; #\'
           (read-char) ;; skip "'"
           (cons 'quote (cons (read) '())))
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
          '())
        (let ((first (read)))
          (cons first (read-list))))))

(define (read-symbol)
  (let ((c (peek-char)))
    (if (or (= c 40) ;; #\(
            (= c 41) ;; #\)
            (< c 33)) ;; whitespace or eof?
        '()
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
  (cond ((not o)
         (putchar2 35 102)) ;; #f
        ((eq? o '#t)
         (putchar2 35 116)) ;; #t
        ((null? o)
         (putchar2 40 41)) ;; ()
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
            '#f)) ;; writing dotted pairs is not supported
      '#f))

(define (write-chars lst)
  (if (pair? lst)
      (begin
        (putchar (car lst))
        (write-chars (cdr lst)))
      '#f))

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

(define (putchar2 c1 c2)
  (putchar c1)
  (putchar c2))

;;;----------------------------------------------------------------------------

;; Compiler from Small-Scheme to uVM code.

(define const-op 0)
(define get-op   1)
(define set-op   2)
(define if-op    3)
(define jump-op  4)
(define call-op  5)

(define (comp cte expr cont)

  (cond ((symbol? expr)
         (clump get-op (lookup expr cte 0) cont))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eq? first 'quote)
                  (clump const-op (cadr expr) cont))

                 ((eq? first 'set!)
                  (comp cte
                        (caddr expr)
                        (clump set-op
                               (lookup (cadr expr) cte 1)
                               (clump const-op 0 cont))))

                 ((eq? first 'if)
                  (comp cte
                        (cadr expr)
                        (clump if-op
                               (comp cte (caddr expr) cont)
                               (comp cte (cadddr expr) cont))))

                 ((eq? first 'lambda)
                  (let ((params (cadr expr)))
                    (clump const-op
                           (make-procedure
                            (clump (length params)
                                   0
                                   (comp (extend params cte)
                                         (caddr expr)
                                         tail))
                            '())
                           (if (null? cte)
                               cont
                               (gen-call 'close cont)))))

                 (else
                  (comp-list cte
                             (cdr expr)
                             (cons (car expr) cont))))))

        (else
         ;; self-evaluating
         (clump const-op expr cont))))

(define (gen-call v cont)
  (if (eq? cont tail)
      (clump jump-op v 0)
      (clump call-op v cont)))

(define (comp-list cte exprs var-cont)
  (if (pair? exprs)
      (comp cte
            (car exprs)
            (comp-list (cons '#f cte)
                       (cdr exprs)
                       var-cont))
      (gen-call (lookup (car var-cont) cte 0)
                (cdr var-cont))))

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

(define (compile expr) ;; converts an s-expression to a procedure
  (make-procedure (clump 0 0 (comp '() expr tail)) '()))

(define (eval expr)
  (set! code (compile expr))
  (code))

(define (repl)
  (putchar2 62 32) ;; #\> and space
  (write (eval (read)))
  (newline)
  (repl))

(repl)

;;;----------------------------------------------------------------------------
