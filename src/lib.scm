;; This is the Ribbit Scheme runtime library.

;;;----------------------------------------------------------------------------

;; Exported symbols.

(export

#|
    rib
    id
    arg1
    arg2
    close
    rib?
    field0
    field1
    field2
    field0-set!
    field1-set!
    field2-set!
    eqv?
    <
    +
    -
    *
    quotient
    putchar
    getchar
|#

*
+
-
<
=
assoc
assq
cadddr
caddr
cadr
call/cc
car
cddr
cdr
cons
eof-object?
equal?
eqv?
eval
length
list->string
list-ref
list-tail
newline
not
null?
pair?
peek-char
procedure?
quotient
read
read-char
reverse
set-car!
set-cdr!
string->list
string->number
string->symbol
string-length
string-ref
string-set!
string?
symbol->string
symbol?
vector-length
vector-ref
vector-set!
vector?
write

quote set! define if lambda

#; ;; support for begin special form
begin

#; ;; support for single armed let special form
let

)

;;;----------------------------------------------------------------------------

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (primitive index) (rib index 0 procedure-type))

;;(define rib (primitive 0)) ;; predefined
(define id          (primitive 1))
(define arg1        (primitive 2))
(define arg2        (primitive 3))
(define close       (primitive 4))
(define rib?        (primitive 5))
(define field0      (primitive 6))
(define field1      (primitive 7))
(define field2      (primitive 8))
(define field0-set! (primitive 9))
(define field1-set! (primitive 10))
(define field2-set! (primitive 11))
(define eqv?        (primitive 12))
(define <           (primitive 13))
(define +           (primitive 14))
(define -           (primitive 15))
(define *           (primitive 16))
(define quotient    (primitive 17))
(define getchar     (primitive 18))
(define putchar     (primitive 19))

;;;----------------------------------------------------------------------------

;; Implementation of Ribbit Scheme types using the uVM operations.

(define (instance? type) (lambda (o) (and (rib? o) (eqv? (field2 o) type))))

(define pair? (instance? pair-type))
(define (cons car cdr) (rib car cdr pair-type))
(define car field0)
(define cdr field1)
(define set-car! field0-set!)
(define set-cdr! field1-set!)
(define (cadr pair) (field0 (field1 pair)))
(define (cddr pair) (field1 (field1 pair)))
(define (caddr pair) (cadr (field1 pair)))
(define (cadddr pair) (caddr (field1 pair)))
(define (not x) (eqv? x #f))

(define procedure? (instance? procedure-type))
(define (make-procedure code env) (rib code env procedure-type))
(define procedure-code field0)
(define procedure-env field1)

(define vector? (instance? vector-type))
(define (list->vector lst) (rib lst (length lst) vector-type))
(define vector->list field0)
(define vector-length field1)
(define (vector-ref vect i) (list-ref (field0 vect) i))
(define (vector-set! vect i x) (list-set! (field0 vect) i x))

(define string? (instance? string-type))
(define (list->string lst) (rib lst (length lst) string-type))
(define string->list field0)
(define string-length field1)
(define (string-ref str i) (list-ref (field0 str) i))
(define (string-set! str i x) (list-set! (field0 str) i x))

(define symbol? (instance? symbol-type))
(define (string->uninterned-symbol str) (rib #f str symbol-type))
(define symbol->string field1)
(define global-var-ref field0)
(define global-var-set! field0-set!)

;;(define false (rib 0 0 singleton-type)) ;; predefined
;;(define true  (rib 0 0 singleton-type)) ;; predefined
;;(define nil   (rib 0 0 singleton-type)) ;; predefined

(define (null? o) (eqv? o '()))
(define = eqv?)

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

(define (string->symbol str)
  (string->symbol-aux str symtbl))

(define (string->symbol-aux str syms)
  (if (pair? syms)
      (let ((sym (field0 syms)))
        (if (equal? (field1 sym) str)
            sym
            (string->symbol-aux str (field1 syms))))
      (let ((sym (string->uninterned-symbol str)))
        (set! symtbl (cons sym symtbl))
        sym)))

(define symtbl (field1 rib))
;;  (arg1 (field1 rib) (putchar 33))) ;; get the symbol table

(field1-set! rib 0) ;; release symbol table if not otherwise needed

;;;----------------------------------------------------------------------------

(define (equal? x y)
  (or (eqv? x y)
      (and (rib? x)
           (rib? y)
           (equal? (field0 x) (field0 y))
           (equal? (field1 x) (field1 y))
           (equal? (field2 x) (field2 y)))))

(define (list-ref lst i)
  (car (list-tail lst i)))

(define (list-set! lst i x)
  (set-car! (list-tail lst i) x))

(define (list-tail lst i)
  (if (< 0 i)
      (list-tail (cdr lst) (- i 1))
      lst))

(define (length lst)
  (if (pair? lst)
      (+ 1 (length (cdr lst)))
      0))

(define (assq x lst)
  (if (pair? lst)
      (let ((couple (car lst)))
        (if (eqv? x (car couple))
            couple
            (assq x (cdr lst))))
      #f))

(define (assoc x lst)
  (if (pair? lst)
      (let ((couple (car lst)))
        (if (equal? x (car couple))
            couple
            (assoc x (cdr lst))))
      #f))

(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst result)
  (if (pair? lst)
      (reverse-aux (cdr lst) (cons (car lst) result))
      result))

;;;----------------------------------------------------------------------------

;; First-class continuations.

(define (call/cc receiver)
  (let ((c (field1 (field1 (close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (field1 (field1 (close #f)))))
                  (field0-set! c2 (field0 c)) ;; set "stack" field
                  (field2-set! c2 (field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

;;;----------------------------------------------------------------------------

;; Character I/O (characters are represented with integers).

(define eof (- 0 1))
(define (eof-object? o) (eqv? o eof))

(define empty (- 0 2))
(define buffer (rib empty 0 0))

(define (read-char)
  (let ((c (field0 buffer)))
    (if (eqv? c eof)
        c
        (read-char-aux
         (if (eqv? c empty)
             (getchar)
             c)))))

(define (read-char-aux c)
  (field0-set! buffer c)
  (if (eqv? c eof)
      c
      (begin
        (field0-set! buffer empty)
        c)))

(define (peek-char)
  (let ((c (read-char)))
    (field0-set! buffer c)
    c))

;;;----------------------------------------------------------------------------

;; The read procedure.

(define (read)
  (let ((c (peek-char-non-whitespace)))
    (cond ((< c 0)
           c)
          ((eqv? c 40) ;; #\(
           (read-char) ;; skip "("
           (read-list))
          ((eqv? c 35) ;; #\#
           (read-char) ;; skip "#"
           (let ((c (peek-char)))
             (cond ((eqv? c 102) ;; #\f
                    (read-char) ;; skip "f"
                    #f)
                   ((eqv? c 116) ;; #\t
                    (read-char) ;; skip "t"
                    #t)
                   (else ;; assume it is #\(
                    (list->vector (read))))))
          ((eqv? c 39) ;; #\'
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
    (if (eqv? c 41) ;; #\)
        (begin
          (read-char) ;; skip ")"
          '())
        (let ((first (read)))
          (cons first (read-list))))))

(define (read-symbol)
  (let ((c (peek-char)))
    (if (or (eqv? c 40) ;; #\(
            (eqv? c 41) ;; #\)
            (< c 33)) ;; whitespace or eof?
        '()
        (begin
          (read-char)
          (cons c (read-symbol))))))

(define (peek-char-non-whitespace)
  (let ((c (peek-char)))
    (if (eof-object? c) ;; eof?
        (- 0 1)
        (if (< 32 c) ;; above #\space ?
            (if (eqv? c 59) ;; #\;
                (skip-comment)
                c)
            (begin
              (read-char)
              (peek-char-non-whitespace))))))

(define (skip-comment)
  (let ((c (read-char)))
    (if (< c 0) ;; eof?
        c
        (if (eqv? c 10) ;; #\newline
            (peek-char-non-whitespace)
            (skip-comment)))))

;;;----------------------------------------------------------------------------

;; The write procedure.

(define (write o)
  (cond ((not o)
         (putchar2 35 102)) ;; #f
        ((eqv? o #t)
         (putchar2 35 116)) ;; #t
        ((null? o)
         (putchar2 40 41)) ;; ()
        ((pair? o)
         (putchar 40)  ;; #\(
         (write (car o))
         (write-list (cdr o))
         (putchar 41)) ;; #\)
        ((string? o)
         (putchar 34)
         (write-chars (string->list o))
         (putchar 34))
        ((symbol? o)
         (write-chars (string->list (symbol->string o))))
        ((vector? o)
         (putchar 35) ;; #\#
         (write (vector->list o)))
        ((procedure? o)
         (putchar2 35 112) ;; #proc
         (putchar2 114 111)
         (putchar 99))
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
            #f)) ;; writing dotted pairs is not supported
      #f))

(define (write-chars lst)
  (if (pair? lst)
      (begin
        (putchar (car lst))
        (write-chars (cdr lst)))
      #f))

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

;; Compiler from Ribbit Scheme to uVM code.

(define jump/call-op 0)
(define set-op       1)
(define get-op       2)
(define const-op     3)
(define if-op        4)

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
                          (rib (length params)
                               0
                               ;#; ;; support for single expression in body
                               (comp (extend params
                                             (cons #f
                                                   (cons #f
                                                         cte)))
                                     (caddr expr)
                                     tail)
                               #; ;; support for multiple expressions in body
                               (comp-begin (extend params
                                                   (cons #f
                                                         (cons #f
                                                               cte)))
                                           (cddr expr)
                                           tail))
                          '())
                         (if (null? cte)
                             cont
                             (gen-call 'close cont)))))

#; ;; support for begin special form
                 ((eqv? first 'begin)
                  (comp-begin cte (cdr expr) cont))

#; ;; support for single armed let special form
                 ((eqv? first 'let)
                  (let ((binding (car (cadr expr))))
                    (comp-bind cte
                               (car binding)
                               (cadr binding)
                               (cddr expr)
                               cont)))

                 (else
                  ;#; ;; support for calls with only variable in operator position
                  (comp-call cte
                             (cdr expr)
                             (cons first cont))
                  #; ;; support for calls with any expression in operator position
                  (let ((args (cdr expr)))
                    (if (symbol? first)
                        (comp-call cte
                                   args
                                   (cons first cont))
                        (comp-bind cte
                                   '_
                                   first
                                   ;#; ;; support for single expression in body
                                   (cons '_ args)
                                   #; ;; support for multiple expressions in body
                                   (cons (cons '_ args) '())
                                   cont)))))))

        (else
         ;; self-evaluating
         (rib const-op expr cont))))

(define (comp-bind cte var expr body cont)
  (comp cte
        expr
        ;#; ;; support for single expression in body
        (comp (cons var cte)
              body
              (if (eqv? cont tail)
                  cont
                  (rib jump/call-op ;; call
                       'arg2
                       cont)))
        #; ;; support for multiple expressions in body
        (comp-begin (cons var cte)
                    body
                    (if (eqv? cont tail)
                        cont
                        (rib jump/call-op ;; call
                             'arg2
                             cont)))))

(define (comp-begin cte exprs cont)
  (comp cte
        (car exprs)
        (if (pair? (cdr exprs))
            (rib jump/call-op ;; call
                 'arg1
                 (comp-begin cte (cdr exprs) cont))
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

(define (comp-call cte exprs var-cont)
  (if (pair? exprs)
      (comp cte
            (car exprs)
            (comp-call (cons #f cte)
                       (cdr exprs)
                       var-cont))
      (let ((var (car var-cont)))
        (let ((cont (cdr var-cont)))
          (let ((v (lookup var cte 0)))
            (gen-call v cont))))))

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

(define (compile expr) ;; converts an s-expression to a procedure
  (make-procedure (rib 0 0 (comp '() expr tail)) '()))

(define (eval expr)
  ((compile expr)))

(define (repl)
  (putchar2 62 32) ;; #\> and space
  (let ((expr (read)))
    (if (eof-object? expr)
        #f
        (begin
          (write (eval expr))
          (newline)
          (repl)))))

;;(eval (cons 'define (cons 'f (cons (cons 'lambda (cons (cons 'x '()) (cons (cons 'if (cons (cons '< (cons 'x (cons 2 '()))) (cons 'x (cons (cons '+ (cons (cons 'f (cons (cons '- (cons 'x (cons 1 '()))) '())) (cons (cons 'f (cons (cons '- (cons 'x (cons 2 '()))) '())) '()))) '())))) '()))) '()))))
;;(eval (cons 'f (cons 25 '())))

;(write symtbl)
;;(eval (string->symbol (list->string '())))
;;(repl)

;;(write 42)

;;;----------------------------------------------------------------------------
