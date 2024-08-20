;; Ribbit Scheme runtime library.

;; This is the "min" version with only the core R4RS predefined procedures.

;;;----------------------------------------------------------------------------

;; Implementation of Ribbit Scheme types using the RVM operations.


(define rib         ##rib         ) ;; predefined by RVM (must be first and 0)
(define id          ##id          )
(define arg1        ##arg1        )
(define arg2        ##arg2        )
(define close       ##close       )
(define rib?        ##rib?        )
(define field0      ##field0      )
(define field1      ##field1      )
(define field2      ##field2      )
(define field0-set! ##field0-set! )
(define field1-set! ##field1-set! )
(define field2-set! ##field2-set! )
(define eqv?        ##eqv?        )
(define <           ##<           )
(define +           ##+           )
(define -           ##-           )
(define *           ##*           )
(define quotient    ##quotient    )
(define getchar     ##getchar     )
(define putchar     ##putchar     )
(define exit        ##exit        )

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (instance? type) (lambda (o) (and (rib? o) (eqv? (field2 o) type))))

;;;----------------------------------------------------------------------------

;; Booleans (R4RS section 6.1).

(define (not x) (eqv? x #f))

;;(define (boolean? obj) (or (eqv? obj #t) (not obj)))

;;;----------------------------------------------------------------------------

;; Equivalence predicates (R4RS section 6.2).

;;(define eq? eqv?)

(define (equal? x y)
  (or (eqv? x y)
      (and (rib? x)
           (if (eqv? (field2 x) singleton-type)
               #f
               (and (rib? y)
                    (equal? (field2 x) (field2 y))
                    (equal? (field1 x) (field1 y))
                    (equal? (field0 x) (field0 y)))))))

;;;----------------------------------------------------------------------------

;; Pairs and lists (R4RS section 6.3).

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

;;(define (caar pair) (field0 (field0 pair)))
;;(define (cadr pair) (field0 (field1 pair)))
;;(define (cdar pair) (field1 (field0 pair)))
;;(define (cddr pair) (field1 (field1 pair)))

;;(define (caaar pair) (caar (field0 pair)))
;;(define (caadr pair) (caar (field1 pair)))
;;(define (cadar pair) (cadr (field0 pair)))
;;(define (caddr pair) (cadr (field1 pair)))
;;(define (cdaar pair) (cdar (field0 pair)))
;;(define (cdadr pair) (cdar (field1 pair)))
;;(define (cddar pair) (cddr (field0 pair)))
;;(define (cdddr pair) (cddr (field1 pair)))

;;(define (caaaar pair) (caaar (field0 pair)))
;;(define (caaadr pair) (caaar (field1 pair)))
;;(define (caadar pair) (caadr (field0 pair)))
;;(define (caaddr pair) (caadr (field1 pair)))
;;(define (cadaar pair) (cadar (field0 pair)))
;;(define (cadadr pair) (cadar (field1 pair)))
;;(define (caddar pair) (caddr (field0 pair)))
;;(define (cadddr pair) (caddr (field1 pair)))
;;(define (cdaaar pair) (cdaar (field0 pair)))
;;(define (cdaadr pair) (cdaar (field1 pair)))
;;(define (cdadar pair) (cdadr (field0 pair)))
;;(define (cdaddr pair) (cdadr (field1 pair)))
;;(define (cddaar pair) (cddar (field0 pair)))
;;(define (cddadr pair) (cddar (field1 pair)))
;;(define (cdddar pair) (cdddr (field0 pair)))
;;(define (cddddr pair) (cdddr (field1 pair)))

(define (null? obj) (eqv? obj '()))

;;(define (list? obj)
;;  (list?-aux obj obj))

;;(define (list?-aux fast slow)
;;  (if (pair? fast)
;;      (let ((fast (cdr fast)))
;;        (cond ((eq? fast slow)
;;               #f)
;;              ((pair? fast)
;;               (list?-aux (cdr fast) (cdr slow)))
;;              (else
;;               (null? fast))))
;;      (null? fast)))

;;(define (list . args) args)

(define (length lst)
  (if (pair? lst)
      (+ 1 (length (cdr lst)))
      0))

;;(define (append lst1 lst2)
;;  (if (pair? lst1)
;;      (cons (car lst1) (append (cdr lst1) lst2))
;;      lst2))

;;(define (reverse lst)
;;  (reverse-aux lst '()))

;;(define (reverse-aux lst result)
;;  (if (pair? lst)
;;      (reverse-aux (cdr lst) (cons (car lst) result))
;;      result))

(define (list-ref lst i)
  (car (list-tail lst i)))

(define (list-set! lst i x)
  (set-car! (list-tail lst i) x))

(define (list-tail lst i)
  (if (< 0 i)
      (list-tail (cdr lst) (- i 1))
      lst))

;;(define (memv x lst)
;;  (if (pair? lst)
;;      (if (eqv? x (car lst))
;;          lst
;;          (memv x (cdr lst)))
;;      #f))

;;(define memq memv)

;;(define (member x lst)
;;  (if (pair? lst)
;;      (if (equal? x (car lst))
;;          lst
;;          (member x (cdr lst)))
;;      #f))

;;(define (assv x lst)
;;  (if (pair? lst)
;;      (let ((couple (car lst)))
;;        (if (eqv? x (car couple))
;;            couple
;;            (assv x (cdr lst))))
;;      #f))

;;(define assq assv)

;;(define (assoc x lst)
;;  (if (pair? lst)
;;      (let ((couple (car lst)))
;;        (if (equal? x (car couple))
;;            couple
;;            (assoc x (cdr lst))))
;;      #f))

(define (make-list k fill)
  (make-list-aux k fill '()))

(define (make-list-aux k fill lst)
  (if (< 0 k)
      (make-list-aux (- k 1) fill (cons fill lst))
      lst))

;;;----------------------------------------------------------------------------

;; Symbols (R4RS section 6.4).

(define symbol? (instance? symbol-type))
(define (string->uninterned-symbol str) (rib #f str symbol-type))
(define symbol->string field1)
(define global-var-ref field0)
(define global-var-set! field0-set!)

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

(define symtbl (field1 rib)) ;; get symbol table

(field1-set! rib 0) ;; release symbol table if not otherwise needed

;;;----------------------------------------------------------------------------

;; Numbers (R4RS section 6.5).

;;(define (integer? obj) (not (rib? obj)))

;;(define rational? integer?)
;;(define real? rational?)
;;(define complex? real?)
;;(define number? complex?)

;;(define (exact? obj) #t)
;;(define (inexact? obj) #f)

(define = eqv?)
;;(define (> x y) (< y x))
;;(define (<= x y) (not (< y x)))
;;(define (>= x y) (not (< x y)))

;;(define (zero? x) (eqv? x 0))
;;(define (positive? x) (< 0 x))
;;(define (negative? x) (< x 0))
;;(define (even? x) (eqv? x (* 2 (quotient x 2))))
;;(define (odd? x) (not (even? x)))

;;(define (max x y) (if (< x y) y x))
;;(define (min x y) (if (< x y) x y))

;;(define (abs x) (if (< x 0) (- 0 x) x))

;;(define (remainder x y)
;;  (- x (* y (quotient x y))))

;;(define (modulo x y)
;;  (let ((q (quotient x y)))
;;    (let ((r (- x (* y q))))
;;      (if (eqv? r 0)
;;          0
;;          (if (eqv? (< x 0) (< y 0))
;;              r
;;              (+ r y))))))

;;(define (gcd x y)
;;  (let ((ax (abs x)))
;;    (let ((ay (abs y)))
;;      (if (< ax ay)
;;          (gcd-aux ax ay)
;;          (gcd-aux ay ax)))))

;;(define (gcd-aux x y)
;;  (if (eqv? x 0)
;;      y
;;      (gcd-aux (remainder y x) x)))

;;(define (lcm x y)
;;  (if (eqv? y 0)
;;      0
;;      (let ((ax (abs x)))
;;        (let ((ay (abs y)))
;;          (* (quotient ax (gcd ax ay)) ay)))))

;;(define numerator id)
;;(define (denominator x) 1)

;;(define floor id)
;;(define ceiling id)
;;(define truncate id)
;;(define round id)

;;(define (rationalize x y) ...)
;;(define (exp x) ...)
;;(define (log x) ...)
;;(define (sin x) ...)
;;(define (cos x) ...)
;;(define (tan x) ...)
;;(define (asin x) ...)
;;(define (acos x) ...)
;;(define (atan y . x) ...)

;;(define (sqrt x) ...)

;;(define (expt x y)
;;  (if (eqv? y 0)
;;      1
;;      (let ((t (expt (* x x) (quotient y 2))))
;;        (if (odd? y)
;;            (* x t)
;;            t))))

;;(define (make-rectangular x y) ...)
;;(define (make-polar x y) ...)
;;(define (real-part x) ...)
;;(define (imag-part x) ...)
;;(define (magnitude x) ...)
;;(define (angle x) ...)

;;(define (exact->inexact x) ...)
;;(define (inexact->exact x) ...)

;; Integer to string conversion.

(define (number->string x)
  (list->string
   (if (< x 0)
       (cons 45 (number->string-aux (- 0 x) '()))
       (number->string-aux x '()))))

(define (number->string-aux x tail)
  (let ((q (quotient x 10)))
    (let ((d (+ 48 (- x (* q 10)))))
      (let ((t (cons d tail)))
        (if (< 0 q)
            (number->string-aux q t)
            t)))))

;; String to integer conversion.

(define (string->number str)
  (let ((lst (string->list str)))
    (if (null? lst)
        #f
        (if (eqv? (car lst) 45)
            (string->number-aux (cdr lst))
            (let ((n (string->number-aux lst)))
              (and n (- 0 n)))))))

(define (string->number-aux lst)
  (if (null? lst)
      #f
      (string->number-aux2 lst 0)))

(define (string->number-aux2 lst n)
  (if (pair? lst)
      (let ((c (car lst)))
        (and (< 47 c)
             (< c 58)
             (string->number-aux2 (cdr lst) (- (* 10 n) (- c 48)))))
      n))

;;;----------------------------------------------------------------------------

;; Characters (R4RS section 6.6).

;;(define char? integer?)

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

;; Strings (R4RS section 6.7).

(define string? (instance? string-type))
(define (list->string lst) (rib lst (length lst) string-type))
(define string->list field0)
(define string-length field1)
(define (string-ref str i) (list-ref (field0 str) i))
(define (string-set! str i x) (list-set! (field0 str) i x))

(define (make-string k) (list->string (make-list k 32)))

;;(define (string . args) ...)

;;(define (string=? str1 str2) (eqv? (string-cmp str1 str2) 0))
;;(define (string<? str1 str2) (< (string-cmp str1 str2) 0))
;;(define (string>? str1 str2) (< 0 (string-cmp str1 str2)))
;;(define (string<=? str1 str2) (not (string>? str1 str2)))
;;(define (string>=? str1 str2) (not (string<? str1 str2)))

;;(define string-ci=? string=?)
;;(define string-ci<? string<?)
;;(define string-ci>? string>?)
;;(define string-ci<=? string<=?)
;;(define string-ci>=? string>=?)

;;(define (string-cmp str1 str2)
;;  (string-cmp-aux (string->list str1) (string->list str2)))

;;(define (string-cmp-aux lst1 lst2)
;;  (if (pair? lst1)
;;      (if (pair? lst2)
;;          (let ((c1 (car lst1)))
;;            (let ((c2 (car lst2)))
;;              (if (< c1 c2)
;;                  -1
;;                  (if (< c2 c1)
;;                      1
;;                      (string-cmp-aux (cdr lst1) (cdr lst2))))))
;;          1)
;;      (if (pair? lst2)
;;          -1
;;          0)))

;;(define (substring str start end)
;;  (substring-aux str start end '()))

;;(define (substring-aux str start end tail)
;;  (if (< start end)
;;      (let ((i (- end 1)))
;;        (substring-aux str start i (cons (string-ref str i) tail)))
;;      (list->string tail)))

;;(define (string-append str1 str2)
;;  (list->string (append (string->list str1)
;;                        (string->list str2))))

;;(define (string-copy str)
;;  (list->string (append (string->list str) '())))

;;(define (string-fill! str fill)
;;  (field0-set! str (make-list (field1 str) fill)))

;;;----------------------------------------------------------------------------

;; Vectors (R4RS section 6.8).

(define vector? (instance? vector-type))
(define (list->vector lst) (rib lst (length lst) vector-type))
(define vector->list field0)
(define vector-length field1)
(define (vector-ref vect i) (list-ref (field0 vect) i))
(define (vector-set! vect i x) (list-set! (field0 vect) i x))

(define (make-vector k) (list->vector (make-list k 0)))

;;(define (vector . args) ...)

;;(define (vector-fill! vect fill)
;;  (field0-set! vect (make-list (field1 vect) fill)))

;;;----------------------------------------------------------------------------

;; Control features (R4RS section 6.9).

(define procedure? (instance? procedure-type))
(define (make-procedure code env) (rib code env procedure-type))
(define procedure-code field0)
(define procedure-env field1)

;;(define (apply proc . args) ...)

;;(define (map proc lst)
;;  (if (pair? lst)
;;      (cons (proc (car lst)) (map proc (cdr lst)))
;;      '()))

;;(define (for-each proc lst)
;;  (if (pair? lst)
;;      (begin
;;        (proc (car lst))
;;        (for-each proc (cdr lst)))
;;      #f))

;; First-class continuations.

(define (call/cc receiver)
  (let ((c (field1 (field1 (close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (field1 (field1 (close #f)))))
                  (field0-set! c2 (field0 c)) ;; set "stack" field
                  (field2-set! c2 (field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

;;;----------------------------------------------------------------------------

;; Input and output (R4RS section 6.10).

;;(define (call-with-input-file string proc) ...)
;;(define (call-with-output-file string proc) ...)
;;(define (input-port? obj) ...)
;;(define (output-port? obj) ...)
;;(define (current-input-port) ...)
;;(define (current-output-port) ...)
;;(define (with-input-from-file string thunk) ...)
;;(define (with-output-to-file string thunk) ...)
;;(define (open-input-file filename) ...)
;;(define (open-output-file filename) ...)
;;(define (close-input-port port) ...)
;;(define (close-output-port port) ...)
;;(define (char-ready?) ...)
;;(define (load filename) ...)
;;(define (transcript-on filename) ...)
;;(define (transcript-off) ...)

;; Character I/O (characters are represented with integers).

(define eof -1)
(define (eof-object? obj) (eqv? obj eof))

(define empty -2)
(define buffer empty)

(define (read-char)
  (let ((c buffer))
    (if (eqv? c eof)
        c
        (read-char-aux
         (if (eqv? c empty)
             (getchar)
             c)))))

(define (read-char-aux c)
  (set! buffer c)
  (if (eqv? c eof)
      c
      (begin
        (set! buffer empty)
        c)))

(define (peek-char)
  (let ((c (read-char)))
    (set! buffer c)
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
;#; \x and \X Support
;;                   ((or (eqv? c 120) (eqv? c 88)) ;; #\x or #\X
;;                    (read-char) ;; skip "x"
;;                    (let ((c (peek-char)))
;;                      (if (eqv? c 45)
;;                          (begin
;;                            (read-char)
;;                            (read-hex 0))
;;                          (- 0 (read-hex 0)))))
                   (else ;; assume it is #\(
                    (list->vector (read))))))
          ((eqv? c 39) ;; #\'
           (read-char) ;; skip "'"
           (cons 'quote (cons (read) '())))
;;          ((eqv? c 34) ;; #\"
;;           (read-char) ;; skip """
;;           (list->string (read-chars '())))
          (else
           (read-char) ;; skip first char
           (let ((s (list->string (cons c (read-symbol)))))
             (let ((n (string->number s)))
               (or n
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

;;(define (read-chars lst)
;;  (let ((c (read-char)))
;;    (cond ((eof-object? c)
;;           '())
;;          ((eqv? c 34) ;; #\"
;;           (reverse lst))
;;          ((eqv? c 92) ;; #\\
;;           #; ;; no support for \n in strings
;;           (read-chars (cons (read-char) lst))
;;           ;#; ;; support for \n in strings
;;           (let ((c2 (read-char)))
;;             (read-chars
;;              (cons (cond
;;                     ;#; ;; support for \n in strings
;;                     ((eqv? c2 110) 10) ;; #\n
;;                     ;#; ;; support for \r in strings
;;                     ((eqv? c2 114) 13) ;; #\r
;;                     ;#; ;; support for \t in strings
;;                     ((eqv? c2 116) 9)  ;; #\t
;;                     (else          c2))
;;                    lst))))
;;          (else
;;           (read-chars (cons c lst))))))

(define (peek-char-non-whitespace)
  (let ((c (peek-char)))
    (if (eof-object? c) ;; eof?
        -1
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
  (cond ((string? o)
         (putchar 34)
         (write-chars (string->list o))
         (putchar 34))
        (else
         (display o))))

(define (display o)
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
        ((symbol? o)
         (display (symbol->string o)))
        ((string? o)
         (write-chars (string->list o)))
;;        ((vector? o)
;;         (putchar 35) ;; #\#
;;         (write (vector->list o)))
        ((procedure? o)
         (putchar2 35 112)) ;; #p
        (else
         ;; must be a number
         (display (number->string o)))))

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

;;(define (write-chars lst escape?)
;;  (if (pair? lst)
;;      (let ((c (car lst)))
;;        (putchar
;;         (cond ((not escape?)
;;                c)
;;               ;#; ;; support for \n in strings
;;               ((eqv? c 10) ;; #\newline
;;                (putchar 92) ;; #\\
;;                110)         ;; #\n
;;               ;#; ;; support for \r in strings
;;               ((eqv? c 13) ;; #\return
;;                (putchar 92) ;; #\\
;;                114)         ;; #\r
;;               ;#; ;; support for \t in strings
;;               ((eqv? c 9) ;; #\tab
;;                (putchar 92) ;; #\\
;;                116)         ;; #\t
;;               ((or (eqv? c 34) ;; #\"
;;                    (eqv? c 92)) ;; #\\
;;                (putchar 92) ;; #\\
;;                c)
;;               (else
;;                c)))
;;        (write-chars (cdr lst) escape?))
;;      #f))


(define (write-chars lst)
  (if (pair? lst)
      (let ((c (car lst)))
        (putchar c)
        (write-chars (cdr lst)))
      #f))

(define write-char putchar)

(define (newline)
  (putchar 10))

(define (putchar2 c1 c2)
  (putchar c1)
  (putchar c2))

;;;----------------------------------------------------------------------------

;; Compiler from Ribbit Scheme to RVM code.

(define jump/call-op 0)
(define set-op       1)
(define get-op       2)
(define const-op     3)
(define if-op        4)

(define (add-nb-args nb tail)
  (if-feature arity-check
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
                               #; ;; support for single expression in body
                               (comp (extend params
                                             (cons #f
                                                   (cons #f
                                                         cte)))
                                     (caddr expr)
                                     tail))
;;                               #; ;; support for multiple expressions in body
;;                               (comp-begin (extend params
;;                                                   (cons #f
;;                                                         (cons #f
;;                                                               cte)))
;;                                           (cddr expr)
;;                                           tail))
                          '())
                         (if (null? cte)
                             cont
                             (add-nb-args
                               1
                               (gen-call '##close cont))))))

;;;#; ;; support for begin special form
;;                 ((eqv? first 'begin)
;;                  (comp-begin cte (cdr expr) cont))
;;
;;;#; ;; support for single armed let special form
;;                 ((eqv? first 'let)
;;                  (let ((binding (car (cadr expr))))
;;                    (comp-bind cte
;;                               (car binding)
;;                               (cadr binding)
;;;;                               #; ;; support for single expression in body
;;;;                               (caddr expr)
;;                               ;#; ;; support for multiple expressions in body
;;                               (cddr expr)
;;                               cont)))
;;
;;;#; ;; support for and special form
;;                 ((eqv? first 'and)
;;                  (comp cte
;;                        (if (pair? (cdr expr))
;;                            (let ((second (cadr expr)))
;;                              (if (pair? (cddr expr))
;;                                  (build-if second
;;                                            (cons 'and (cddr expr))
;;                                            #f)
;;                                  second))
;;                            #t)
;;                        cont))
;;
;;;#; ;; support for or special form
;;                 ((eqv? first 'or)
;;                  (comp cte
;;                        (if (pair? (cdr expr))
;;                            (let ((second (cadr expr)))
;;                              (if (pair? (cddr expr))
;;                                  (list3 'let
;;                                         (list1 (list2 '_ second))
;;                                         (build-if '_
;;                                                   '_
;;                                                   (cons 'or (cddr expr))))
;;                                  second))
;;                            #f)
;;                        cont))
;;
;;;#; ;; support for cond special form
;;                 ((eqv? first 'cond)
;;                  (comp cte
;;                        (if (pair? (cdr expr))
;;                            (if (eqv? 'else (car (cadr expr)))
;;                                (cons 'begin (cdr (cadr expr)))
;;                                (build-if (car (cadr expr))
;;                                          (cons 'begin (cdr (cadr expr)))
;;                                          (cons 'cond (cddr expr))))
;;                            #f)
;;                        cont))

                 (else
                 ;#; ;; support for calls with only variable in operator position
                 (let ((args (cdr expr)))
                   (comp-call cte
                              args
                              (length args)
                              (cons first cont)))
;;                  ;#; ;; support for calls with any expression in operator position
;;                  (let ((args (cdr expr)))
;;                    (if (symbol? first)
;;                        (comp-call cte
;;                                   args
;;                                   (length args)
;;                                   (cons first cont))
;;                        (comp-bind cte
;;                                   '_
;;                                   first
;;;;                                   #; ;; support for single expression in body
;;;;                                   (cons '_ args)
;;                                   ;#; ;; support for multiple expressions in body
;;                                   (cons (cons '_ args) '())
;;                                   cont)))
))))

        (else
         ;; self-evaluating
         (rib const-op expr cont))))

;#; ;; support for and, or, cond special forms
;;(define (build-if a b c) (cons 'if (list3 a b c)))
;;(define (list3 a b c) (cons a (list2 b c)))
;;(define (list2 a b) (cons a (list1 b)))
;;(define (list1 a) (cons a '()))
;;(define (list . args) args)

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
                               '##arg2
                                cont))))))

(define (comp-begin cte exprs cont)
  (comp cte
        (car exprs)
        (if (pair? (cdr exprs))
          (add-nb-args
            2
            (rib jump/call-op ;; call
                 '##arg1
                 (comp-begin cte (cdr exprs) cont)))
            cont)))

(define (gen-call v cont)
  (if (eqv? cont tail)
      (rib jump/call-op v 0)      ;; jump
      (rib jump/call-op v cont))) ;; call

(define (gen-assign v cont)
  (rib set-op v (gen-noop cont)))

(define (is-call? name cont)
  (and (rib? cont)
       (if-feature arity-check
           (and (eqv? (field0 cont) const-op)
                (rib? (field2 cont))
                (eqv? (field0 (field2 cont)) jump/call-op)
                (eqv? (field1 (field2 cont)) name))
           (and (eqv? (field0 cont) jump/call-op)
                (eqv? (field1 cont) name)))))

(define (gen-noop cont)
  (if (is-call? '##arg1 cont)
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
              (gen-call
                (if-feature arity-check
                  (if (not (rib? v)) (+ v 1) v)
                  v)
                cont)))))))

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

(define tail (add-nb-args 1 (rib jump/call-op '##id 0))) ;; jump

(define (compile expr) ;; converts an s-expression to a procedure
  (make-procedure (rib 0 0 (comp '() expr tail)) '()))

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
    base))

(define (error msg info)
  (display msg)
  (display " ")
  (write info)
  (newline)
  (exit 1))

;;;----------------------------------------------------------------------------
