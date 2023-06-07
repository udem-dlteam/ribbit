(##include-once "./bool.scm")

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)
(define char-type      6)

(define (instance? type) (lambda (o) (and (rib? o) (eqv? (field2 o) type))))

(define pair? (instance? pair-type))
(define symbol? (instance? symbol-type))
(define string? (instance? string-type))
(define vector? (instance? vector-type))
(define procedure? (instance? procedure-type))
(define char? (instance? char-type))

(set! ##eqv? eqv?)

(define (eqv? o1 o2)
  (cond ((and (char? o1) (char? o2)) (##eqv? (field0 o1) (field0 o2)))
        (else (##eqv? o1 o2))))

(define (null? obj) (eqv? obj '()))

(define (integer? obj) (eqv? (rib? obj) #f))

(define (list? obj)

  (define (list?-aux fast slow)
    (if (pair? fast)
      (let ((fast (field1 fast)))
        (cond ((eqv? fast slow)
               #f)
              ((pair? fast)
               (list?-aux (field1 fast) (field1 slow)))
              (else
                (null? fast))))
      (null? fast)))

  (list?-aux obj obj))


(define (length lst)
  (if (pair? lst)
      (+ 1 (length (field1 lst)))
      0))

;; ---------------------- CONVERSIONS ---------------------- ;;


(define char->integer field0)
(define (integer->char n) (rib n 0 char-type))

(define (##list->string lst) (rib lst (length lst) string-type))
(define ##string->list field0)

(define (##map proc lst)
  (if (pair? lst)
    (rib (proc (field0 lst)) (##map proc (field1 lst)) pair-type)
    '()))

(define (list->string lst) (##list->string (##map char->integer lst)))
(define (string->list s) (##map integer->char (##string->list s)))

(define (list->vector lst) (rib lst (length lst) vector-type))
(define vector->list field0)

(define symbol->string field1)

(define (string->symbol str)

  (define (string->symbol-aux str syms)
    (if (pair? syms)
      (let ((sym (field0 syms)))
        (if (equal? (field1 sym) str)
          sym
          (string->symbol-aux str (field1 syms))))
      (let ((sym (string->uninterned-symbol str)))
        (set! symtbl (rib sym symtbl pair-type)) ;; cons
        sym)))

  (string->symbol-aux str symtbl))

(define (string->uninterned-symbol str) (rib #f str symbol-type))

(define symtbl (field1 rib)) ;; get symbol table

(define (number->string x (radix 10))
  (define (number->string-aux x tail)
    (let ((q (quotient x radix)))
      (let ((d (- x (* q radix))))
        (let ((t (rib (if (< 9 d) (+ 65 (- d 10)) (+ 48 d)) tail pair-type))) ;; cons
          (if (< 0 q)
            (number->string-aux q t)
            t)))))

  (let ((chars (if (< x 0)
                 (rib 45 (number->string-aux (- 0 x) '()) pair-type) ;; cons
                 (number->string-aux x '()))))
    (rib
      chars 
      (length chars)
      string-type)))


(define (string->number str (radix 10))

  (define (convert-16 c)
    (cond 
      ((and (< 47 c) (< c 58)) (- c 48))   ;; 0-9
      ((and (< 64 c) (< c 71)) (- c 65))   ;; A-F
      ((and (< 96 c) (< c 103)) (- c 97))  ;; a-f
      (else #f)))

  (define (convert c)
    (if (and (< 47 c) (< c 58))
      (- c 48)   ;; 0-9
      #f))

  (define (string->number-aux lst)
    (if (null? lst)
      #f
      (string->number-aux2 lst 0 (if (eqv? radix 16) convert-16 convert))))

  (define (string->number-aux2 lst n converter)
    (if (pair? lst)
      (let* ((c (field0 lst))
             (x (converter c)))
        (if x
            (string->number-aux2 
              (field1 lst) ;; cdr
              (- (* radix n) x)
              converter)
            #f))
        n))

  (let ((lst (##string->list str)))
    (if (null? lst)
      #f
      (if (eqv? (field0 lst) 45) ;; car
        (string->number-aux (field1 lst)) ;; cdr
        (let ((n (string->number-aux lst)))
          (and n (- 0 n)))))))
