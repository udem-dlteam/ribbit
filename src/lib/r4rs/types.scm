(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)
(define char-type 5)

(define (instance? type) (lambda (o) (and (rib? o) (eqv? (field2 o) type))))

(define pair? (instance? pair-type))
(define symbol? (instance? symbol-type))
(define string? (instance? string-type))
(define vector? (instance? vector-type))
(define procedure? (instance? procedure-type))
(define char? (instance? char-type))

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


(define char->integer (if ##feature-chars field0 id))
(define integer->char (if ##feature-chars (lamdba (x) (rib x 0 char-type)) id))

(define (##list->string lst) (rib lst (length lst) string-type))
(define ##string->list field0)

(define list->string (if ##feature-chars (lambda (lst) (##list->string (map integer->char lst))) ##list->string))
(define string->list (if ##feature-chars (lambda (str) (map integer->char (##string->list str))) ##string->list))

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
        (let ((t (rib (if (< 9 d) (+ 65 (- d 10)) (+ 48 d)) tail 0))) ;; cons
          (if (< 0 q)
            (number->string-aux q t)
            t)))))

  (list->string
    (if (< x 0)
      (rib 45 (number->string-aux (- 0 x) '()) 0) ;; cons
      (number->string-aux x '()))))


(define (string->number str (radix 10))

  (define (string->number-aux lst)
    (if (null? lst)
      #f
      (string->number-aux2 lst 0)))

  (define (string->number-aux2 lst n)
    (if (pair? lst)
      (let* ((c (field0 lst))
             (x (cond 
                  ((and (< 47 c) (< c 58)) (- c 48))   ;; 0-9
                  ((and (< 64 c) (< c 71)) (- c 65))   ;; A-F
                  ((and (< 96 c) (< c 103)) (- c 97))  ;; a-f
                  (else #f)))) ;; car
        (if x
            (string->number-aux2 
              (field1 lst) ;; cdr
              (- (* radix n) x))
            #f))
        n))

  (let ((lst (string->list str)))
    (if (null? lst)
      #f
      (if (eqv? (field0 lst) 45) ;; car
        (string->number-aux (field1 lst)) ;; cdr
        (let ((n (string->number-aux lst)))
          (and n (- 0 n)))))))
