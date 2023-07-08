(##include-once "./bool.scm")

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)
(define char-type      6)

(define input-port-type 8)
(define output-port-type 9)


(define (type-of o)
  (cond 
    ((number? o) 'number)
    ((list? o) 'list)
    ((pair? o) 'pair)
    ((procedure? o) 'procedure)
    ((symbol? o) 'symbol)
    ((string? o) 'string)
    ((vector? o) 'vector)
    ((char? o) 'char)
    ((boolean? o) 'boolean)
    ((input-port? o) 'input-port)
    ((output-port? o) 'output-port)
    (else 'unknown)))


(define (instance? type) (lambda (o) (and (##rib? o) (##eqv? (##field2 o) type))))

(define pair? (instance? pair-type))
(define symbol? (instance? symbol-type))
(define string? (instance? string-type))
(define vector? (instance? vector-type))
(define procedure? (instance? procedure-type))
(define char? (instance? char-type))
(define (boolean? o1) (or (##eqv? o1 #t) (##eqv? o1 #f)))

(define input-port? (instance? input-port-type))
(define output-port? (instance? output-port-type))
  

(define (eqv? o1 o2)
  (if (and (char? o1) (char? o2)) 
    (##eqv? (##field0 o1) (##field0 o2))
    (##eqv? o1 o2)))


(define (eq? o1 o2) (##eqv? o1 o2))

(define (null? obj) (##eqv? obj '()))

(define (integer? obj) (not (##rib? obj)))
(define number? integer?)
(define rational? integer?)
(define real? rational?)
(define complex? real?)

(define (list? obj)

  (define (list?-aux fast slow)
    (if (pair? fast)
      (let ((fast (##field1 fast)))
        (cond ((##eqv? fast slow)
               #f)
              ((pair? fast)
               (list?-aux (##field1 fast) (##field1 slow)))
              (else
                (null? fast))))
      (null? fast)))

  (list?-aux obj obj))


;; ---------------------- CONVERSIONS ---------------------- ;;

;;; Warning: You need to include v-io.scm for this procedure to work
(define (object->string o)
  (let ((str-port (open-output-string)))
    (write o str-port)
    (get-output-string str-port)))

(define (char->integer x) (##field0 x))

(define-signature
  char->integer 
  ((x
     guard: (char? x)
     expected: "CHARACTER")))



(define (integer->char n) (##rib n 0 char-type))

(define-signature
  integer->char 
  ((n
     guard: (integer? n)
     expected: "INTEGER")))



(define (##list->string lst) (##rib lst (length lst) string-type))
(define (##string->list x) (##field0 x))

(define (##map proc lst)
  (if (pair? lst)
    (##rib (proc (##field0 lst)) (##map proc (##field1 lst)) pair-type)
    '()))

(define (list->string lst) (##list->string (##map char->integer lst)))
(define (list->vector lst) (##rib lst (length lst) vector-type))

(define-signatures
  (list->string list->vector)
  ((lst 
     guard: (list? lst)
     expected: "LIST")))



(define (string->list str) (##map integer->char (##string->list str)))

(define (string->symbol str)

  (define (string->symbol-aux str syms)
    (if (pair? syms)
      (let ((sym (##field0 syms)))
        (if (equal? (##field1 sym) str)
          sym
          (string->symbol-aux str (##field1 syms))))
      (let ((sym (string->uninterned-symbol str)))
        (set! symtbl (##rib sym symtbl pair-type)) ;; cons
        sym)))

  (string->symbol-aux str symtbl))

(define (string->uninterned-symbol str) (##rib #f (string-append str) symbol-type))

(define symtbl (##field1 ##rib)) ;; get symbol table

(define-signatures
  (string->list string->symbol)
  ((str 
     guard: (string? str)
     expected: "STRING")))


(define (vector->list x) (##field0 x))

(define-signature
  vector->list 
  ((x
     guard: (vector? x)
     expected: "VECTOR")))
 


(define (symbol->string x) (##field1 x))

(define-signature
  symbol->string 
  ((x
     guard: (symbol? x)
     expected: "SYMBOL")))



(define (number->string x (radix 10))
  (define (number->string-aux x tail)
    (let ((q (##quotient x radix)))
      (let ((d (##- x (##* q radix))))
        (let ((t (##rib (if (##< 9 d) (##+ 65 (##- d 10)) (##+ 48 d)) tail pair-type))) ;; cons
          (if (##< 0 q)
            (number->string-aux q t)
            t)))))

  (let ((chars (if (##< x 0)
                 (##rib 45 (number->string-aux (##- 0 x) '()) pair-type) ;; cons
                 (number->string-aux x '()))))
    (##rib
      chars 
      (length chars)
      string-type)))


(define-signature 
  number->string 
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (radix 
     default: 10
     guard: (memv radix '(2 8 10 16))
     expected: "Either 2, 8, 10, or 16")))



(define (string->number str (radix 10))

  (define (convert-16 c)
    (cond 
      ((and (##< 47 c) (##< c 58)) (##- c 48))   ;; 0-9
      ((and (##< 64 c) (##< c 71)) (##- c 65))   ;; A-F
      ((and (##< 96 c) (##< c 103)) (##- c 97))  ;; a-f
      (else #f)))

  (define (convert c)
    (if (and (##< 47 c) (##< c 58)) ;; 0-9
      (##- c 48)   
      #f))

  (define (string->number-aux lst)
    (if (null? lst) ;; FIXME: remove the null? check
      #f
      (string->number-aux2 lst 0 (if (##eqv? radix 16) convert-16 convert))))

  (define (string->number-aux2 lst n converter)
    (if (pair? lst)
      (let* ((c (##field0 lst))
             (x (converter c)))
        (if x
            (string->number-aux2 
              (##field1 lst) ;; cdr
              (##+ (##* radix n) x)
              converter)
            #f))
        n))

  (let ((lst (##field0 str)))
    (if (null? lst)
      #f
      (if (##eqv? (##field0 lst) 45) ;; car
        (let ((n (string->number-aux (##field1 lst))))
          (and n (##- 0 n)))
        (string->number-aux lst))))) ;; cdr


(define-signature 
  string->number 
  ((str
     guard: (string? str)
     expected: "STRING")
   (radix 
     default: 10
     guard: (memv radix '(2 8 10 16))
     expected: "Either 2, 8, 10, or 16")))
