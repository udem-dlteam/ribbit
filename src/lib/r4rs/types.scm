(##include-once (ribbit "expander-utils"))
(##include-once "./bool.scm")


(define-const pair-type      0)
(define-const procedure-type 1)
(define-const symbol-type    2)
(define-const string-type    3)
(define-const vector-type    4)
(define-const singleton-type 5)
(define-const char-type      6)

(define-const input-port-type 8)
(define-const output-port-type 9)


(if-feature hybrid
(define-primitive (##rc-rib x y z)
  "{
  obj z = CAR(stack);
    obj y = CAR(CDR(stack));
    obj x = CAR(CDR(CDR(stack)));
    obj r = TAG_RIB(alloc_rib(x, y, z, 1));
    set_stack(CDR(CDR(CDR(stack))));
    push(r);
break;
}
")
(define ##rc-rib ##rib))

(define (rc-list-copy lst)
  (let loop ((x lst) )
    (if (null? x)
	'()
	(##rc-rib (car x) (loop (cdr x)) pair-type))))

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


(define (length lst)
  (if (pair? lst)
      (##+ 1 (length (##field1 lst)))
      0))

;; ---------------------- CONVERSIONS ---------------------- ;;

(if-feature 
  v-port
  (define (object->string o)
    (let ((str-port (open-output-string)))
      (write o str-port)
      (get-output-string str-port)))
  (begin))

(define (char->integer x) (##field0 x))
(define (integer->char n) (##rib n 0 char-type))

(define (##list->string lst) (##rc-rib (rc-list-copy lst) (length lst) string-type))
(define (##string->list x) (##field0 x))

(define (##map proc lst)
  (if (pair? lst)
    (##rib (proc (##field0 lst)) (##map proc (##field1 lst)) pair-type)
    '()))

(define (list->string lst) (##list->string (##map char->integer lst)))
(define (string->list s) (##map integer->char (##string->list s)))

(define (list->vector lst) (##rib lst (length lst) vector-type))
(define (vector->list x) (##field0 x))

(define (symbol->string x) (##field1 x))

(define symtbl (##field1 ##rib)) ;; get symbol table

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
