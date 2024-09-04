;;; File: fixnum.scm

(##include-once (ribbit "prim-fx"))

;;==============================================================================

;; Numerical operations

;;------------------------------------------------------------------------------

;; Numerical predicates


;; Numerical types predicates

;; number? => nums.scm
;; complex? => nums.scm
;; real? => nums.scm
;; rational? => nums.scm
;; integer? => nums.scm


;; Exactness predicates

;; exact? => nums.scm
;; inexact? => nums.scm


;; Comparison predicates

;; =
;; <
;; >
;; <=
;; >=


;; Numerical properties predicates

;; zero?
;; positive?
;; negative?
;; odd?

(define (##fx-even? x) (##eqv? x (##* 2 (##quotient x 2))))

;; Max and min

(define (##fx-max a b) (if (##< a b) b a))

(define (##fx-min a b) (if (##< a b) a b))


;; Arithmetic operations: +, *, -, /

;; (+ z1 . . . ) procedure
;; (* z1 . . . ) procedure

;; (- z) procedure
;; (- z1 z2 . . . ) procedure
;; (/ z) procedure
;; (/ z1 z2 . . . ) procedure



;; Absolute value

(define (##fx-abs a) (if (##< a 0) (##- 0 a) a))


;; Quotient, remainder and modulo

;; quotient

(define (##fx-remainder x y)
  (##- x (##* y (##quotient x y))))

(define (##fx-modulo x y)
  (let ((q (##quotient x y)))
    (let ((r (##- x (##* y q))))
      (if (##eqv? r 0)
          0
          (if (##eqv? (##< x 0) (##< y 0))
              r
              (##+ r y))))))

;; Gcd and lcm

(define (##fx-gcd a b)
  (let ((_a (abs a))
        (_b (abs b)))
    (if (##< _a _b) (##fx-gcd-aux _a _b) (##fx-gcd-aux _b _a))))

(define (##fx-gcd-aux a b)
  (if (##eqv? a 0)
      b
      (##fx-gcd-aux (##fx-remainder b a) a)))


(define (##fx-lcm a b)
  (if (##eqv? b 0)
      0
      (let ((_a (abs a))
            (_b (abs b)))
        (##* (##quotient _a (##fx-gcd _b)) _b))))

;; Floor, ceiling, truncate, round

;;


;;==============================================================================

;; Numerical input and output


(define (##fx-string->number str (radix 10))

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

  (define (##fx-string->number-aux lst)
    (if (null? lst) ;; FIXME: remove the null? check
      #f
      (##fx-string->number-aux2 lst 0 (if (##eqv? radix 16) convert-16 convert))))

  (define (##fx-string->number-aux2 lst n converter)
    (if (pair? lst)
      (let* ((c (##field0 lst))
             (x (converter c)))
        (if x
            (##fx-string->number-aux2 
              (##field1 lst) ;; cdr
              (##+ (##* radix n) x)
              converter)
            #f))
        n))

  (let ((lst (##field0 str)))
    (if (null? lst)
      #f
      (if (##eqv? (##field0 lst) 45) ;; car
        (let ((n (##fx-string->number-aux (##field1 lst))))
          (and n (##- 0 n)))
        (##fx-string->number-aux lst))))) ;; cdr


(define (##fx-number->string a)

  (define (##number->string-aux _a tail radix)
    (let* ((quo (##quotient _a radix))
           (rem (##fx-remainder _a radix))
           (chars (##rib (##+ 48 rem) tail pair-type)))
      (if (##eqv? 0 quo)
          chars
          (##number->string-aux quo chars radix))))

  (let ((chars (if (##< a 0)
                   (##rib 45
                          (##number->string-aux (##- 0 a) '() 10)
                          pair-type)
                   (##number->string-aux a '() 10))))

    (##rib chars (length chars) string-type))) ;; FIXME ##length?
