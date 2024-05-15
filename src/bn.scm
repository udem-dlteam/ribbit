;;; bn.scm

;; bignums operations (two's complement, little endian representation)

;; second `x` is for the variadic version of the procedure

;; `x+t` means that the procedure is "fully" tested

;; addition (bn+)              [x+t] [x] logic works but needs some cleanup
;; substraction (bn-)          [x+t] [x]
;; unary substraction (bn-u)   [x+t]

;; multiplication (bn*)        [x+t] [x] need a better algorithm 

;; quotient (bn-quotient)      [x+t]     need a better algorithm
;; remainder (bn-remainder)    [x+t]
;; modulo (bn-modulo)          [x+t]

;; equal (bn=)                 [x+t]
;; less (bn<)                  [x+t]
;; less or equal (bn<=)        [x+t]
;; greater (bn>)               [x+t]
;; greater or equal (bn>=)     [x+t]

;; negation (bn-neg)           [x+t]

;; bitwise not (bn~)           [ ]     <- need to implement bitwise operations
;; bitwise and (bn-and)        [ ]        for this to work in ribbit
;; bitwise or (bn-ior)         [ ] 
;; bitwise xor (bn-xor)        [ ]
;; shl                         [ ]
;; shift-left                  [ ]
;; shr                         [ ]
;; shift-right                 [ ]

;; bn-zero?                    [x]
;; bn-positive?                [x]
;; bn-negative?                [x]
;; bn-max                      [x]
;; bn-min                      [x]
;; bn-abs                      [x]
;; bn-gcd                      [x]
;; bn-lcm                      [x]


;; TODO stress tests for multiplication and quotient



;; TODO
;;  - bitwise operations + tests
;;  - eq? and equal? + normalisation
;;  - cond-expand
;;  - remove tabs
;;  - test with ribbit


;;------------------------------------------------------------------------------

;; utilities


;; (define base (expt 2 15))

(define base (expt 2 3))

;; (define base 2) ;; for testing

;; (cond-expand
 
;;  (gambit
  
;;   (define (rib field0 field1 field2)
;;     (let ((r (make-vector 3)))
;;       (vector-set! r 0 field0)
;;       (vector-set! r 1 field1)
;;       (vector-set! r 2 field2)
;;       r))

;;   (define (rib? o) (vector? o))
;;   (define (field0 o) (vector-ref o 0))
;;   (define (field1 o) (vector-ref o 1))
;;   (define (field2 o) (vector-ref o 2))
;;   (define (field0-set! o x) (vector-set! o 0 x) o)
;;   (define (field1-set! o x) (vector-set! o 1 x) o)
;;   (define (field2-set! o x) (vector-set! o 2 x) o)
  
;;   ))


;; (define bn0 (rib 0 '_ 0))
;; (field1-set! bn0 bn0)

;; (define bn-1 (rib (- base 1) '_ 0))
;; (field1-set! bn-1 bn-1)

;; (define (car lst) (field0 lst))
;; (define (cdr lst) (field1 lst))
;; (define (cons a b) (rib a b 0))



;; for ribbit only
(define (set-cdr! lst x)                
  (##field1-set! lst x))




(define bn0 (cons 0 '_))
(set-cdr! bn0 bn0)

(define bn-1 (cons (- base 1) '_))
(set-cdr! bn-1 bn-1)

(define (end? n)
  (or (eq? n bn0) (eq? n bn-1)))

;; (define (end? n)
;;   (or (equal? n bn0) (equal? n bn-1)))

(define (_pp lst)
  (define (__pp lst)
    (if (or (eq? lst bn0) (eq? lst bn-1))
     (begin
       (display (if (eq? lst bn0) "$0 $0 ...)" "$-1 $-1 ...)"))
       (newline))
     (begin
       (display (car lst))
       (display " ")
       (__pp (cdr lst)))))
  (display "(")
  (__pp lst))


;; (define (_pp lst)
;;   (define (__pp lst)
;;     (if (or (eq? lst bn0) (eq? lst bn-1))
;;         (begin
;;           (display (if (eq? lst bn0) "#($0 #($0 ..." "#($-1 #($-1 ..."))
;;           (newline))
;;         (begin
;;           (display "#(")
;;           (display (car lst))
;;           (display " ")
;;           (__pp (cdr lst)))))
;;   (__pp lst))

(define (bn-fold func base lst)
  (if (pair? lst)
      (bn-fold func (func (car lst) base) (cdr lst))
      base))

;;------------------------------------------------------------------------------

;; arithmetic


;; possible cases for addition 

;; - parity(a) != parity(b) => no "overflow" possible 
;;    - carry => result will be positive => bn0
;;    - no carry => result will be negative => bn-1

;; - parity(a) == parity(b) => possible "overflow"
;;    - carry => add carry and then same as below 
;;    - no carry => result will have the same parity as both a and b => parity(a)
;;        (intuition: sum of pos (neg) numbers will always be pos (neg))

(define (bn+ a b)
  (define (_bn+ a b carry)
    (if (and (end? a) (end? b))
        (if (eq? a b)
            (if (eq? 0 carry)
                (if (eq? a bn0) bn0 bn-1) ;;(cons 0 bn-1))
                (cons carry (_bn+ (cdr a) (cdr b) 0)))
            (if (eq? 0 carry) bn-1 bn0))
        (let* ((_a (car a))
               (_b (car b))
               (res (+ _a _b carry))
               (rem (modulo res base))
               (quo (quotient res base)))
          (cons rem (_bn+ (cdr a) (cdr b) quo)))))
  ;; (_bn+ a b 0))
  (if (and (eq? a bn-1) (eq? b bn-1)) ;; small patch, need to rework that
      (cons 0 bn-1)
      (_bn+ a b 0)))

(define (var-bn+ . args)
  (bn-fold (lambda (a b) (bn+ a b)) bn0 args))

(define (bn- a b)
  (bn+ a (bn-neg b)))

(define (var-bn- . args)
  (bn-fold (lambda (a b) (bn- a b)) bn0 args))

(define (bn-u a)
  (bn- bn0 a))

(define (bn-remainder a b)
  (bn- a (bn* b (bn-quotient a b))))

(define (bn-modulo a b)
  (let ((r (bn-remainder a b)))
    (if (eq? r bn0)
        bn0
        (if (eqv? (bn< a bn0) (bn< b bn0))
            r
            (bn+ r b)))))


;;------------------------------------------------------------------------------

;; multiplication (need a better algorithm)


;; long multiplication in O(n^2) so that we have something that works for now

(define (bn* a b)
  
  (define (_bn* a b carry)  ;; one line multiplication
    (if (end? a)
        (if (eq? 0 carry) bn0 (cons carry bn0))
        (let* ((_a (car a))
               (_b (car b))
               (res (+ (* _a _b) carry))
               (rem (modulo res base))
               (quo (quotient res base)))
          (cons rem (_bn* (cdr a) b quo)))))
  
  (define (__bn* a b) ;; full (positive) mutiplication
    (if (end? b)
        bn0 
        (let ((res (_bn* a b 0)) 
              (pad (cons 0 a))) 
          (bn+ res (__bn* pad (cdr b))))))
  
  (if (eqv? (bn< a bn0) (bn< b bn0)) 
      (__bn* (bn-abs a) (bn-abs b))
      (bn-neg (__bn* (bn-abs a) (bn-abs b)))))

(define (var-bn* . args)
  (bn-fold (lambda (a b) (bn* a b)) (cons 1 bn0) args))


;; faster algorithms

;;  - Booth's multiplication algorithm (specifically for two's complement)

;;  - Karatsuba O(n^(ln 3)) ≈ O(n^1.585)

;;  - Schönhage–Strassen based on fft O(n log(n) log(log(n)))

;;  - Tom-Cook (or Tom-3)


;;------------------------------------------------------------------------------

;; quotient (need a better algorithm)

;; FIXME currently VERY VERY slow 

(define (bn-quotient a b)
  (define (_bn-quotient a b quo)
    (if (bn< (bn-abs a) (bn-abs b))
        quo
        (if (eqv? (bn< a bn0) (bn< b bn0))
            (_bn-quotient (bn- a b) b (bn+ quo (cons 1 bn0)))
            (_bn-quotient (bn+ a b) b (bn+ quo (cons 1 bn-1))))))
  (if (eq? b bn0)
      (display "error: division by 0") ;; error handling?
      (_bn-quotient a b bn0)))


;;------------------------------------------------------------------------------

;; comparison

(define (bn= a b)
  (equal? a b))

(define (bn< a b)
  (if (and (end? a) (end? b))  
      (if (eq? a b) #f (eq? a bn-1))
      (if (equal? (cdr a) (cdr b))
          (< (car a) (car b))
          (bn< (cdr a) (cdr b)))))

(define (bn<= a b)
  (or (bn= a b) (bn< a b)))

(define (bn> a b)
  (not (bn<= a b)))

(define (bn>= a b)
  (not (bn< a b)))


;;------------------------------------------------------------------------------

;; bitwise operations

(define (bn~ a) 
    (if (end? a)
        (if (or (eq? a bn0) (eq? (cdr a) bn0)) bn-1 bn0)
        ;;(if (equal? a bn0) bn-1 bn0)
        (let* ((_a (car a))
               (res (- (- base 1) _a)))
          (cons res (bn~ (cdr a))))))

;; FIXME only works with gambit since ribbit doesn't support bitwise operations

(cond-expand

 (gambit

  (define (bn-and a b)
    (if (and (end? a) (end? b))
        (if (and (equal? a bn-1) (equal? b bn-1)) bn-1 bn0)
        (let ((res (bitwise-and (car a) (car b))))
          (cons res (bn-and (cdr a) (cdr b))))))

  (define (bn-ior a b)
    (if (and (end? a) (end? b))
        (if (or (equal? a bn-1) (equal? b bn-1)) bn-1 bn0)
        (let ((res (bitwise-ior (car a) (car b))))
          (cons res (bn-or (cdr a) (cdr b))))))

  (define (bn-xor a b)
    (if (and (end? a) (end? b))
        (if (equal? a b) bn0 bn-1)
        (let ((res (bitwise-xor (car a) (car b))))
          (cons res (bn-xor (cdr a) (cdr b))))))

 ))

;; TODO bit shifts currently only work with base 2

;; (define (bn-shl a)
;;   (cons 0 a))

;; (define (bn-shift-left a n) ;; should probably treat the case where n < 0
;;   (if (equal? n 0)
;;       a
;;       (bn-shift-left (bn-shl a) (- n 1))))

;; (define (bn-shr a) ;; (arithmetic shift)
;;   (cdr a))

;; (define (bn-shift-right a n) ;; should probably treat the case where n < 0
;;   (if (equal? n 0)
;;       a
;;       (bn-shift-right (bn-shr a) (- n 1))))


;;------------------------------------------------------------------------------

;; misc (mostly just adapted from ribbit's own definitions)

(define (bn-neg a)
  (bn+ (bn~ a) (cons 1 bn0)))

(define (bn-zero? a)
  (equal? a bn0))

(define (bn-positive? a)
  (bn< bn0 a))

(define (bn-negative? a)
  (bn< a bn0))

(define (bn-max a b)
  (if (bn< a b) b a))

(define (bn-min a b)
  (if (bn< a b) a b))

(define (bn-abs a)
  (if (bn< a bn0) (bn-u a) a))

(define (bn-gcd a b)
  (let ((_a (bn-abs a))
        (_b (bn-abs b)))
    (if (bn< _a _b)
        (bn-gcd-aux _a _b)
        (bn-gcd-aux _b _a))))

(define (bn-gcd-aux a b)
  (if (equal? a bn0)
      b
      (bn-gcd-aux (bn-remainder b a) a)))

(define (bn-lcm a b)
  (if (equal? b bn0)
      (let ((_a (bn-abs a))
            (_b (bn-abs b)))
        (bn* (bn-quotient _a (bn-gcd _a _b)) _b))))


;;------------------------------------------------------------------------------

;; unit tests


;; utilities

;; (define-macro (test a b)
;;   `(let ((_a ,a)
;;          (_b ,b))
;;      (if (not (equal? _a _b))
;;          (begin
;;            (display "results not matching: ") (pp ',a)
;;            (display "a: ") (_pp _a)
;;            (display "b: ")(_pp _b)
;;            (newline)))))

(define (test a b)
  (if (not (equal? a b))
      (begin
        (display "results not matching: ") 
        (display "a: ") (_pp a)
        (display "b: ")(_pp b)
        (newline))))

;; (define-macro (test2 a b)
;;   `(let ((_a ,a)
;;          (_b ,b))
;;      (if (not (eq? _a _b))
;;          (begin
;;            (display "results not matching: ") (pp ',a)
;;            (display "a: ") (pp _a)
;;            (display "b: ")(pp _b)
;;            (newline)))))

;; for comparison tests

(define (test2 a b)
  (if (not (eqv? a b))
      (begin
        (display "a comparison test failed")
        (newline))))

(define base-1 (- base 1))


;;------------------------------------------------------------------------------

;; arithmetic


;; addition 

;; different parity, carry (pos res)
(test (bn+ (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons (- base 1) bn0)))

;; different parity, carry (res = 0)
(test (bn+ (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons 0 bn0)))
      (cons (- base 2) bn0))

;; different parity, no carry (neg res)
(test (bn+ (cons 0 (cons (- base 1) bn-1))
           (cons (- base 1) (cons 0 bn0)))
      bn-1)

;; same parity (pos), carry
(test (bn+ (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons (- base 1) (cons 1 bn0))))

;; same parity (pos), no carry
(test (bn+ (cons (- base 1) (cons 0 bn0))
           (cons 0 (cons 0 bn0)))
      (cons base-1 bn0))

;; same parity (neg), carry
(test (bn+ (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons (- base 2) (cons (- base 1) (cons 1 bn-1))))

;; same parity (neg), no carry
(test (bn+ (cons (- base 1) (cons 0 bn-1))
           (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 bn-1)))

(test (bn+ bn0 bn0) bn0)

(test (bn+ bn0 bn-1) bn-1)

(test (bn+ bn-1 bn0) bn-1)

(test (bn+ bn-1 bn-1) (cons 0 bn-1))



;; substraction

;; logic for these tests is no longer relevant, but they should still work

;; same parity (positive), no n-carry 
(test (bn- (cons (- base 1) (cons (- base 1) bn0))
           (cons 0 (cons (- base 1) bn0)))
      (cons base-1 bn0))

;; same parity (negative), no n-carry 
(test (bn- (cons (- base 1) (cons (- base 1) bn-1))
           (cons 0 (cons (- base 1) bn-1)))
      (cons base-1 bn0))

;; same parity (positive), n-carry 
(test (bn- (cons (- base 1) (cons 0 bn0))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 1 bn-1)))

;; same parity (negative), n-carry
(test (bn- (cons (- base 1) (cons 0 bn-1))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons 0 (cons 1 bn-1)))

;; different parity (pos, neg), no n-carry
(test (bn- (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons 0 (cons 0 (cons 1 bn0))))

;; different parity (neg, pos), no n-carry
(test (bn- (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 0 (cons 1 bn-1))))

;; different parity (pos, neg), n-carry
(test (bn- (cons (- base 1) bn0)
           (cons (- base 1) (cons (- base 1) (cons 0 bn-1))))
      (cons 0 (cons 1 (cons base-1 bn0))))

(test (bn- bn0 bn0) bn0)

(test (bn- bn0 bn-1) (cons 1 bn0))

(test (bn- bn-1 bn0) bn-1)

(test (bn- bn-1 bn-1) bn0)



;; unary substraction

(test (bn-u bn0) bn0)

(test (bn-u bn-1) (cons 1 bn0))



;; multiplication 

;; simple test, pos * pos => pos
(test (bn* (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 1 (cons 0 (cons (- base 2) (cons (- base 1) bn0)))))

;; simple test, pos * neg => neg
(test (bn* (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons 1 (cons 0 bn-1)))

;; simple test, neg * pos => neg     
(test (bn* (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 1 (cons 0 bn-1)))

;; simple test, neg * neg => neg
(test (bn* (cons 1 bn-1)
           (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 1) (cons 1 (cons (- base 2) bn0))))

(test (bn* bn-1 bn-1) (cons 1 bn0))

;; anything * 0 = 0
(test (bn* bn0 bn-1) bn0)
 
(test (bn* bn-1 bn0) bn0)



;; quotient 

;; (quotient pos pos) => pos 
(test (bn-quotient (cons (- base 1) (cons (- base 1) bn0))
                   (cons 0 (cons (- base 1) bn0)))
      (cons 1 bn0))

;; (quotient neg pos) => neg
(test (bn-quotient (cons (- base 1) (cons 0 bn-1))
                   (cons 0 (cons (- base 1) bn0)))
      (cons 1 bn-1))

;; (quotient pos neg) => neg
(test (bn-quotient (cons 0 (cons (- base 1) bn0))
                   (cons 0 (cons (- base 1) bn-1)))
      bn-1)

;; (quotient neg neg) => pos
(test (bn-quotient (cons (- base 1) (cons 0 bn-1))
                   (cons 0 bn-1))
      (cons (- base 1) bn0))

;; a and b equal => 1
(test (bn-quotient (cons (- base 1) (cons (- base 1) bn0))
                   (cons (- base 1) (cons (- base 1) bn0)))
      (cons 1 bn0))


;; division by 0, error
;; (test (bn-quotient bn0 bn0)
;;       "error: division by 0")


(test (bn-quotient bn0 bn-1) bn0)

(test (bn-quotient bn-1 bn-1) (cons 1 bn0))


(test (bn-quotient (cons (- base 1) (cons (- base 1) (cons (- base 1) bn0)))
                   (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 1 bn0)))

(test (bn-quotient (cons (- base 1) (cons (- base 1) (cons (- base 1) bn0)))
                   (cons (- base 1) (cons 0 bn-1)))
      (cons 0 (cons 1 bn-1)))

(test (bn-quotient (cons (- base 1) (cons 0 (cons 0 bn-1)))
                   (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 1 bn-1)))

(test (bn-quotient (cons (- base 1) (cons 0 (cons 0 bn-1)))
                   (cons (- base 1) (cons 0 bn-1)))
      (cons 0 (cons 1 bn0)))


;; remainder

;; (remainder pos pos) => pos
(test (bn-remainder (cons (- base 1) (cons 0 (cons (- base 1) bn0)))
                    (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons 1 (cons 0 bn0))))

;; (remainder pos neg) => pos
(test (bn-remainder (cons (- base 1) (cons (- base 1) bn0))
                    (cons (- base 1) (cons (- base 2) bn-1)))
      (cons (- base 2) (cons (- base 2) bn0)))

;; (remainder neg pos) => neg
(test (bn-remainder (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                    (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons base-1 (cons 1 bn-1))))

;; (remainder neg neg) => neg
(test (bn-remainder (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                    (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 (cons (- base 1) bn-1))))

;; (remainder a b) where |a| == |b| => 0
(test (bn-remainder (cons (- base 1) bn0)
                    bn-1)
      (cons (- base 2) bn0))



;; modulo 

;; (modulo pos pos) => pos
(test (bn-modulo (cons (- base 1) (cons 0 (cons (- base 1) bn0)))
                 (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons 1 bn0)))

;; (modulo pos neg) => neg
(test (bn-modulo (cons 1 (cons (- base 2) bn0))
                 (cons 0 (cons 1 bn-1)))
      (cons 1 (cons (- base 1) bn-1)))

;; please ignore this
(if (> base 2)
    ;; (modulo neg pos) => pos
    (test (bn-modulo (cons 0 (cons 0 (cons (- base 1) bn-1)))
                     (cons 0 (cons 1 bn0)))
          (cons 0 (cons 1 (cons (- base 2) bn0)))))

;; (modulo a b), a,b < 0, |a| > |b| => neg
(test (bn-modulo (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                 (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 (cons (- base 1) bn-1))))

;; (modulo a b), a,b < 0, |a| = |b| => 0
(test (bn-modulo bn-1 bn-1) bn0)

;; division by 0
;; (test (bn-modulo bn0 bn0) error)


;;------------------------------------------------------------------------------

;; comparison


;; equality

(test2 (bn= bn0 bn0) #t) 

(test2 (bn= bn-1 bn-1) #t)

(test2 (bn= bn0 bn-1) #f)

(test2 (bn= bn-1 bn0) #f)

(test2 (bn= (cons (- base 1) bn0)
            (cons (- base 1) bn0))
       #t)

(test2 (bn= (cons (- base 1) bn0)
            (cons 0 bn0))
       #f)


;; less than

(test2 (bn< bn0 bn0) #f)

(test2 (bn< bn-1 bn-1) #f)

(test2 (bn< bn0 bn-1) #f)

(test2 (bn< bn-1 bn0) #t)

(test2 (bn< (cons (- base 1) bn0) bn0) #f)

(test2 (bn< bn0 (cons (- base 1) bn0)) #t)

(test2 (bn< (cons 0 (cons (- base 1) bn-1))
            (cons 0 (cons (- base 1) bn0)))
       #t)

(test2 (bn< (cons 0 (cons (- base 1) bn0))
            (cons 0 (cons (- base 1) bn-1)))
       #f)

(test2 (bn< (bn-abs (cons 0 (cons (- base 1) bn0)))
            (bn-abs (cons (- base 1) (cons (- base 1) bn0))))
       #t)

(test2 (bn< (cons 0 (cons (- base 1) bn0))
            (cons (- base 1) (cons (- base 1) bn0)))
       #t)


;; less or equal

(test2 (bn<= bn0 bn0) #t)

(test2 (bn<= bn-1 bn-1) #t)

(test2 (bn<= bn0 bn-1) #f)

(test2 (bn<= bn-1 bn0) #t)


;; greater than

(test2 (bn> bn0 bn0) #f)

(test2 (bn> bn-1 bn-1) #f)

(test2 (bn> bn0 bn-1) #t)

(test2 (bn> bn-1 bn0) #f)


;; greater or equal

(test2 (bn>= bn0 bn0) #t)

(test2 (bn>= bn-1 bn-1) #t)

(test2 (bn>= bn0 bn-1) #t)

(test2 (bn>= bn-1 bn0) #f)


;;------------------------------------------------------------------------------

;; bitwise 


;;------------------------------------------------------------------------------

;; misc

;; negation

(test (bn-neg bn0) bn0)

(test (bn-neg bn-1) (cons 1 bn0))

(test (bn-neg (cons 1 bn0)) bn-1)


