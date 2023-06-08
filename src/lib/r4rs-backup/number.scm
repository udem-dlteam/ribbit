(##include-once "./bool.scm")
(##include-once "./control.scm")

;; Numbers (R4RS section 6.5).
(set! ##< <)

(set! ##no-args '(0))

(set! ##+ +)
(set! ##* *)
(set! ##- -)

(define (+ . args)
  (fold ##+ 0 args))

(define (* . args)
  (fold ##* 1 args))


(define (- x (y ##no-args) . rest)
  (if (eqv? y ##no-args)
    (##- 0 x)
    (fold ##- (##- x y) rest)))

(define (/ x (y ##no-args) . rest)
  (if (eqv? y ##no-args)
    (quotient 1 x)
    (fold quotient (quotient x y) rest)))

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? obj) #t)
(define (inexact? obj) #f)

(define (= x . rest)
  (scan-until eqv? x #t rest #f))

(define (< x . rest) 
  (scan-until ##< x #t rest #f))

(define (> x . rest) 
  (let ((##> (lambda (x y) (##< y x))))
    (scan-until ##> x #t rest #f)))

(define (<= x . rest)
  (let ((##<= (lambda (x y) (not (##< y x)))))
    (scan-until ##<= x #t rest #f)))

(define (>= x . rest) 
  (let ((##>= (lambda (x y) (not (##< x y)))))
    (scan-until ##>= x #t rest #f)))

(define (zero? x) (eqv? x 0))
(define (positive? x) (< 0 x))
(define (negative? x) (< x 0))
(define (even? x) (eqv? x (* 2 (quotient x 2))))
(define (odd? x) (not (even? x)))
