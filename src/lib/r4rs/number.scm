(##include-once "./types.scm")
(##include-once "./bool.scm")
(##include-once "./control.scm")

;; Numbers (R4RS section 6.5).
(define ##no-args '(0))

(define (+ . args)
  (fold (lambda (x y) (##+ x y)) 0 args))

(define (* . args)
  (fold (lambda (x y) (##* x y)) 1 args))


(define (- x (y ##no-args))
  (if (##eqv? y ##no-args)
    (##- 0 x)
    (##- x y)))

(define (/ x (y ##no-args))
  (if (##eqv? y ##no-args)
    (##quotient 1 x)
    (##quotient x y)))

(define (quotient x y) (##quotient x y))

(define (exact? obj) #t)
(define (inexact? obj) #f)

(define (= x . rest)
  (scan-until (lambda (x y) (##eqv? x y)) x #t rest #f))

(define (< x . rest) 
  (scan-until (lambda (x y) (##< x y)) x #t rest #f))

(define (> x . rest) 
  (scan-until (lambda (x y) (##< y x)) x #t rest #f))

(define (<= x . rest)
  (scan-until (lambda (x y) (not (##< y x))) x #t rest #f))

(define (>= x . rest) 
  (scan-until (lambda (x y) (not (##< x y))) x #t rest #f))

(define (zero? x) (##eqv? x 0))
(define (positive? x) (##< 0 x))
(define (negative? x) (##< x 0))
(define (even? x) (##eqv? x (##* 2 (##quotient x 2))))
(define (odd? x) (not (even? x)))
