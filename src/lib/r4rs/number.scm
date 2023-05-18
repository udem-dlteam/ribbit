(##include "./bool.scm")

;; Numbers (R4RS section 6.5).

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

;;(define (exact? obj) #t)
;;(define (inexact? obj) #f)

(define = eqv?)
(define (> x y) (< y x))
(define (<= x y) (not (< y x)))
(define (>= x y) (not (< x y)))

(define (zero? x) (eqv? x 0))
(define (positive? x) (< 0 x))
(define (negative? x) (< x 0))
(define (even? x) (eqv? x (* 2 (quotient x 2))))
(define (odd? x) (not (even? x)))

;;(define (exact->inexact x) ...)
;;(define (inexact->exact x) ...)
