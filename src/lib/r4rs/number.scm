(##include "./bool.scm")
(##include "./control.scm")

;; Numbers (R4RS section 6.5).
(set! ##< <)

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? obj) #t)
(define (inexact? obj) #f)

(define (= x . rest)
  (lazy-scan eqv? x #t rest #f))

(define (< x . rest) 
  (lazy-scan ##< x #t rest #f))

(define (> x . rest) 
  (let ((##> (lambda (x y) (##< y x))))
    (lazy-scan ##> x #t rest #f)))

(define (<= x . rest)
  (let ((##<= (lambda (x y) (not (##< y x)))))
    (lazy-scan ##<= x #t rest #f)))

(define (>= x . rest) 
  (let ((##>= (lambda (x y) (not (##< x y)))))
    (lazy-scan ##>= x #t rest #f)))

(define (zero? x) (eqv? x 0))
(define (positive? x) (< 0 x))
(define (negative? x) (< x 0))
(define (even? x) (eqv? x (* 2 (quotient x 2))))
(define (odd? x) (not (even? x)))

;;(define (exact->inexact x) ...)
;;(define (inexact->exact x) ...)
