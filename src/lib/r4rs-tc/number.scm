(##include-once "./bool.scm")
(##include-once "./types.scm")
(##include-once "./pair-list.scm")
(##include-once "./control.scm")

;; Numbers (R4RS section 6.5).

(define (+ . args)
  (let op ((total 0) (rest args))
    (if (pair? rest)
      (op (##+ total (car rest)) (cdr rest))
      total)))

(define (* . args)
  (let op ((total 1) (rest args))
    (if (pair? rest)
      (op (##* total (car rest)) (cdr rest))
      total)))

(define (- x . y)
  (if (null? y)
    (##- 0 x)
    (##fold (lambda (x y) (##- x y)) x y)))

(define (/ x . y)
  (if (null? y)
    (##quotient 1 x)    ;; 1/x
    (##fold quotient x y)))

(define (quotient x y) (##quotient x y))

(define (exact? obj) #t)
(define (inexact? obj) #f)

(define (= x . rest)
  (##scan-until-false eqv? x #t rest))

(define (< x . rest) 
  (##scan-until-false (lambda (x y) (##< x y)) x #t rest))

(define (> x . rest) 
  (##scan-until-false (lambda (x y) (##< y x)) x #t rest))

(define (<= x . rest)
  (##scan-until-false (lambda (x y) (not (##< y x))) x #t rest))

(define (>= x . rest) 
  (##scan-until-false (lambda (x y) (not (##< x y))) x #t rest))

(define (zero? x) (##eqv? x 0))
(define (positive? x) (##< 0 x))
(define (negative? x) (##< x 0))
(define (even? x) (##eqv? x (##* 2 (##quotient x 2))))
(define (odd? x) (not (even? x)))
