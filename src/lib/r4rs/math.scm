(##include "./types.scm")
(##include "./number.scm")

(set! ##+ +)

(define (+ . args)
  (if (null? args)
    0
    (let sum ((total 0) (x (car args)) (rest (cdr args)))
      (if (null? rest)
        (##+ total x)
        (sum (##+ total x) (car rest) (cdr rest)))
      )))

(define (max x y) (if (< x y) y x))
(define (min x y) (if (< x y) x y))

(define (abs x) (if (< x 0) (- 0 x) x))

(define (remainder x y)
  (- x (* y (quotient x y))))

(define (modulo x y)
  (let ((q (quotient x y)))
    (let ((r (- x (* y q))))
      (if (eqv? r 0)
          0
          (if (eqv? (< x 0) (< y 0))
              r
              (+ r y))))))

(define (gcd x y)
  (let ((ax (abs x)))
    (let ((ay (abs y)))
      (if (< ax ay)
          (gcd-aux ax ay)
          (gcd-aux ay ax)))))

(define (gcd-aux x y)
  (if (eqv? x 0)
      y
      (gcd-aux (remainder y x) x)))

(define (lcm x y)
  (if (eqv? y 0)
      0
      (let ((ax (abs x)))
        (let ((ay (abs y)))
          (* (quotient ax (gcd ax ay)) ay)))))

(define numerator id)
(define (denominator x) 1)

(define floor id)
(define ceiling id)
(define truncate id)
(define round id)

;;(define (rationalize x y) ...)
;;(define (exp x) ...)
;;(define (log x) ...)
;;(define (sin x) ...)
;;(define (cos x) ...)
;;(define (tan x) ...)
;;(define (asin x) ...)
;;(define (acos x) ...)
;;(define (atan y . x) ...)

;;(define (sqrt x) ...)

(define (expt x y)
  (if (eqv? y 0)
      1
      (let ((t (expt (* x x) (quotient y 2))))
        (if (odd? y)
            (* x t)
            t))))

;;(define (make-rectangular x y) ...)
;;(define (make-polar x y) ...)
;;(define (real-part x) ...)
;;(define (imag-part x) ...)
;;(define (magnitude x) ...)
;;(define (angle x) ...)
