(##include "./types.scm")
(##include "./control.scm")
(##include "./number.scm")

(define (max x . rest) 
  (fold (lambda (curr best) (if (< best curr) curr best)) x rest))

(define (min x . rest) 
  (fold (lambda (curr best) (if (< best curr) best curr)) x rest))

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

(define (gcd . args)
  (define (gcd-aux x y)
    (if (eqv? x 0)
      y
      (gcd-aux (remainder y x) x)))

  (fold (lambda (x y)
          (let ((ax (abs x)) (ay (abs y)))
            (if (< ax ay)
              (gcd-aux ax ay)
              (gcd-aux ay ax))))
        0
        args))


(define (lcm . args)
  (define (lcm-aux x y)
    (if (eqv? y 0)
      0
      (let ((ax (abs x)) (ay (abs y)))
        (* (quotient ax (gcd ax ay)) ay))))

  (fold lcm-aux 1 args))


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