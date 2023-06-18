(##include-once "./bool.scm")
(##include-once "./types.scm")
(##include-once "./control.scm")

(define (max x . rest) 
  (fold (lambda (curr best) (if (##< best curr) curr best)) x rest))

(define (min x . rest) 
  (fold (lambda (curr best) (if (##< best curr) best curr)) x rest))

(define (abs x) (if (##< x 0) (##- 0 x) x))

(define (remainder x y)
  (##- x (##* y (##quotient x y))))

(define (modulo x y)
  (let ((q (##quotient x y)))
    (let ((r (##- x (##* y q))))
      (if (##eqv? r 0)
          0
          (if (##eqv? (##< x 0) (##< y 0))
              r
              (##+ r y))))))

(define (gcd . args)
  (define (gcd-aux x y)
    (if (##eqv? x 0)
      y
      (gcd-aux (remainder y x) x)))

  (fold (lambda (x y)
          (let ((ax (abs x)) (ay (abs y)))
            (if (##< ax ay)
              (gcd-aux ax ay)
              (gcd-aux ay ax))))
        0
        args))


(define (lcm . args)
  (define (lcm-aux x y)
    (if (##eqv? y 0)
      0
      (let ((ax (abs x)) (ay (abs y)))
        (##* (##quotient ax (gcd ax ay)) ay))))

  (fold lcm-aux 1 args))


(define (denominator x) 1)

(define (floor x) (##id x))
(define numerator floor)
(define ceiling floor)
(define truncate floor)
(define round floor)
