(define (succ n) (+ n 1))
(define (pred n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (= 0 y)
      (k x)
      (addc (succ x) (pred y) k)))

(define (fibc x c)
  (if (= 0 x)
      (c 0)
      (if (= 0 (pred x))
          (c 1)
          (addc (call-with-current-continuation
                 (lambda (c) (fibc (pred x) c)))
                (call-with-current-continuation
                 (lambda (c) (fibc (pred (pred x)) c)))
                c))))

(run-bench
    (lambda () (call-with-current-continuation (lambda (c) (fibc 20 c))))
    (lambda (result) (= result 6765)))