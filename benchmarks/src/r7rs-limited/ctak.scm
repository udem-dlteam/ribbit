(define (ctak x y z)
    (call-with-current-continuation
        (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
    (if (not (< y x))
        (k z)
        (call-with-current-continuation
        (lambda (k)
        (ctak-aux
            k
            (call-with-current-continuation
            (lambda (k) (ctak-aux k (- x 1) y z)))
            (call-with-current-continuation
            (lambda (k) (ctak-aux k (- y 1) z x)))
            (call-with-current-continuation
            (lambda (k) (ctak-aux k (- z 1) x y))))))))

(run-bench
    (lambda () (ctak 18 12 6))
    (lambda (result) (equal? result 7)))