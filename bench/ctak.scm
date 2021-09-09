(define ctak
  (lambda (x y z)
    (call/cc
      (lambda (k)
       (ctak-aux k x y z)))))

(define ctak-aux
  (lambda (k x y z)
    (if (not (< y x))
      (k z)
      (call/cc
        (lambda (k)
          (ctak-aux
            k
            (call/cc (lambda (k) (ctak-aux k (- x 1) y z)))
            (call/cc (lambda (k) (ctak-aux k (- y 1) z x)))
            (call/cc (lambda (k) (ctak-aux k (- z 1) x y)))))))))

(ctak 18 12 6)
