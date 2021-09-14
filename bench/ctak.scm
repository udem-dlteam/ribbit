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

(define run-n
  (lambda (lo hi r)
    (if (< lo hi)
      (run-n (+ 1 lo) hi (ctak 18 12 6))
      r)))

(define run
 (lambda ()
  (run-n 0 90 0)))

(run)


