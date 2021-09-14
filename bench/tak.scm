(define tak
  (lambda (x y z)
    (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y)))))

(define run-n
  (lambda (lo hi r)
    (if (< lo hi)
      (run-n (+ 1 lo) hi (tak 18 12 6))
      r)))

(define run
  (lambda ()
    (run-n 0 200 0)))


(run)


