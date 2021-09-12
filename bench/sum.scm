(define calc (lambda (i sum)
  (if (< i 1)
    sum
    (calc (- i 1) (+ i sum)))))

(define run-n
  (lambda (lo hi r)
    (if (< lo hi)
      (run-n (+ 1 lo) hi (calc 10000 0))
    r)))

(define run
  (lambda ()
    (run-n 0 2500 0)))

(run)
