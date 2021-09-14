(define fib
  (lambda (n)
    (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2))))))

(define run-n
 (lambda (lo hi r)
 (if (< lo hi)
  (run-n (+ 1 lo) hi (fib 35))
  r)))

(define run
 (lambda ()
  (run-n 0 1 0)))

(run)
