(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(run-bench
  (lambda () (fib 20))
  (lambda (result) (= result 6765)))