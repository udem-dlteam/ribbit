;;; SUM -- Compute sum of integers from 0 to 10000

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
        sum
        (loop (- i 1) (+ i sum)))))

(run-bench
    (lambda () (run (hide 12 10000)))
    (lambda (r) (equal? r 50005000)))
