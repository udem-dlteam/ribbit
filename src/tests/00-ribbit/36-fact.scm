(define fact
  (lambda (n)
    (if (< n 2)
        1
        (* n (fact (- n 1))))))

(write (fact 10))
(##putchar 10)

;;;run: -l min
;;;run: -l max
;;;r4rs-run: -l r4rs
;;;expected:
;;;3628800
