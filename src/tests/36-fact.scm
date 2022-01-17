(define fact
  (lambda (n)
    (if (< n 2)
        1
        (* n (fact (- n 1))))))

(write (fact 10))
(putchar 10)

;;;options: -l min
;;;expected:
;;;3628800
