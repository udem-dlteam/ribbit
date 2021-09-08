(define calc (lambda (i sum)
  (if (< i 1)
    sum
    (calc (- i 1) (+ i sum)))))

(calc 10000 0)
