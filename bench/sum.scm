(define calc (lambda (i sum)
  (if (< i 1)
    sum
    (calc (- i 1) (+ i sum)))))

(define run (lambda ()
 (calc 10000 0)))

(run)
