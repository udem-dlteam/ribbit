(define ack
  (lambda( m n)
    (if (= m 0) 
      (+ n 1)
      (if (= n 0)
        (ack (- m 1) 1)
        (ack (- m 1) (ack m (- n 1)))
        ))))


(define run-n
 (lambda (lo hi r)
 (if (< lo hi)
  (run-n (+ 1 lo) hi (ack 3 9))
  r)))

(define run
 (lambda ()
  (run-n 0 2 0)))

(run)
