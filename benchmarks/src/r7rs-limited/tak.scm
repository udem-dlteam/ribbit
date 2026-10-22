;;; TAK -- A vanilla version of the TAKeuchi function.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(run-bench
    (lambda () (tak 18 12 6))
    (lambda (result) (equal? result 7)))