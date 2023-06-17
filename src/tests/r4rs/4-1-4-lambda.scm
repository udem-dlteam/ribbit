(define reverse-subtract
  (lambda (x y) (- y x)))

(display (reverse-subtract 7 10))
(newline)

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))

(display (add4 6))
(newline)

(display ((lambda x x) 3 4 5 6))
(newline)

(display ((lambda (x y . z) z) 3 4 5 6))
(newline)

;;;options: -l r4rs
;;;expected:
;;;3
;;;10
;;;(3 4 5 6)
;;;(5 6)
