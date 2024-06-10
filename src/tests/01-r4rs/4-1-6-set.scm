(define x 2)
(display (+ x 1))
(newline)

(set! x 4)
(display (+ x 1))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;3
;;;5
