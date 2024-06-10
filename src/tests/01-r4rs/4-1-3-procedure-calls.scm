(display (+ 3 4))
(newline)

(display ((if #f + *) 3 4))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;7
;;;12
