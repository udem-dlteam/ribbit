(export *)

(write (eval (read)))
(##putchar 10)

;;;run: -l min -f+ arity-check
;;;run: -l max -f+ arity-check
;;;run: -l min
;;;run: -l max
;;;r4rs-run: -l r4rs
;;;input:(* 6 7)
;;;expected:
;;;42
