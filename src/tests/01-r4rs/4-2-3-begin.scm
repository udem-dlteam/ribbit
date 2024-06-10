(display (begin (set! x (begin (begin 5)))
		      (begin ((begin +) (begin x) (begin (begin 1))))))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;6
