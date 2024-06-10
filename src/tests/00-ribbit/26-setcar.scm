(define p (cons 48 48))
(set-car! p 65)
(##putchar (car p))
(##putchar (cdr p))
(##putchar 10)

;;;run: -l min
;;;run: -l max
;;;r4rs-run: -l r4rs
;;;expected:
;;;A0
