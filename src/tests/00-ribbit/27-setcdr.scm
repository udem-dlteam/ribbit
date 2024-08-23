(define p (cons 48 48))
(set-cdr! p 65)
(##putchar (car p))
(##putchar (cdr p))
(##putchar 10)

;;;run: -l min
;;;run: -l max
;;;run: -l max-tc
;;;r4rs-run: -l r4rs
;;;expected:
;;;0A
