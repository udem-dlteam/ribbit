(define p (cons 48 48))
(set-car! p 65)
(putchar (car p))
(putchar (cdr p))
(putchar 10)

;;;options: -l min
;;;expected:
;;;A0
