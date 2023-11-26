(export world)

(write "hello")
(putchar 10)
(write 'world)
(putchar 10)
(write 123456)
(putchar 10)
(write #f)
(putchar 10)
(write #t)
(putchar 10)
(write '())
(putchar 10)
(write (cons 1 (cons 2 '())))
(putchar 10)

;;;options: -l min
;;;expected:
;;;"hello"
;;;world
;;;123456
;;;#f
;;;#t
;;;()
;;;(1 2)
