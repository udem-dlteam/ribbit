(export world)

(display "hello")
(%%putchar 10)
(display 'world)
(%%putchar 10)
(display 123456)
(%%putchar 10)

;;;run: -l min
;;;run: -l max
;;;run: -l max-tc
;;;r4rs-run: -l r4rs
;;;expected:
;;;hello
;;;world
;;;123456
