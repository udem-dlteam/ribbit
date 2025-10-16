(%%putchar (if (pair? 42) 65 66))
(%%putchar (if (pair? (cons 1 2)) 65 66))
(%%putchar 10)

;;;run: -l min
;;;run: -l max
;;;run: -l max-tc
;;;r4rs-run: -l r4rs
;;;expected:
;;;BA
