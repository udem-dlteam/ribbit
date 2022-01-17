(putchar (if (pair? 42) 65 66))
(putchar (if (pair? (cons 1 2)) 65 66))
(putchar 10)

;;;options: -l min
;;;expected:
;;;BA
