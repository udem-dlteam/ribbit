(##putchar (if (##eqv? 1 2) 65 66))
(##putchar (if (##eqv? 2 1) 65 66))
(##putchar (if (##eqv? 2 2) 65 66))
(##putchar 10)

;;;run: -l empty
;;;expected:
;;;BBA
