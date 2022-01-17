(putchar (if #f 65 66))
(putchar (if #t 65 66))
(putchar (if 42 65 66))
(putchar (if (not #f) 65 66))
(putchar (if (not #t) 65 66))
(putchar (if (not 42) 65 66))
(putchar 10)

;;;options: -l min
;;;expected:
;;;BAAABB
