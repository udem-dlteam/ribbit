(putchar (if (< 1 2) 65 66))
(putchar (if (< 2 1) 65 66))
(putchar (if (< 2 2) 65 66))
(putchar 10)

;;;options: -l empty
;;;expected:
;;;ABB
