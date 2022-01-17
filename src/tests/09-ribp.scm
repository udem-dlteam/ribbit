(putchar (if (rib? 42) 65 66))
(putchar (if (rib? (rib 1 2 3)) 65 66))
(putchar 10)

;;;options: -l empty
;;;expected:
;;;BA
