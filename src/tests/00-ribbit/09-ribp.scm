(%%putchar (if (%%rib? 42) 65 66))
(%%putchar (if (%%rib? (%%rib 1 2 3)) 65 66))
(%%putchar 10)

;;;run: -l empty
;;;expected:
;;;BA
