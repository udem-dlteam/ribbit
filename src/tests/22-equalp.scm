(putchar (if (equal? 1 2) 65 66))
(putchar (if (equal? 2 1) 65 66))
(putchar (if (equal? 2 2) 65 66))
(putchar (if (equal? 42 (rib 1 2 3)) 65 66))
(putchar (if (equal? (rib 1 2 3) 42) 65 66))
(putchar (if (equal? (rib 1 2 3) (rib 1 2 3)) 65 66))
(putchar (if (equal? (rib 1 2 3) (rib 1 2 4)) 65 66))
(putchar 10)

;;;options: -l min
;;;expected:
;;;BBABBAB
