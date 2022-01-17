(define r (rib 48 48 48))
(field1-set! r 65)
(putchar (field0 r))
(putchar (field1 r))
(putchar (field2 r))
(putchar 10)

;;;options: -l empty
;;;expected:
;;;0A0
