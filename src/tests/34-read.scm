(export a b)

(write (read))
(putchar 10)

;;;options: -l min
;;;input:(#f #t 123 (a b) "abc")
;;;expected:
;;;(#f #t 123 (a b) "abc")
