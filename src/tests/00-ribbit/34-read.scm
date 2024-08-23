(export a b)

(write (read))
(##putchar 10)


;;;run: -l min
;;;run: -l max
;;;run: -l max-tc
;;;r4rs-run: -l r4rs
;;;input:(#f #t 123 (a b) "abc")
;;;expected:
;;;(#f #t 123 (a b) "abc")
