;; (display (apply ##+ (list 5 6)))
(display (map ##+ (list 1 2 3) (list 4 5 6)))
(newline)

;;;r4rs-run: -l r4rs/io -l r4rs/control -l r4rs/math
;;;expected:
;;;(5 7 9)
