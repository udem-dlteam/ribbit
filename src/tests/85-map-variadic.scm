;; (display (apply ##+ (list 5 6)))
(display (map ##+ (list 1 2 3) (list 4 5 6)))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/io -l r4rs/control -l r4rs/math
;;;expected:
;;;(5 7 9)
