(display (- 1 2))
(newline)

(display (- 5))
(newline)

(display (- 1 2 3 10))
(newline)

(display (- -5))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/math -l r4rs/io
;;;expected:
;;;-1
;;;-5
;;;-14
;;;5
