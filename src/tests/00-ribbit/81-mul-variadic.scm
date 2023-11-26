(display (* 1 2))
(newline)

(display (* 5))
(newline)

(display (* 1 2 3))
(newline)

(display (*))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/number -l r4rs/io
;;;expected:
;;;2
;;;5
;;;6
;;;1
