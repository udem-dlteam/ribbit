(display (min 9 2))
(newline)

(display (min 5 5 -5))
(newline)

(display (min 42 3 2 1 2 3))
(newline)

(display (min -5))
(newline)

(display "-----")
(newline)

(display (max 9 2))
(newline)

(display (max 5 5 -5))
(newline)

(display (max 42 3 2 1 2 3))
(newline)

(display (max -5))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/math -l r4rs/io
;;;expected:
;;;2
;;;-5
;;;1
;;;-5
;;;-----
;;;9
;;;5
;;;42
;;;-5
