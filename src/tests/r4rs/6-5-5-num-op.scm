(display (+ 1 2))
(newline)

(display (+ 5))
(newline)

(display (+ 1 2 3))
(newline)

(display (+))
(newline)

(newline)

(display (* 1 2))
(newline)

(display (* 5))
(newline)

(display (* 1 2 3))
(newline)

(display (*))
(newline)

(newline)

(display (- 1 2))
(newline)

(display (- 5))
(newline)

(display (- -5))
(newline)

(newline)

(display (/ 9 2))
(newline)

(display (/ 5))
(newline)

(display (/ 42 -3))
(newline)

(display (/ -5))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/number -l r4rs/io
;;;expected:
;;;3
;;;5
;;;6
;;;0
;;;
;;;2
;;;5
;;;6
;;;1
;;;
;;;-1
;;;-5
;;;5
;;;
;;;4
;;;0
;;;-14
;;;0
