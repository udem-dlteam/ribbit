(display (number? 3))
(newline)

(display (complex? 3))
(newline)

(display (real? 3))
(newline)

(display (rational? 3))
(newline)

(display (integer? 3))
(newline)

(display (exact? 3))
(newline)

(display (inexact? 3))
(newline)

(display (zero? 0))
(newline)

(display (zero? 1))
(newline)

(display (zero? -1))
(newline)

(display (zero? -100))
(newline)

(display (positive? 4))
(newline)

(display (positive? -4))
(newline)

(display (positive? 0))
(newline)

(display (negative? 4))
(newline)

(display (negative? -4))
(newline)

(display (negative? 0))
(newline)

(display (odd? 3))
(newline)

(display (odd? 2))
(newline)

(display (odd? -4))
(newline)

(display (odd? -1))
(newline)

(display (even? 3))
(newline)

(display (even? 2))
(newline)

(display (even? -4))
(newline)

(display (even? -1))
(newline)

;;;options: -l r4rs
;;;expected:
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#f
;;;#t
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#t
;;;#f
;;;#f
;;;#t
;;;#f
;;;#t
;;;#t
;;;#f
