(display (and (= 2 2) (> 2 1)))
(newline)

(display (and (= 2 2) (< 2 1)))
(newline)

(display (and 1 2 'c '(f g)))
(newline)

(display (and))
(newline)

(display (or (= 2 2) (> 2 1)))
(newline)

(display (or (= 2 2) (< 2 1)))
(newline)

(display (or #f #f #f))
(newline)

(display (or))
(newline)

(display (or (memq 'b '(a b c)) (+ 3 0)))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;#t
;;;#f
;;;(f g)
;;;#t
;;;#t
;;;#t
;;;#f
;;;#f
;;;(b c)
