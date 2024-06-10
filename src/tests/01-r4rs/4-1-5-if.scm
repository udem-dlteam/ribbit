(display (if (> 3 2) 'yes 'no))
(newline)

(display (if (> 2 3) 'yes 'no))
(newline)

(display (if (> 3 2) (- 3 2) (+ 3 2)))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;yes
;;;no
;;;1
