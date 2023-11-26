(display (if (> 3 2) 'yes 'no))
(newline)

(display (if (> 2 3) 'yes 'no))
(newline)

(display (if (> 3 2) (- 3 2) (+ 3 2)))
(newline)

;;;options: -l r4rs
;;;expected:
;;;yes
;;;no
;;;1
