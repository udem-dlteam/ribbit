(display (case (* 3 4)
  ((1 2) 'a)
  ((3 4) 'b)
  ((5 6 12) 'c)
  (else 'd)))
(newline)

(display (case (* 1 2)
  ((1 2) 'a)
  ((3 4) 'b)
  ((5 6 12) 'c)
  (else 'd)))
(newline)

(display (case (* 2 2)
  ((1 2) 'a)
  ((3 4) 'b)
  ((5 6 12) 'c)
  (else 'd)))
(newline)

(display (case (* 2 0)
  ((1 2) 'a)
  ((3 4) 'b)
  ((5 6 12) 'c)
  (else 'd)))
(newline)

;;;fancy-compiler
;;;options: -l r4rs
;;;expected:
;;;c
;;;a
;;;b
;;;d
