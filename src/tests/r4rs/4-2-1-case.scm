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

(display (case (* 2 3)
           ((2 3 5 7) 'prime)
           ((1 4 6 8 9) 'composite)))
(newline)

(display (case (car '(c d))
           ((a e i o u) 'vowel)
           ((w y) 'semivowel)
           (else 'consonant)))
(newline)

;;;fancy-compiler
;;;options: -l r4rs
;;;expected:
;;;c
;;;a
;;;b
;;;d
;;;composite
;;;consonant
