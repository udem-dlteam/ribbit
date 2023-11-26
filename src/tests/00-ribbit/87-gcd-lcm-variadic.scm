(display (gcd 32 -36))
(newline)

(display (gcd 45 15 -5))
(newline)

(display (gcd 42 3 7 6 21 3))
(newline)

(display (gcd))
(newline)

(display "-----")
(newline)

(display (lcm 32 -36))
(newline)

(display (lcm 45 15 -5))
(newline)

(display (lcm 42 3 7 6 21 3))
(newline)

(display (lcm))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/math -l r4rs/io
;;;expected:
;;;4
;;;5
;;;1
;;;0
;;;-----
;;;288
;;;45
;;;42
;;;1
