(display (quote a))
(newline)

(display (quote #(a b c)))
(newline)

(display (quote (+ 1 2)))
(newline)

(display 'a)
(newline)

(display '#(a b c))
(newline)

(display '())
(newline)

(display '(+ 1 2))
(newline)

(display '(quote a))
(newline)

(display ''a)
(newline)


;;;r4rs-run: -l r4rs
;;;expected:
;;;a
;;;#(a b c)
;;;(+ 1 2)
;;;a
;;;#(a b c)
;;;()
;;;(+ 1 2)
;;;(quote a)
;;;(quote a)
