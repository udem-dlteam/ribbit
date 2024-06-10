(display (append '(x) '(y)))
(newline)
(display (append '(a) '(b c d)))
(newline)
(display (append '(a (b)) '((c))))
(newline)
(display (append '(a b) '(c . d)))
(newline)
(display (append '() 'a))
(newline)
(display (append '(a b) 'c))
(newline)

;;;r4rs-run: -l r4rs/pair-list -l r4rs/io
;;;expected:
;;;(x y)
;;;(a b c d)
;;;(a (b) (c))
;;;(a b c . d)
;;;a
;;;(a b . c)
