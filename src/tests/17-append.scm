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

;;;options: -l r4rs
;;;expected:
;;;(x y)
;;;(a b c d)
;;;(a (b) (c))
;;;(a b c . d)
;;;a
;;;(a b . c)
