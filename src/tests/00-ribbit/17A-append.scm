(display (append '(x) '(y)))
(newline)
(display (append '(a) '(b c d)))
(newline)
(display (append '(a (b)) '((c))))
(newline)
(display (append '(a b) '(c d)))
(newline)

;;;run: -l max
;;;run: -l max-tc
;;;expected:
;;;(x y)
;;;(a b c d)
;;;(a (b) (c))
;;;(a b c d)
