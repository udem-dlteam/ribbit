(display (not #t))
(newline)

(display (not 3))
(newline)

(display (not (list 3)))
(newline)

(display (not #f))
(newline)

(display (not '()))
(newline)

(display (not (list)))
(newline)

(display (not 'nil))
(newline)

(display (boolean? #f))
(newline)

(display (boolean? 0))
(newline)

(display (boolean? '()))
(newline)


;;;r4rs-run: -l r4rs
;;;expected:
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#f
