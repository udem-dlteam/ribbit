(display (procedure? car))
(newline)

(display (procedure? 'car))
(newline)

(display (procedure? (lambda (x) (* x x))))
(newline)

(display (procedure? '(lambda (x) (* x x))))
(newline)

(display (call-with-current-continuation procedure?))
(newline)

;;;options: -l r4rs
;;;expected:
;;;#t
;;;#f
;;;#t
;;;#f
;;;#t
