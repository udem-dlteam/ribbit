(display (apply + (list 3 4)))
(newline)

(display (apply (lambda (a b) (+ a b)) (list 3 4)))
(newline)

(display (apply + (append (list 10) (list 3 4))))
(newline)

(display (apply list '()))
(newline)

(define compose (lambda (f g) (lambda args (f (apply g args)))))
(display ((compose - *) 12 75))
(newline)

;;;options: -l r4rs
;;;expected:
;;;7
;;;7
;;;17
;;;()
;;;-900
