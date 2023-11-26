(display (map cadr '((a b) (d e) (g h))))
(newline)

(display (map + '(1 2 3) '(4 5 6)))
(newline)

(display (map + '(1 2 3)))
(newline)

(display (map * '(1 2 3)))
(newline)

(display (map - '(1 2 3)))
(newline)

(display (let ((v (make-vector 5)))
           (for-each (lambda (i) (vector-set! v i (* i i)))
                     '(0 1 2 3 4))
           v))
(newline)

;;;options: -l r4rs
;;;expected:
;;;(b e h)
;;;(5 7 9)
;;;(1 2 3)
;;;(1 2 3)
;;;(-1 -2 -3)
;;;#(0 1 4 9 16)
