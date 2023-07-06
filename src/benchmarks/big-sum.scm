(define range (vector->list (make-vector-fill 1000 1)))

(display (let loop ((total 0) (rest range))
  (if (pair? rest)
    (loop (+ total (car rest)) (cdr rest))
    total)))
(newline)

;;;options: -l r4rs; -l r4rs/full-r4rs
;;;expected:
;;;1000
