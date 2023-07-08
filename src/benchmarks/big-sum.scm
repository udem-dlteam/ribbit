(define range (vector->list (make-vector-fill 5000 1)))

(let loop ((total 0) (rest range))
  (if (pair? rest)
    (loop (+ total (car rest)) (cdr rest))
    total))

;;;options: -l r4rs; -l r4rs/full-r4rs
;;;expected:
