(define (pp-return proc . x)
  (let ((r (apply proc x)))
    (pp r)
    r))
