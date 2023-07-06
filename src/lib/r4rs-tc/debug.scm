(if-feature
  v-port
  (begin (##include-once "./v-io.scm"))
  (begin (##include-once "./io.scm")))

(define (pp-return proc . x)
  (let ((r (apply proc x)))
    (pp r)
    r))
