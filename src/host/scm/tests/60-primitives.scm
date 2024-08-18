(define-primitive (square a)
  "(prim1 (lambda (x) (* x x)))")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
