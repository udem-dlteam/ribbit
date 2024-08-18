(define-primitive (square a)
  "lambda: push(pop() ** 2),")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
