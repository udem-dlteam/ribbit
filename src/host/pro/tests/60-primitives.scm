(define-primitive (square a) "_) :-
  narg_check(1, Stack, Stack0, square),
  pop(Stack0, X, Stack1),
  N is X * X,
  push(Stack1, N, NewStack).
")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
