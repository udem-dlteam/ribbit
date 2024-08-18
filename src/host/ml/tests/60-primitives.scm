(define-primitive (square a)
  "prim1 (fun x -> match x with | Integer x -> Integer (x * x) | _ -> invalid_arg \"square argument must be Integer\");")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
