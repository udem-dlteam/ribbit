(define-primitive (square a)
     " , prim1 (pure . (\\case RibInt x -> RibInt (x * x); _ -> ribFalse))\n")

(%%putchar (square 6))
(%%putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
