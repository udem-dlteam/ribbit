(define-primitive (square a)
     "{PRIM1();push2(TAG_NUM((NUM(x) * NUM(x))), PAIR_TAG);break;}")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
