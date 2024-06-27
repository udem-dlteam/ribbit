(define-primitive (square a)
      "\tdd   prim_square\n")

(define-feature square
  ((prims "
prim_square: 
%if FIX_TAG != 0
\tdec LAST_ARG
%endif
\tshr LAST_ARG, 1
\timul LAST_ARG, LAST_ARG
\tshl LAST_ARG, 1
%if FIX_TAG != 0
\tinc LAST_ARG
%endif
\tNBARGS(1)
\tret")))

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
