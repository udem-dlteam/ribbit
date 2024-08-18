(define-primitive (square a)
     "eval _C=\\$_X$_S
      _C=$((_C*_C))
      _PUSH
      ;;")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
