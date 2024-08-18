(define-primitive (square a)
     "const val: RibField = self.stackPop();
try self.stackPush(num(val.num * val.num));
},\n")

(##putchar (square 6))
(##putchar 10)

;;;run: -l max
;;;run: -l empty
;;;variadics-run: -l max -f+ arity-check
;;;variadics-run: -l empty -f+ arity-check
;;;expected:
;;;$
