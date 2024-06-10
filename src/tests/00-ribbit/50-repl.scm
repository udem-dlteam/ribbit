(export define lambda if < * -)

(repl)

;;;run: -l max -f+ arity-check
;;;r4rs-run: -l r4rs
;;;input:(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 10)
;;;expected:
;;;> 0
;;;> 3628800
;;;> 
