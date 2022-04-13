(export begin define lambda if < * -)

(repl)

;;;options: -l max
;;;input:(begin (define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 10))
;;;expected:
;;;> 3628800
;;;> 
