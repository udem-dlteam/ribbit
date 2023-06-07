(repl)

;;;fancy-compiler
;;;input:(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 10)
;;;options: -l r4rs
;;;expected:
;;;
;;;              ____________________
;;;             |                    |
;;;             | Welcome to Ribbit! |
;;;             |                    |
;;;    λ        | - Rib the Frog     |
;;;  @...@  --- |____________________|
;;; (-----)
;;;( >___< )
;;;^^ ~~~ ^^
;;;
;;;> 0
;;;> 3628800
;;;> 
