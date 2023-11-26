(define x '#(1 (2 3) "allo"))


(write x)
(newline)

;;;fancy-compiler
;;;options: -l r4rs/vector -l r4rs/io
;;;expected:
;;;#(1 (2 3) "allo")
