(define x 12)

(display `(x ,x))
(newline) 

(define fruits '(apple banana cherry))
(define more-fruits '(orange kiwi))

(define combined-fruits `(grape ,@fruits ,@more-fruits))

(display combined-fruits)
(newline)

;;;fancy-compiler
;;;options: -l r4rs/io -l r4rs/control -l r4rs/pair-list
;;;expected:
;;;(x 12)
;;;(grape apple banana cherry orange kiwi)
