(define x 12)

(display `(x ,x))
(newline) 

(define fruits '(apple banana cherry))
(define more-fruits '(orange kiwi))

(define combined-fruits `(grape ,@fruits ,@more-fruits))

(display combined-fruits)
(newline)

;; (display `(1 `,(+ 1 ,(+ 2 3)) 4))
;; (newline)
;;
;; (display `(1 ```,,@,,@(list (+ 1 2)) 4))
;; (newline)

;;;r4rs-run: -l r4rs/pair-list -l r4rs/io
;;;expected:
;;;(x 12)
;;;(grape apple banana cherry orange kiwi)
