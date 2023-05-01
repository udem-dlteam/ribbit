(define (foo x y (z 65))
  (putchar x)
  (putchar y)
  (putchar z))

(foo 69 69 69) ; E E E
(foo 69 69) ; E E A
; (foo) ; error
; (foo 69) ; error
; (foo 69 69 69 69) ; error

