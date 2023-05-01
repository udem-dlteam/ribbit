(define (foo x y (z 65))
  (putchar x)
  (putchar y)
  (putchar z))

(foo 69 69 69) ; E E E
(putchar 10)

(foo 69 69) ; E E A
(putchar 10)

(define (bar x (y 65) . z)
  (putchar x)
  (putchar y)
  (putchar (if (null? z) 78 (car z))))

(bar 69) ; E A N
(putchar 10)

(bar 69 70) ; E F N
(putchar 10)

(bar 69 70 71 66) ; E F G
(putchar 10)

(define sym 69)
(define (baz (x sym))
  (putchar x))

(baz 66) ; B
(putchar 10)

(baz) ; E
(putchar 10)

(define (plus x (y 1))
  (+ x y))

(define (barbar y (x (plus 68)))
  (putchar y)
  (putchar x))

(barbar 70) ; F E
(putchar 10)

(barbar 70 71) ; F G
(putchar 10)

; (foo) ; error
; (foo 69) ; error
; (foo 69 69 69 69) ; error

