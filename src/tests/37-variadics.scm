(define (show-lst lst)
  (putchar 40)    ; #\(
  (show-lst* lst)
  (putchar 41))   ; #\)
  

(define (show-lst* lst)
  (if (pair? lst)
    (begin
        (putchar (car lst))
        (show-lst* (cdr lst)))))
    

(define (foo1 . x) (show-lst x))
(define (foo2 a . x) (putchar a) (show-lst x))
(define (foo3 a b . x) (putchar a) (putchar b) (show-lst x))

(define foo4 (lambda x (show-lst x)))
(define foo5 (lambda (a . x) (putchar a) (show-lst x)))
(define foo6 (lambda (a b . x) (putchar a) (putchar b) (show-lst x)))

(foo1)
(foo1 65)
(foo1 65 66)
(foo2 65)
(foo2 65 66)
(foo2 65 66 67)
(foo3 65 66)
(foo3 65 66 67)
(foo3 65 66 67 68)
(putchar 10)
(foo4)
(foo4 65)
(foo4 65 66)
(foo5 65)
(foo5 65 66)
(foo5 65 66 67)
(foo6 65 66)
(foo6 65 66 67)
(foo6 65 66 67 68)
(putchar 10)

;;;options: -l min -f+ rest-param
;;;fancy-compiler
;;;expected:
;;;()(A)(AB)A()A(B)A(BC)AB()AB(C)AB(CD)
;;;()(A)(AB)A()A(B)A(BC)AB()AB(C)AB(CD)
