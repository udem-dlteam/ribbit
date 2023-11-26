
(define x 12)

(display `(x ,x))
(newline) 

(define fruits '(apple banana cherry))
(define more-fruits '(orange kiwi))

(define combined-fruits `(grape ,@fruits ,@more-fruits))

(display combined-fruits)
(newline)

(display `(list ,(+ 1 2) 4))
(newline)

(display (let ((name 'a)) `(list ,name ',name)))
(newline)

(display `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(newline)

(display `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
(newline)

(define (add4 x)
  (+ x 4))

(display `#(10 5 ,(add4 4) ,@(map add4 '(16 9)) 8))
(newline)

(display `,(+ 2 3))
(newline)

(display `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(newline)

(display (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
(newline)

(display (quasiquote (list (unquote (+ 1 2)) 4)))
(newline)

(display '(quasiquote (list (unquote (+ 1 2)) 4)))
(newline)


;;;options: -l r4rs
;;;expected:
;;;(x 12)
;;;(grape apple banana cherry orange kiwi)
;;;(list 3 4)
;;;(list a (quote a))
;;;(a 3 4 5 6 b)
;;;((foo 7) . cons)
;;;#(10 5 8 20 13 8)
;;;5
;;;(a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f)
;;;(a (quasiquote (b (unquote x) (unquote (quote y)) d)) e)
;;;(list 3 4)
;;;(quasiquote (list (unquote (+ 1 2)) 4))
