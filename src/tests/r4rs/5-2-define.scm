(define add3 (lambda (x) (+ x 3)))
(display (add3 3))
(newline)

(define first car)
(display (first '(1 2)))
(newline)

(define old-+ +)
(begin (begin (begin)
              (begin (begin (begin) (define + (lambda (x y) (list y x)))
                            (begin)))
              (begin))
       (begin)
       (begin (begin (begin) 
                     (display (add3 6))
                     (newline)
                     (begin))))
(set! + old-+)
(display (add3 6))
(newline)

(begin)
(begin (begin))
(begin (begin (begin (begin))))

(define x 34)
(define (foo) (define x 5) x)
(display (foo))
(newline)

(display x)
(newline)

(define foo (lambda () (define x 5) x))
(display (foo))
(newline)

(display x)
(newline)

(define (foo x) ((lambda () (define x 5) x)) x)

(display (foo 88))
(newline)

(display (foo 4))
(newline)

(display x)
(newline)


(display (letrec ((foo (lambda (arg)
					  (or arg (and (procedure? foo)
						       (foo 99))))))
			    (define bar (foo #f))
			    (foo #f)))
(newline)


(display (letrec ((foo 77)
				   (bar #f)
				   (retfoo (lambda () foo)))
			    (define baz (retfoo))
			    (retfoo)))
(newline)

;;;options: -l r4rs
;;;expected:
;;;6
;;;1
;;;(3 6)
;;;9
;;;5
;;;34
;;;5
;;;34
;;;88
;;;4
;;;34
;;;99
;;;77
