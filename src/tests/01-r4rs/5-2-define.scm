(define add3 (lambda (x) (+ x 3)))
(display (add3 3)) ;; 6
(newline)

(define first car)
(display (first '(1 2))) ;; (1 2)   
(newline)

(define old-+ +)
(begin (begin (begin)
              (begin (begin (begin) (define + (lambda (x y) (list y x)))
                            (begin)))
              (begin))
       (begin)
       (begin (begin (begin) 
                     (display (add3 6)) ;; (3 6)
                     (newline)
                     (begin))))
(set! + old-+)
(display (add3 6)) ;; 9
(newline)

(begin)
(begin (begin))
(begin (begin (begin (begin))))

(define x 34)

(define (foo) (define x 5) x)
(display (foo)) ;; 5
(newline)

(display x) ;; 34
(newline)


(define (foo x) ((lambda () (define x 5) x)) x)

(display (foo 88)) ;; 88
(newline)

(display (foo 4)) ;; 4
(newline)

(display x) ;; 34
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
;;;88
;;;4
;;;34
;;;99
;;;77
