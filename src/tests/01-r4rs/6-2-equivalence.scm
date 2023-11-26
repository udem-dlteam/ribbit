(display (eqv? 'a 'a))
(newline)

(display (eqv? 'a 'b))
(newline)

(display (eqv? 2 2))
(newline)

(display (eqv? '() '()))
(newline)

(display (eqv? '10000 '10000))
(newline)

(display (eqv? (cons 1 2) (cons 1 2)))
(newline)

(display (eqv? (lambda () 1) (lambda () 2)))
(newline)

(display (eqv? #f 'nil))
(newline)

(let ((p (lambda (x) x)))
  (display (eqv? p p))
  (newline))

(define gen-counter
 (lambda ()
   (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(let ((g (gen-counter))) 
  (display (eqv? g g))
  (newline))

(display (eqv? (gen-counter) (gen-counter)))
(newline)

(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (display (eqv? f g))
  (newline))

(display (eq? 'a 'a))
(newline)

(display (eq? (list 'a) (list 'a)))
(newline)

(display (eq? '() '()))
(newline)

(display (eq? car car))
(newline)

(let ((x '(a))) (display (eq? x x)))
(newline)

(let ((x '#())) (display (eq? x x)))
(newline)

(let ((x (lambda (x) x))) (display (eq? x x)))
(newline)

(define test-eq?-eqv?-agreement
  (lambda (obj1 obj2)
    (cond ((eq? (eq? obj1 obj2) (eqv? obj1 obj2)))
	  (else
	   (display "eqv? and eq? disagree about ")
	   (write obj1)
	   (display #\space)
	   (write obj2)
	   (newline)
       (crash)))))

(test-eq?-eqv?-agreement '#f '#f)
(test-eq?-eqv?-agreement '#t '#t)
(test-eq?-eqv?-agreement '#t '#f)
(test-eq?-eqv?-agreement '(a) '(a))
(test-eq?-eqv?-agreement '(a) '(b))
(test-eq?-eqv?-agreement car car)
(test-eq?-eqv?-agreement car cdr)
(test-eq?-eqv?-agreement (list 'a) (list 'a))
(test-eq?-eqv?-agreement (list 'a) (list 'b))
(test-eq?-eqv?-agreement '#(a) '#(a))
(test-eq?-eqv?-agreement '#(a) '#(b))
(test-eq?-eqv?-agreement "abc" "abc")
(test-eq?-eqv?-agreement "abc" "abz")

(display (equal? 'a 'a))
(newline)

(display (equal? '(a) '(a)))
(newline)

(display (equal? '(a (b) c) '(a (b) c)))
(newline)

(display (equal? "abc" "abc"))
(newline)

(display (equal? 2 2))
(newline)

(display (equal? (vector 'a 'a 'a 'a 'a) (vector 'a 'a 'a 'a 'a)))
(newline)


;;;options: -l r4rs
;;;expected:
;;;#t
;;;#f
;;;#t
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
;;;#t
;;;#t
;;;#f
;;;#f
;;;#t
;;;#f
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
