(define (create-n n)
(do ((n n (- n 1))
     (a '() (cons '() a)))
    ((= n 0) a)))

(define (iterative-div2 l)
(do ((l l (cddr l))
     (a '() (cons (car l) a)))
    ((null? l) a)))

(define input (create-n 100))

(run-bench
    (lambda () (iterative-div2 input))
    (lambda (result)(equal? (length result) 50)))
