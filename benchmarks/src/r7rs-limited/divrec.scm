(define (create-n n)
(do ((n n (- n 1))
     (a '() (cons '() a)))
    ((= n 0) a)))

(define (recursive-div2 l)
(cond ((null? l) '())
      (else (cons (car l) (recursive-div2 (cddr l))))))

(define input (create-n 100))

(run-bench
    (lambda () (recursive-div2 input))
    (lambda (result) (equal? (length result) 50)))