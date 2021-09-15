(define my-append
  (lambda (x y)
    (if (null? x)
        y
        (cons (car x) (my-append (cdr x) y)))))

(define one-to-aux
  (lambda (n tail)
    (if (= n 0)
        tail
        (one-to-aux (- n 1) (cons n tail)))))

(define one-to
  (lambda (n)
    (one-to-aux n '())))

(define my-try
  (lambda (x y z)
    (if (null? x)
        (if (null? y) 1 0)
        (+ (if (ok? (car x) 1 z)
               (my-try (my-append (cdr x) y) '() (cons (car x) z))
               0)
           (my-try (cdr x) (cons (car x) y) z)))))

(define ok?
  (lambda (row dist placed)
    (if (null? placed)
        #t
        (if (= (car placed) (+ row dist))
            #f
            (if (= (car placed) (- row dist))
                #f
                (ok? row (+ dist 1) (cdr placed)))))))

(define nqueens
  (lambda (n)
    (my-try (one-to n) '() '())))

(define run-n
  (lambda (lo hi r)
    (if (< lo hi)
        (run-n (+ 1 lo) hi (nqueens 8))
        r)))

(define run
  (lambda ()
    (run-n 0 250 0)))

(run)
