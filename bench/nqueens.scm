(define my-append
 (lambda (x y)
   (if (null? x)
     y
     (cons (car x) (my-append (cdr x) y)))))

(define listn (lambda (n)
                (if (= n 0)
                  '()
                  (cons n (listn (- n 1))))))

(define one-to (lambda (n)
  (reverse (listn n))))

(define my-try (lambda (x y z)
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
      (if (not (= (car placed) (+ row dist)))
        (if (not (= (car placed) (- row dist)))
          (ok? row (+ dist 1) (cdr placed))
          (= 1 2))
        (= 1 2)))))

(define nqueens (lambda (n)
  (my-try (one-to n) '() '())))

(nqueens 8)
