(define i-my-modulo (lambda ( a b acc)
                      (if (< a b)
                        a
                        (if (< (+ acc b) a)
                          (i-my-modulo a b (+ acc b))
                          (if (= (+ acc b) a)
                            0
                            (- a acc)
                            )))))

(define my-modulo
  (lambda (a b)
    (i-my-modulo a b 0)))

(define interval-list (lambda (m n)
                        (if (< m n)
                          (cons m (interval-list (+ 1 m) n))
                          (cons m '()))))

(define remove-multiples 
  (lambda (n l)
    (if (null? l)
      '()
      (if (= (my-modulo (car l) n) 0)
        (remove-multiples n (cdr l))
        (cons (car l)
              (remove-multiples n (cdr l)))))))

(define sieve (lambda (l)
                (if (null? l)
                  '()
                  (cons (car l)
                        (sieve (remove-multiples (car l) (cdr l)))))))

(define primes<= (lambda (n)
                   (sieve (interval-list 2 n))))
(define run
 (lambda ()
   (primes<= 100)))

(run)

