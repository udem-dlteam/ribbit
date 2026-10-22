;;; PRIMES -- Compute primes less than n, written by Eric Mohr.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define  (interval-list m n)
  (if (> m n)
      '()
      (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                  '()
                  (if (= (remainder (car l) n) 0)
                      (remove-multiples n (cdr l))
                      (cons (car l)
                            (remove-multiples n (cdr l))))))))
    (if (null? l)
        '()
        (cons (car l)
              (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (sieve (interval-list 2 n)))

(run-bench
    (lambda () (primes<= (hide 10 10000)))
    (lambda (r) (and (equal? (length r) 1229) (equal? (list-ref r 123) 683))))
