(define-macro (when c . body) `(if ,c (begin ,@body)))

(define-macro (unless c . body) `(if (not ,c) (begin ,@body)))

(define-macro (hide x y)
    `(car
        (make-list
            (let ((&&x&& ,x)) (if (<= &&x&& 0) 1 &&x&&))
            ,y)))

(define-macro (import . rest) #f) ;;ignore imports

(define-macro (exact-integer? x) `(integer? ,x)) ;; no floats in ribbit yet

(define make-vector (let ((make-vector make-vector))
    (lambda (size . default)
        (if (null? default)
            (make-vector size)
            (list->vector (make-list size (car default)))))))

(define (iota n) (let loop ((i 0)) (if (< i n) (cons i (loop (+ i 1))) '())))

(define (expt b n)
    (define (expt-iter b n acc)
        (cond ((= n 0) acc)
                ((= (remainder n 2) 0) ; if n is even
                (expt-iter (* b b) (/ n 2) acc))
                (else ; n is odd
                (expt-iter b (- n 1) (* acc b)))))
        (expt-iter b n 1))(define repetitions 1)
(define %_____junk-data_____ (make-list 0 0))
(define (run-bench-aux run ok?)
  (let loop ((i repetitions) (result '(undefined)))
    (if (> i 0)
        (loop (- i 1) (run))
        (if (ok? result)
            (begin (display "ok\n") result)
            (error "wrong result")))))

(define (run-bench run ok?)
  (time (run-bench-aux run ok?)));;; NTAKL -- The TAKeuchi function using lists as counters,
;;; with an alternative boolean expression.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

;; Part of the fun of this benchmark is seeing how well the compiler
;; can understand this ridiculous code, which dates back to the original
;; Common Lisp.  So it probably isn't a good idea to improve upon it.

;; (define (shorterp x y)
;;   (and (not (null? y))
;;        (or (null? x)
;;            (shorterp (cdr x)
;;                      (cdr y)))))

  ;; But SML/NJ runs this benchmark about 15 times as fast when the
  ;; code above is rewritten as follows, so I tried it for Scheme also.

(define (shorterp x y)
(cond ((null? y) #f)
        ((null? x) #t)
        (else
        (shorterp (cdr x) (cdr y)))))

(run-bench
    (lambda ()
      (mas (hide 600 '(24 23 22 21 20 19 18 17 16 15 14 13 12 11
                       10  9  8  7  6  5  4  3  2  1))
           (hide 4 '(16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1))
           (hide 1 '(8 7 6  5  4  3  2  1))))
    (lambda (r) (equal? r '(9 8 7 6 5 4 3 2 1))))

((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
