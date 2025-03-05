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
  (time (run-bench-aux run ok?)))(define (append-to-tail! x y)
(if (null? x)
    y
    (let loop ((a x) (b (cdr x)))
      (if (null? b)
          (begin
            (set-cdr! a y)
            x)
          (loop b (cdr b))))))

(define (destructive n m)
(let ((l (do ((i 10 (- i 1)) (a '() (cons '() a)))
             ((= i 0) a))))
  (do ((i n (- i 1)))
      ((= i 0) l)
    (cond ((null? (car l))
           (do ((l l (cdr l)))
               ((null? l))
             (if (null? (car l))
               (set-car! l (cons '() '())))
             (append-to-tail! (car l)
                              (do ((j m (- j 1)) (a '() (cons '() a)))
                                  ((= j 0) a)))))
          (else
           (do ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
               ((null? l2))
             (set-cdr! (do ((j (quotient (length (car l2)) 2) (- j 1))
                            (a (car l2) (cdr a)))
                           ((zero? j) a)
                         (set-car! a i))
                       (let ((n (quotient (length (car l1)) 2)))
                         (cond ((= n 0)
                                (set-car! l1 '())
                                (car l1))
                               (else
                                (do ((j n (- j 1)) (a (car l1) (cdr a)))
                                    ((= j 1)
                                     (let ((x (cdr a)))
                                       (set-cdr! a '())
                                       x))
                                  (set-car! a i))))))))))))


(run-bench
    (lambda () (destructive 600 50))
    (lambda (result) (equal? result '((1 1 2)
            (1 1 1)
            (1 1 1 2)
            (1 1 1 1)
            (1 1 1 1 2)
            (1 1 1 1 2)
            (1 1 1 1 2)
            (1 1 1 1 2)
            (1 1 1 1 2)
            (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3)))))
((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
