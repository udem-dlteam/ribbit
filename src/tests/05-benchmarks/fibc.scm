(define-macro (time . args)
  `(begin ,@args))
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
  (time (run-bench-aux run ok?)))(define (succ n) (+ n 1))
(define (pred n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (= 0 y)
      (k x)
      (addc (succ x) (pred y) k)))

(define (fibc x c)
  (if (= 0 x)
      (c 0)
      (if (= 0 (pred x))
          (c 1)
          (addc (call-with-current-continuation
                 (lambda (c) (fibc (pred x) c)))
                (call-with-current-continuation
                 (lambda (c) (fibc (pred (pred x)) c)))
                c))))

(run-bench
    (lambda () (call-with-current-continuation (lambda (c) (fibc 20 c))))
    (lambda (result) (= result 6765)))
((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
;;;bench-run: -l define-macro -l r4rs
;;;expected:
;;;ok
