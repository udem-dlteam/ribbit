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
  (time (run-bench-aux run ok?)));;; DERIV -- Symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (deriv a)
(cond ((not (pair? a))
       (if (eq? a 'x) 1 0))
      ((eq? (car a) '+)
       (cons '+
             (map deriv (cdr a))))
      ((eq? (car a) '-)
       (cons '-
             (map deriv (cdr a))))
      ((eq? (car a) '*)
       (list '*
              a
              (cons '+
                    (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
      ((eq? (car a) '/)
       (list '-
             (list '/
                   (deriv (cadr a))
                   (caddr a))
             (list '/
                   (cadr a)
                   (list '*
                         (caddr a)
                         (caddr a)
                         (deriv (caddr a))))))
      (else
       (error "No derivation method available"))))

(define the-expression '(+ (* 3 x x) (* a x x) (* b x) 5))

(run-bench
    (lambda () (deriv the-expression))
    (lambda (result)
        (equal? result
                '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                    (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                    (* (* b x) (+ (/ 0 b) (/ 1 x)))
                    0))))
((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
