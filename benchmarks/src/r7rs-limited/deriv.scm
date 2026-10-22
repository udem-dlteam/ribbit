;;; DERIV -- Symbolic derivation.

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