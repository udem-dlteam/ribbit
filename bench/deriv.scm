(define my-map
  (lambda (f lst)
    (if (pair? lst)
        (cons (f (car lst)) (my-map f (cdr lst)))
        '())))

(define deriv
  (lambda (a)
    (if (not (pair? a))
        (if (eqv? a 'x) 1 0)
        (if (eqv? (car a) '+)
            (cons '+ (my-map deriv (cdr a)))
            (if (eqv? (car a) '-)
                (cons '- (my-map deriv (cdr a)))
                (if (eqv? (car a) '*)
                    (cons '* (cons a (cons '+ (my-map (lambda (a) (cons '/ (cons (deriv a) (cons a '())))) (cdr a))) ))
                    (if (eqv? (car a) '/)
                        (cons '- (cons (cons '/ (cons (deriv (cadr a)) (cons (caddr a) '())))
                                       (cons '/ (cons (cadr a) (cons '* (cons (caddr a) (cons (caddr a) (cons (deriv (caddr a)) '()))))))))
                        0)))))))

(define run-n
 (lambda (lo hi r)
 (if (< lo hi)
  (run-n (+ 1 lo) hi (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))
  r)))

(define run
 (lambda ()
  (run-n 0 204000 0)))

(run)
