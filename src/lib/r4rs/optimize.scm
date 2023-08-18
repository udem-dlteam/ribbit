(define-expander
  (- expr expand-expr)
  (cond
    ((not (pair? expr)) expr)

    ((eq? (car expr) 'set!) `(set! ,(cadr expr) ,(expand-expr (caddr expr))))

    (else 
      (let ((args (map expand-expr (cdr expr))))
        (case (length args)
          ((1) (if (number? (car args)) 
                 (- (car args)) 
                 `(##- 0 ,(car args))))
          ((2) (if (and (number? (car args)) (number? (cadr args)))
                 (- (car args) (cadr args))
                 `(##- ,@args)))
          (else 
            (if (null? (filter (lambda (x) (not (number? x))) args))
              (apply - args)
              `(- ,@args))))))))

(define-expander
  (+ expr expand-expr)
  (cond
    ((not (pair? expr)) expr)

    ((eq? (car expr) 'set!) `(set! ,(cadr expr) ,(expand-expr (caddr expr))))

    (else 
      (let ((args (map expand-expr (cdr expr))))
        (case (length args)
          ((0) 0)
          ((1) (car args))
          ((2) (if (and (number? (car args)) (number? (cadr args)))
                 (+ (car args) (cadr args))
                 `(##+ ,@args)))
          (else 
            (if (null? (filter (lambda (x) (not (number? x))) args))
              (apply + args)
              `(+ ,@args))))))))


(define-expander
  (< expr expand-expr)
  (cond
    ((not (pair? expr)) expr)

    ((eq? (car expr) 'set!) `(set! ,(cadr expr) ,(expand-expr (caddr expr))))

    (else 
      (let ((args (map expand-expr (cdr expr))))
        (case (length args)
          ((1) #t)
          ((2) (if (and (number? (car args)) (number? (cadr args)))
                 (< (car args) (cadr args))
                 `(##< ,@args)))
          (else 
            (if (null? (filter (lambda (x) (not (number? x))) args))
              (apply < args)
              `(< ,@args))))))))


