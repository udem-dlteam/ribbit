(define-expander 
  (do expr expand-expr)
  (if (or (not (pair? expr)) (eq? (car expr) 'set!))
    (error "*** do cannot be used as a variable or be assigned."))

  (let ((variables (cadr expr))
        (test (caddr expr))
        (body (cdddr expr)))
    (expand-expr
      `(let %%it-do-be-doing
         ,(map (lambda (variable)
                 (list (car variable) (cadr variable)))
               variables)
         (if ,(car test)
           (begin 
             ,@(cdr test))
           (begin
             ,@body
             (%%it-do-be-doing 
              ,@(map (lambda (variable)
                       (if (pair? (cddr variable))
                         (caddr variable)
                         (car variable)))
                     variables))))))))



