
;; good ol' define-macro
(define-expander
  (define-macro expr expand-expr)
  (if (or (not (pair? expr)) (eq? (car expr) 'set!))
    (error "*** define-macro cannot be used as a variable or be assigned."))

  (if (pair? (cadr expr))  ;; (define-macro (foo x) ...)
    (let ((macro-name (caadr expr))
          (macro-body `(lambda (,@(cdadr expr))
                         ,@(cddr expr))))
      (expand-expr
        `(define-expander
           (,macro-name %%inner-expr %%inner-expand-expr)
           (if (symbol? %%inner-expr)
             (error "*** a macro cannot be used as a variable:" %%inner-expr))
           (%%inner-expand-expr (apply ,macro-body (cdr %%inner-expr))))))

    (let ((macro-name (cadr expr)) ;; (define-macro foo (lambda (x) ...))
          (macro-body (caddr expr)))
      (if (not (eq? (car macro-body) 'lambda))
        (error "*** define-macro: expected lambda exprsession" macro-body))
      (expand-expr
        `(define-expander
           (,macro-name %%inner-expr %%inner-expand-expr)
           (if (symbol? %%inner-expr)
             (error "*** a macro cannot be used as a variable:" %%inner-expr))
           (%%inner-expand-expr (apply ,macro-body (cdr %%inner-expr))))))))


;; defines an identifier macro used as a compile-time constant
(define-expander
  (define-const expr expand-expr)
  (cond
    ((not (pair? expr)) (error "*** define-const cannot be used as a variable."))

    ((eq? (car expr) 'set!) (error "*** define-const cannot be assigned."))

    (else
      (let ((name (cadr expr))
            (value (expand-expr (caddr expr))))
        (expand-expr
          `(define-expander 
             (,name %%inner-expr %%inner-expand-expr)
             (cond 
               ((not (pair? %%inner-expr)) ,value)

               ((eq? (car %%inner-expr) 'set!) (error ,(string-append "*** const " (symbol->string name) " cannot be assigned.")))

               (else
                 (error ,(string-append "*** const " (symbol->string name) " was called but it is not a procedure."))))))))))


;; sugar for defining the behavior of a procedure depending on the number of args passed to it.
(define-expander
  (define-expander-case expr expand-expr)
  (cond
    ((not (pair? expr)) (error "*** define-expander-case cannot be used as a variable."))

    ((eq? (car expr) 'set!) (error "*** define-expander-case cannot be assigned."))

    (else
      (let* ((expander-name (caadr expr))
             (args-name (cadadr expr))
             (expander-cases (cddr expr))
             (expander-cases (if (assq 'else expander-cases) ;; has else 
                               expander-cases
                               (append 
                                 expander-cases 
                                 `((else 
                                     (error 
                                       (string-append 
                                         "*** " 
                                         (symbol->string ',expander-name)
                                         " called with "
                                         (number->string (length ,args-name))
                                         " arguments. Number of arguments must be: " 
                                         ,(string-concatenate 
                                            (map number->string (map caar expander-cases)) ", ")))))))))
        (expand-expr 
          `(define-expander
             (,expander-name expr expand-expr)
             (cond
               ((not (pair? expr)) expr)

               ((eq? (car expr) 'set!) `(set! ,(cadr expr) ,(expand-expr (caddr expr))))

               (else 
                 (let ((,args-name (map expand-expr (cdr expr))))
                   (case (length ,args-name)
                     ,@expander-cases))))))))))



