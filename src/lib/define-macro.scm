(define-expander
  (define-macro expr expand-expr)
  (if (pair? (cadr expr))  ;; (define-macro (foo x) ...)
    (let ((macro-name (caadr expr))
          (macro-body `(lambda (,@(cdadr expr))
                         ,@(cddr expr))))
      (expand-expr
        `(define-expander
           (,macro-name ##inner-expr ##inner-expand-expr)
           (if (symbol? ##inner-expr)
             (error "*** a macro cannot be used as a variable:" ##inner-expr))
           (##inner-expand-expr (apply ,macro-body (cdr ##inner-expr))))))

    (let ((macro-name (cadr expr)) ;; (define-macro foo (lambda (x) ...))
          (macro-body (caddr expr)))
      (if (not (eq? (car macro-body) 'lambda))
        (error "*** define-macro: expected lambda exprsession" macro-body))
      (expand-expr
        `(define-expander
           (,macro-name ##inner-expr ##inner-expand-expr)
           (if (symbol? ##inner-expr)
             (error "*** a macro cannot be used as a variable:" ##inner-expr))
           (##inner-expand-expr (apply ,macro-body (cdr ##inner-expr))))))))
