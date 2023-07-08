(define (##append lst1 lst2)
  (if (pair? lst1)
    (cons (##field0 lst1) (##append (##field1 lst1) lst2))
    lst2))

(define (##list . args) args)

(define (##not o)
  (##eqv? o #f))

(define-macro
  (define-signature proc-name args-info)
  (let ((mangled-name (string->symbol (string-append "==##==" (symbol->string proc-name) "==##==")))
        (variadic? #f))
    `(begin
       (set! ,mangled-name ,proc-name)
       (define 
         ,(let loop ((args (list proc-name)) (rest args-info))
            (if (pair? rest)
              (let ((arg-name (caar rest))
                    (default (let ((maybe-default (memq 'default: (car rest))))
                               (and maybe-default (cadr maybe-default))))
                    (rest-param (memq 'rest-param: (car rest))))
                (if rest-param
                  (if default
                    (error "A rest param cannot have a default value. Got: " default)
                    (begin 
                      (set! variadic? #t)
                      (append (reverse args) arg-name)))
                  (loop (cons
                          (if default
                            (list arg-name default)
                            arg-name)
                          args) 
                        (cdr rest))))
              (reverse args)))
         ,@(let loop ((guards '()) (i 1) (rest args-info))
             (if (pair? rest)
               (let* ((arg-info (car rest))
                      (arg-name (car arg-info))
                      (guard (let ((maybe (memq 'guard: arg-info)))
                               (and maybe (cadr maybe))))
                      (expected (let ((maybe (memq 'expected: arg-info)))
                                  (and maybe (cadr maybe)))))
                 (if guard 
                   (if (not expected)
                     (error "You must define the 'expected' field when defining a guard")
                     (loop 
                       (cons `(if (##not ,guard) 
                                (error "In procedure " ',proc-name " : (ARGUMENT " ,i ") " ,expected " expected."))
                             guards) 
                       (+ i 1)
                       (cdr rest)))
                   (loop guards (+ i 1) (cdr rest))))
               (reverse guards)))
         ,(if variadic? 
            (let ((reverse-args (reverse args-info)))
              `(##apply ,mangled-name (##append (##list ,@(reverse (map car (cdr reverse-args)))) ,(caar reverse-args))))
            `(,mangled-name ,@(map car args-info)))))))


(define-macro
  (define-signatures procs common-signature)
  `(begin 
     ,@(map (lambda (proc) `(define-signature ,proc ,common-signature)) procs)))


