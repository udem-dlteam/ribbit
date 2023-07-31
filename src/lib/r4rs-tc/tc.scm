(define-macro (tc-pair-type) 0)

(define-macro 
  (tc-pair? o)
  `(and (##rib? ,o) (##eqv? (##field2 ,o) (tc-pair-type))))

(define (##tc-append lst1 lst2)
  (if (tc-pair? lst1)
    (##rib (##field0 lst1) (##tc-append (##field1 lst1) lst2) (tc-pair-type)) ;; cons
    lst2))

(define (##tc-list . args) args)

;; ntc means "no type check"
(define-macro
  (define-signature proc args-info)
  (let ((ntc-proc (string->symbol (string-append "##ntc-" (symbol->string proc))))
        (variadic? #f))
    `(begin
       (set! ,ntc-proc ,proc)
       (define 
         ,(let loop ((args (list proc)) (rest args-info))
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
                       (cons `(if (##eqv? ,guard #f)  ;; not
                                (error "In procedure " ',proc " : (ARGUMENT " ,i ") " ,expected " expected."))
                             guards) 
                       (+ i 1)
                       (cdr rest)))
                   (loop guards (+ i 1) (cdr rest))))
               (reverse guards)))
         ,(if variadic? 
            (let ((reverse-args (reverse args-info)))
              `(##apply ,ntc-proc (##tc-append (##tc-list ,@(reverse (map car (cdr reverse-args)))) ,(caar reverse-args))))
            `(,ntc-proc ,@(map car args-info)))))))


(define-macro
  (define-signatures procs common-signature)
  `(begin 
     ,@(map (lambda (proc) `(define-signature ,proc ,common-signature)) procs)))


