;; each node is an association list with the properties:
;; ((type [const | symbol | pair]) (value ...) (value-type ...))

(define-expander 
  (meta-expand expr expand-expr)

  (define (mk-meta meta-type value-type value)
    `((meta-type ,meta-type) (value-type ,value-type) (value ,value)))

  (define (meta-value-type meta)
    (cadr (assq 'value-type meta)))
  (define (meta-type meta)
    (cadr (assq 'meta-type meta)))
  (define (meta-value meta)
    (cadr (assq 'value meta)))

  (define known-symbols (map 
                          (lambda (sym) (mk-meta 'symbol (cadr sym) (car sym)))
                          (cadr expr)))

  (define (expand arg)
    (cond 
      ((number? arg)
       (mk-meta 'const 'number arg))
      ((string? arg)
       (mk-meta 'const 'string arg))
      ((vector? arg)
       (mk-meta 'const 'vector arg))
      ((char? arg)
       (mk-meta 'const 'char arg))
      ((null? arg)
       (mk-meta 'const 'null arg))
      ((boolean? arg)
       (mk-meta 'const 'boolean arg))
      ((procedure? arg)
       (mk-meta 'const 'procedure arg))
      ((symbol? arg) 
       (expand-symbol arg))
      ((pair? arg) (expand-pair arg))
      (else 
        (error ""))))

  (define (expand-symbol sym)
    (let ((sym-val (assq sym known-symbols)))
      (if (not sym-val)
        (mk-meta 'symbol 'unknown sym)
        (mk-meta 'symbol (meta-value-type (cadr sym-val)) sym))))

  (define (expand-call proc args)
    (let ((sym-val (assq sym known-symbols)))
      (if (not sym-val)
        (mk-meta 'symbol 'unknown sym)
        (mk-meta 'symbol (meta-value-type (cadr sym-val)) sym))))

  (define (expand-pair pair)
    (let ((first (car pair)))
      (cond

        ((eqv? first 'quote)
         )

        ((eqv? first 'set!) 
         (let ((var (assq (cadr pair) known-symbols))
               (val (expand (caddr pair))))
           (if var 
             (set-cdr! var val)
             (set! known-symbols (cons (list (cadr pair) val) known-symbols)))
           (mk-meta 'set! 'void (list (expand-symbol (cadr pair)) val))))

        ((eqv? first 'begin)
         (let ((body (map expand (cdr pair))))
           (mk-meta 'begin (meta-value-type (car (reverse body))) body)))

        ((eqv? first 'if)
         (let ((body (expand (cadr pair))))
           (mk-meta 'if (meta-value-type body) body)))

        ((eqv? first 'lambda)
         (let () 
           (mk-meta 'lambda 'procedure '...)))

        (else 
          (expand-symbol first)))))

  (if (or (not (pair? expr)) (eq? (car expr) 'set!))
    (error "*** meta-expand cannot be used as a variable or be assigned."))
  (if (null? (cddr expr)) (error "*** meta-expand must take at least one value."))
  `',(map 
       (lambda (arg) (expand (expand-expr arg)))
       (cddr expr)))

