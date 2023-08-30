(##include-once "./bool.scm")
(##include-once "./types.scm")
(##include-once "./pair-list.scm")

(cond-expand
  ((host js)

   (define-primitive
     (##apply f args)
     "() => {
        let num_args = 0;
        let arg = pop();
        let f = pop();
        while (arg !== NIL) {
            push(arg[0]);
            arg=arg[1];
            num_args++;
        }
        push(num_args); // @@(feature arity-check)@@
        return f;
     }, "))


  ((host py)

   (define-primitive
     (##apply f args)
     "lambda: [exec(compile(\"\"\"
_arg = pop()
globals()['_f_'] = pop()
num_args=0
while _arg is not NIL:
 push(_arg[0])
 _arg=_arg[1]
 num_args += 1
push(num_args) # @@(feature arity-check)@@
 \"\"\", __file__, 'exec')), _f_][1],"))


  ((host c)
   (define-primitive
     (##apply f args)
     "{
     PRIM2();
     int num_args = 0;
     obj arg = TAG_RIB(y);
     while (arg != NIL) {
        push2(CAR(arg), PAIR_TAG);
        arg = TAG_RIB(CDR(arg));
        num_args++;
     }
     push2(TAG_NUM(num_args), PAIR_TAG);
     return TAG_RIB(x);
     }"))

  ((host hs)
   (define-primitive
     (##apply f args)
     " ,  (do
            let numArgs = 0
            arg <- pop
            f <- pop
            let loop arg' numArgs' = do
              case arg' of
                ribNil -> do
                  push (head arg')
                  loop (tail arg') (numArgs' + 1)
                _ -> do
                  push numArgs' -- @@(feature arity-check)@@
                  return f)
        ")))

(define (##params proc)
  (let ((nb-args-raw (##field0 (##field0 proc))))
    (cons (odd? nb-args-raw) (##quotient nb-args-raw 2)))) ;; (cons variadic? nb-args)

(define (##can-call? proc nb-args)
  (let ((nb-params (##params proc)))
    (or (##eqv? nb-args (cdr nb-params))
        (and (car nb-params) (>= nb-args (cdr nb-params))))))

;; Control features (R4RS section 6.9).

(define (apply f args) (##apply f args))

(define-signature
  apply
  ((f 
     guard: (##can-call? f (length args))
     expected: (let ((params (##params f)))
                 (string-append "PROCEDURE called with " 
                                (number->string (length args)) " and taking " 
                                (if (##field0 params) "at least " "")
                                (number->string (##field1 params))
                                ". A PROCEDURE with a number of params equal to the number of args" )))

   (args 
     guard: (list? args)
     expected: "LIST")))



(define (make-procedure code env) (##rib code env procedure-type))
(define (procedure-code x) (##field0 x))
(define (procedure-env x) (##field1 x))


(define (##map proc lst)
  (if (pair? lst)
    (cons (proc (##field0 lst)) (##map proc (##field1 lst)))
    '()))

(define (map proc . lsts)
  (if (pair? (##field0 lsts))
    (cons (apply proc (##map car lsts))
          (apply ##ntc-map (append (list proc) (##map cdr lsts))))
    '()))

(define (for-each proc . lsts)
  (if (pair? (##field0 lsts))
      (begin
        (apply proc (##map car lsts))
        (apply ##ntc-for-each (append (list proc) (##map cdr lsts))))
      #f))


(define-signatures
  (map for-each)
  ((proc 
     guard: (##can-call? proc (length lsts))
     expected: "A PROCEDURE that takes a number of args equal to the number of LISTs")

   (lsts 
     rest-param:
     guard: (or (null? lsts) 
                (and (all list? lsts) 
                     (##scan-until-false 
                      (lambda (lst1 lst2) (##eqv? (length lst1) (length lst2)))
                      (car lsts)
                      #t
                      (cdr lsts))))
     expected: "LISTs with the same length")))



;; First-class continuations.


(define (call/cc receiver)
  (let ((c (##field1 (##field1 (##close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (##field1 (##field1 (##close #f)))))
                  (##field0-set! c2 (##field0 c)) ;; set "stack" field
                  (##field2-set! c2 (##field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

(define-signature 
  call/cc
  ((receiver 
     guard: (##can-call? receiver 1)
     expected: "A PROCEDURE that takes a one argument (a PROCEDURE)")))

(define call-with-current-continuation call/cc)


(define-macro 
  (make-generator . expr)
  `(let ((cont-yield #f)
         (cont-continue #f))

     (define-macro 
       (yield x)
       `(call/cc 
          (lambda (##continue)
            (set! cont-continue ##continue)
            (cont-yield ,x))))

     (lambda (v)
       (call/cc
         (lambda (##yield)
           (set! cont-yield ##yield)
           (if cont-continue
             (cont-continue v)
             (let ((result (begin ,@expr)))
               (cont-yield result))))))))


(define (gen-send generator value)
  (generator value))

(define (gen-next generator)
  (gen-send generator '()))



;; ---------------------- UTILS NOT IN R4RS ---------------------- ;;

(define (find predicate lst)
  (if (pair? lst)
    (if (predicate (car lst))
      (car lst)
      (find predicate (cdr lst)))
    #f))

(define (filter predicate lst)
  (let loop ((lst lst) (acc '()))
    (if (pair? lst)
      (loop (cdr lst) (if (predicate (car lst)) 
                        (cons (car lst) acc) 
                        acc))
      (reverse acc))))

(define (compose f g)
  (lambda args (f (apply g args))))

;; FIXME: Remove and change the procs that depend on it
(define (##fold func base lst)
  (if (pair? lst)
    (##fold func (func base (##field0 lst)) (##field1 lst))
    base))

(define (fold func base lst)
  (if (pair? lst)
    (fold func (func (##field0 lst) base) (##field1 lst))
    base))

(define (fold-until func base lst (stop-value '()))
  (if (and (pair? lst) (not (equal? base stop-value)))
    (fold-until func (func (car lst)) (cdr lst) stop-value)
    base))

(define (scan func base state lst)
  (if (pair? lst)
    (scan func (car lst) (func base (car lst)) (cdr lst))
    state))

(define (scan-until func base state lst (stop-value '()))
  (if (and (pair? lst) (not (equal? state stop-value)))
    (scan-until func (##field0 lst) (func base (##field0 lst)) (##field1 lst) stop-value)
    state))

(define (##scan-until-false func base state lst)
  (if (and (pair? lst) state)
    (##scan-until-false func (##field0 lst) (func base (##field0 lst)) (##field1 lst))
    state))

(define (all pred lst (state #t))
  (if (and (pair? lst) state)
    (all pred (##field1 lst) (pred (##field0 lst)))
    state))

(define (partial f . args)
  (lambda other-args (apply f (append args other-args))))

