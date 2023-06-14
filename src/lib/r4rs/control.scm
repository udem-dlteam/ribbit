(##include-once "./types.scm")
(##include-once "./bool.scm")
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

;; Control features (R4RS section 6.9).

(define (apply f args) (##apply f args))

(define (make-procedure code env) (##rib code env procedure-type))
(define (procedure-code x) (##field0 x))
(define (procedure-env x) (##field1 x))


(define (##map proc lst)
  (if (pair? lst)
    (cons (proc (car lst)) (##map proc (cdr lst)))
    '()))

(define (map proc . lsts)
  (if (pair? (car lsts))
    (cons (apply proc (##map car lsts))
          (apply map (append (list proc) (##map cdr lsts))))
    '()))

(define (for-each proc . lsts)
  (if (pair? (car lsts))
      (begin
        (apply proc (##map car lsts))
        (apply for-each (append (list proc) (##map cdr lsts))))
      #f))

(define (fold func base lst)
  (if (pair? lst)
    (fold func (func base (car lst)) (cdr lst))
    base))

;; (define (fold-until func base lst (stop-value '()))
;;   (if (and (pair? lst) (not (equal? base stop-value)))
;;     (fold-until func (func (car lst)) (cdr lst) stop-value)
;;     base))
;;
;; (define (scan func base state lst)
;;   (if (pair? lst)
;;     (scan func (car lst) (func base (car lst)) (cdr lst))
;;     state))

(define (scan-until func base state lst (stop-value '()))
  (if (and (pair? lst) (not (equal? state stop-value)))
    (scan-until func (car lst) (func base (car lst)) (cdr lst) stop-value)
    state))

;; First-class continuations.

(define (call/cc receiver)
  (let ((c (##field1 (##field1 (##close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (##field1 (##field1 (##close #f)))))
                  (##field0-set! c2 (##field0 c)) ;; set "stack" field
                  (##field2-set! c2 (##field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

