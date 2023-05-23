(##include "./types.scm")
(##include "./bool.scm")
(##include "./pair-list.scm")

(cond-expand
  ((host js)

   (define-primitive
     (apply f args)
     "() => {
        let num_args = 0;
        let arg = pop();
        // we don't push the function, because it is already on top of the stack
        while (arg !== NIL) {
            push(arg[0]);
            arg = arg[1];
            num_args++;
        }
        push(num_args); // @@(feature arity-check)@@
        pc = [0,0,[0,num_args + 1,pc[2]]];
        return true;
     }, "))

  ((host c)
   (define-primitive
     (apply f args)
     "{
     PRIM1();
     int num_args = 0;
     obj arg = TAG_RIB(x);
     // we don't push the function, because it is already on top of the stack
     while (arg != NIL) {
        push2(CAR(arg), PAIR_TAG);
        arg = TAG_RIB(CDR(arg));
        num_args++;
     }
     push2(TAG_NUM(num_args), PAIR_TAG);
     pc = TAG_RIB(alloc_rib(0,0,TAG_RIB(alloc_rib(TAG_NUM(0), TAG_NUM(num_args + 1), TAG(pc)))));
     break;
     }"))

  ((host hs)))

;; Control features (R4RS section 6.9).

(define (make-procedure code env) (rib code env procedure-type))
(define procedure-code field0)
(define procedure-env field1)

(define (map proc lst)
    (if (pair? lst)
      (cons (proc (car lst)) (map proc (cdr lst)))
      '()))

(define (##map proc . lsts))

(define (for-each proc lst)
  (if (pair? lst)
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))
      #f))

(define (fold func base lst)
  (if (pair? lst)
    (fold func (func base (car lst)) (cdr lst))
    base))

(define (lazy-fold func base lst (stop-value '()))
  (if (and (pair? lst) (not (equal? base stop-value)))
    (lazy-fold func (func (car lst)) (cdr lst) stop-value)
    base))

(define (scan func base state lst)
  (if (pair? lst)
    (scan func (car lst) (func base (car lst)) (cdr lst))
    state))

(define (lazy-scan func base state lst (stop-value '()))
  (if (and (pair? lst) (not (equal? state stop-value)))
    (lazy-scan func (car lst) (func base (car lst)) (cdr lst) stop-value)
    state))

;; First-class continuations.

(define (call/cc receiver)
  (let ((c (field1 (field1 (close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (field1 (field1 (close #f)))))
                  (field0-set! c2 (field0 c)) ;; set "stack" field
                  (field2-set! c2 (field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

