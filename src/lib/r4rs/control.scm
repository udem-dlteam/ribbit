(##include "./types.scm")
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

;; (define (apply proc args)
;;   (define (##push stack x)
;;       (field0-set! stack (rib x (field0 stack) 0))) ;; "push" manually 
;;
;;   (let* (
;;          (c (field1 (field1 (close #f)))) ;; get call/cc continuation rib
;;          (stack (field1 (close #f)))
;;          (return-pc (field2 c)))

    #| ;; (field0-set! c (rib (field0 c) (field1 c) return-pc)) ;; "set" the return pc

    (##push proc) ;; "push" the procedure 
    (for-each (lambda (arg) (##push arg)) args) ;; "push" each arg 


    (field2-set! c (rib 0 (+ (length args) 1)  ;; set "pc" field 
                       return-pc)) ;; set the continuation to the return-pc
    ;; (field2-set! c (field2 return-pc)) |#


    ;; (set! real-args (fold (lambda (arg arg-lst) (rib arg arg-lst 0)) (rib 0 (field0 proc) 0) args)) 
    ;; (set! result (make-procedure 
    ;;                (rib 0 '() 
    ;;                     (rib 0 (make-procedure (rib (+ (length args) 5) '() (field2 (field0 proc))) '()) (field2 c)))
    ;;                '()))
    ;; (field1-set! stack (field1 real-args))
    ;; (field0-set! stack (field0 real-args))

    ;; (for-each (lambda (arg) (##push c arg)) args) ;; "push" each arg 
    ;; (if ##feature-arity-check
    ;;   (##push c (length args))) ;; "push" the number of args

    ;; ((make-procedure (rib 0 '()
    ;;                       (rib 0 (make-procedure (rib (length args) '() (field2 (field0 proc))) '()) 0)
    ;;                       '())))

  ;;   ((make-procedure (rib 0 '() (rib 0 (make-procedure (rib (* 2 (length args)) '() (field2 (field0 proc))) '()) 
  ;;                                    (rib 5 0 0) ;; changer pour la vraie continuation, là ça fait juste halt
  ;;                                    )) '()))
  ;; ))

;; (define (map proc . lsts)
;;   (if (null? lsts)
;;     (error "Map must contain at least one list")
;;     (let loop ((new-list '()))
;;       ))

(define (for-each proc lst)
  (if (pair? lst)
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))
      #f))

(define (fold func base lst)
  (if (pair? lst)
    (fold func (func (car lst) base) (cdr lst))
    base))

;; First-class continuations.

(define (call/cc receiver)
  (let ((c (field1 (field1 (close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (field1 (field1 (close #f)))))
                  (field0-set! c2 (field0 c)) ;; set "stack" field
                  (field2-set! c2 (field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

