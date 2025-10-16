(%%include-once (ribbit "expander-utils.scm"))


;; %%%%%%%%%% Booleans && Equality (R4RS sections 6.1 and 6.2) %%%%%%%%%% ;;

(define-expander-case
  (equal? args)
  ((2) (if (or (number? (car args)) (number? (cadr args)))
         `(%%eqv? ,@args)
         `(equal? ,@args))))

(define-expander-case
  (eq? args)
  ((2) `(%%eqv? ,@args)))

;; %%%%%%%%%% Numbers (R4RS section 6.5) %%%%%%%%%% ;;

(define-expander-case 
  (- args)
  ((1) (if (number? (car args)) 
         (- (car args)) 
         `(%%- 0 ,(car args))))
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (- (car args) (cadr args))
         `(%%- ,@args)))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply - args)
      `(- ,@args))))


(define-expander-case
  (+ args)
  ((0) 0)
  ((1) (car args))
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (+ (car args) (cadr args))
         `(%%+ ,@args)))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply + args)
      `(+ ,@args))))


(define-expander-case
  (< args)
  ((1) #t)
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (< (car args) (cadr args))
         `(%%< ,@args)))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply < args)
      `(< ,@args))))

(define-expander-case
  (> args)
  ((1) #t)
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (< (car args) (cadr args))
         `(%%< ,@(reverse args))))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply > args)
      `(< ,@(reverse args)))))

(define-expander-case
  (<= args)
  ((1) #t)
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (<= (car args) (cadr args))
         `(not (%%< ,@(reverse args)))))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply <= args)
      `(not (< ,@(reverse args))))))

(define-expander-case
  (>= args)
  ((1) #t)
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (> (car args) (cadr args))
         `(not (%%< ,@args))))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply >= args)
      `(not (< ,@args)))))

(define-expander-case
  (= args)
  ((1) #t)
  ((2) (if (and (number? (car args)) (number? (cadr args)))
         (= (car args) (cadr args))
         `(%%eqv? ,@args)))
  (else 
    (if (null? (filter (lambda (x) (not (number? x))) args))
      (apply = args)
      `(= ,@args))))





;; %%%%%%%%%% Types (R4RS section 3.4 + others) %%%%%%%%%% ;;

;; %%%%%%%%%% Case (R4RS section 4.2.1) %%%%%%%%%% ;;


;; %%%%%%%%%% Quasiquotes (R4RS section 4.2.6) %%%%%%%%%% ;;


;; %%%%%%%%%% Pairs and lists (R4RS section 6.3) %%%%%%%%%% ;;

(define-expander-case
  (cons args)
  ((2) `(%%rib ,(car args) ,(cadr args) 0)))

(define-expander-case
  (car args)
  ((1) `(%%field0 ,(car args))))

(define-expander-case
  (cdr args)
  ((1) `(%%field1 ,(car args))))

(define-expander-case
  (caar args)
  ((1) `(%%field0 (%%field0 ,(car args)))))

(define-expander-case
  (cadr args)
  ((1) `(%%field0 (%%field1 ,(car args)))))

(define-expander-case
  (cddr args)
  ((1) `(%%field1 (%%field1 ,(car args)))))


(define-expander-case
  (set-car! args)
  ((2) `(%%field0-set! ,@args)))

(define-expander-case
  (set-cdr! args)
  ((2) `(%%field1-set! ,@args)))

;; %%%%%%%%%% Numbers (R4RS section 6.5) %%%%%%%%%% ;;


;; %%%%%%%%%% Characters (R4RS section 6.6) %%%%%%%%%% ;;


;; %%%%%%%%%% Strings (R4RS section 6.7) %%%%%%%%%% ;;


;; %%%%%%%%%% Vectors (R4RS section 6.8) %%%%%%%%%% ;; 

(define-expander-case
  (vector-length args)
  ((1) `(%%field1 ,(car args))))


(define-expander-case 
  (vector-ref args) 
  ((2) `(list-ref (%%field0 ,(car args)) ,(cadr args))))

(define-expander-case 
  (vector-set! args) 
  ((3) `(list-set! (%%field0 ,(car args)) ,(cadr args) ,(caddr args))))

(define-expander-case 
  (make-vector args) 
  ((1) 
   (let ((k (car args))
         (vector-type 4))
     (cond 
       ((number? k)
        `(%%rib ,(vector->list (make-vector k 0)) ,k ,vector-type))
       (else
         `(%%rib (make-list ,k 0) ,k ,vector-type))))))

(define-expander-case
  (vector args)
  ((0) '#())
  (else 
     `(%%rib ,args ,(length args) 4)))


;; %%%%%%%%%% Control (R4RS section 6.9) %%%%%%%%%% ;;


;; %%%%%%%%%% I/O (R4RS section 6.10) %%%%%%%%%% ;;
