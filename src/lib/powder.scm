;;
;; Copyright Léonard Oest O'Leary. All rights reserved.
;;


;; Powder is a reactive library for ribbit. It allows
;; the creation of reactive variables.

;
;(define rib ##rib)
;
;(define-macro (!! expr)
;  `(%create-node (lambda () ,expr)))
;
;(define (%%create-node thunk)
;  (set! %%dependencies '())
;  (let ((value (thunk)))
;    (create-node value thunk %%dependencies)))
;
;(define-macro (! v)
;  `(begin (set! %%dependencies (cons ,v %%dependencies))
;          (node-value ,v)))
;
;(define (create-node value thunk dependencies)
;  (##rib
;   thunk
;   value
;   dependencies))
;
;(define (node-update! x)
;  (let ((current-value (node-value x))
;        (current-thunk (node-thunk x))
;        (current-dependencies (node-dependencies x)))
;    (set! %%dependencies '())
;    (node-dependencies-set! x %%dependencies)
;    (let ((new-value (current-thunk)))
;      (if (not (equal? new-value current-value))
;        (begin
;          (node-value-set! x new-value)
;          (for-each current-dependencies node-update!))))))
;
;
;(define (!set! v value)
;   
;
;
;;; Thunk (what to compute)
;(define (node-thunk node)
;  (##field0 node))
;
;(define (node-thunk-set! node thunk)
;  (##field0-set! node thunk))
;
;;; Value of the node
;(define (node-value node)
;  (##field1 node))
;
;(define (node-value-set! node value)
;  (##field1-set! node value))
;
;;; Dependencies are stored in a list.
;(define (node-dependencies node)
;  (##field2 node))
;
;(define (node-dependencies-add! node dependency)
;  (##field2-set! node (cons dependency (node-dependencies node))))
;
;(define (node-dependencies-set! node dependencies)
;  (##field2-set! node dependencies))


;; Besoin de 3 opérations : 
;;  - Création d'un nouveau noeud `!!`
;;  - Accès à une variable réactive `(x)` (appel de fonction)
;;  - modification d'une variable réactive `(! x y)` (appel de fonction avec 1 argument)

(define %%scope #f)

(define (node-cache-init! node)
  (##field1-set! (##field0 node) (##rib 0 0 (##rib 0 0 0))))

(define (node-cache node)
  (##field1 (##field0 node)))

(define (node-value node)
  (car (node-cache node)))

(define (node-value-set! node value)
  (set-car! (node-cache node) value))

(define (node-thunk node)
  (##field1 (##field2 (node-cache node))))

(define (node-thunk-set! node value)
  (##field1-set! (##field2 (node-cache node)) value))

(define (node-eq node)
  (##field0 (##field2 (node-cache node))))

(define (node-eq-set! node node-eq?)
  (##field0-set! (##field2 (node-cache node)) node-eq?))

(define (node-dependencies node)
  (cdr (node-cache node)))

(define (node-dependencies-add! node dependency)
  (let ((deps (node-dependencies node)))
    (if (not (memq dependency deps))
      (node-dependencies-set! node (cons dependency deps)))))

(define (node-dependencies-remove! node dependency)
  (let ((deps (node-dependencies node)))
    (if (memq dependency deps)
      (node-dependencies-set! node (filter (lambda (x) (eq? x dependency)) deps)))))

(define (node-dependencies-set! node dependencies)
  (##field1-set! (node-cache node) dependencies))


(define-macro (scope-to scope . exprs)
  `(let ((old-%%scope %%scope))
     (set! %%scope ,scope)
     (let ((__return (begin ,@exprs)))
       (set! %%scope old-%%scope)
       __return)))

(define-macro (make-reactive-var! eqv? . exprs)
  `(letrec ((__x (%%reactive-closure __x))
            (__thunk (%%update-closure __x ,@exprs)))
      (node-cache-init! __x)
      (node-dependencies-set! __x '())
      (node-thunk-set! __x __thunk)
      (node-eq-set! __x ,eqv?)
      (node-value-set! __x (__thunk))
      __x))

(define-macro (!! . exprs)
  `(make-reactive-var! eqv? ,@exprs))

(define-macro (!!! . exprs)
  `(make-reactive-var! (lambda (x y) #f) ,@exprs))

(define-macro (%%reactive-closure self)
  `(lambda () 
     (if %%scope (node-dependencies-add! ,self %%scope))
     (node-value ,self)))

(define-macro (%%update-closure self . exprs)
  `(lambda () 
     (map 
       (lambda (x) (node-dependencies-remove! x ,self))
       (node-dependencies ,self))
     (scope-to ,self ,@exprs)))

(define (update! x)
  (let ((node-eq? (node-eq x))
        (deps (node-dependencies x))
        (thunk (node-thunk x))
        (value (node-value x)))
    (let ((new-value (thunk)))
      (if (not (node-eq? value new-value))
        (begin
          (node-value-set! x new-value)
          ;; not ideal beacause it can cause two updates. 
          ;;  I should do a total order on the graph and then 
          ;;  update it.
          (for-each update! deps))))))

(define-macro (! var . exprs)
  `(let ((__thunk (%%update-closure ,var ,@exprs)))
     (node-thunk-set! ,var __thunk)
     (update! ,var)))

;(define-macro (%%update-closure self node-eq? . exprs)
;   `(lambda (value)
;      (let ((deps (node-dependencies ,self))
;            (__return (begin ,@exprs)))
;        (if (not (,node-eq? value __return))
;          (let ((old-%%scope %%scope))
;            (node-value-set! ,self __return)
;            (for-each (node-dependencies ,self) 
;                      (lambda (x) ((node-thunk x))))
;        __return)))

(define x (!! 5))
(define y (!! 10))
(define z (!! (+ (x) (y))))

(!!!
  (display "The value of z is now :")
  (display (z))
  (newline))

(! x 7) 
(! x 8) 
(! x (y))
(! y 20)
; 
; (define pi (lambda () 100))
; (define pi (!! (+ 100000 (quotient 100000 (pi)))))
; (display (pi))

;(define (pp x) #f)
(define (pp x) (display x) (newline))
(define on? (!! (pp "updating on?") #f))
(define on  (!! (pp "updating on")  "The light is on!"))
(define off (!! (pp "updating off") "It's dark ?!?"))
(define on/off (!! (pp "updating on/off") (if (on?) (on) (off))))

(!!! (display (on/off)) (newline))

(display (node-dependencies on))
(! on "Wow! there is a lot of light here")
(! on "Wow! here is a lot of light here")
(display (node-dependencies on))
(! on? #t)
(display (node-dependencies on))
(! on? #f)
(display (node-dependencies on))
(! on? #t)
(display (node-dependencies on))
(! on? #f)








  

