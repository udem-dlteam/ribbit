;;
;; Copyright Léonard Oest O'Leary. All rights reserved.
;;

(define-macro (when-debug . exprs)
    ;`(begin ,@exprs)
    #f
    )

(define (display-rib rib depth)
  (if (> depth 0)
    (begin
      (display "[")
      (cond ((not (rib? rib))
             (display rib))
            ((rib? (field0 rib))
             (display-rib (field0 rib) (- depth 1)))
            (else
              (display (field0 rib))))
      (display " ")
      (cond ((not (rib? rib))
             (display rib))
            ((rib? (field1 rib))
             (display-rib (field1 rib) (- depth 1)))
            (else
              (display (field1 rib))))
      (display " ")
      (cond ((not (rib? rib))
             (display rib))
            ((rib? (field2 rib))
             (display-rib (field2 rib) (- depth 1)))
            (else
              (display (field2 rib))))
      (display "]"))
    (display "...")))


;; Besoin de 3 opérations : 
;;  - Création d'un nouveau noeud `!!`
;;  - Accès à une variable réactive `(x)` (appel de fonction)
;;  - modification d'une variable réactive `(! x y)` (appel de fonction avec 1 argument)

(define %%scope #f)

(define (node-cache-init! node)
  (##field1-set! 
   (##field0 node) 
   (##rib 
    0 ;; value
    0 ;; thunk
    (##rib 
     eqv?  ;; eq?
     '()  ;; sources
     '())))) ;; targets

(define (node-cache node)
  (##field1 (##field0 node)))

;; Value of the node, stored in [X _ [_ _ _]]
(define (node-value node)
  (car (node-cache node)))

(define (node-value-set! node value)
  (set-car! (node-cache node) value))

;; Thunk of the node, stored in [_ X [_ _ _]]
(define (node-thunk node)
  (##field1 (node-cache node)))

(define (node-thunk-set! node value)
  (##field1-set! (node-cache node) value))

;; Eq function of the node, stored in [_ _ [X _ _]]
(define (node-eq node)
  (##field0 (##field2 (node-cache node))))

(define (node-eq-set! node node-eq?)
  (##field0-set! (##field2 (node-cache node)) node-eq?))

;; Sources of the node, stored in [_ _ [_ X _]]
(define (node-sources node)
  (##field1 (##field2 (node-cache node))))

(define (node-sources-set! node sources)
  (##field1-set! (##field2 (node-cache node)) sources))

;; Targets of the node, stored in [_ _ [_ _ X]]
(define (node-targets node)
  (##field2 (##field2 (node-cache node))))

(define (node-targets-set! node targets)
  (##field2-set! (##field2 (node-cache node)) targets))

;; Add a link between two nodes
(define (node-link-add! source target)
  (when-debug
    (display "Trying to add link from ")
    (display (get-debug-name source))
    (display " to ")
    (display (get-debug-name target))
    (display "..."))
  (let ((targets (node-targets source)))
    (if (not (memq target targets))
      (begin
        (when-debug
          (display "done")
          (newline))
        (node-targets-set! source (cons target targets))
        (node-sources-set! target (cons source (node-sources target))))
      (when-debug
        (display "already there")
        (newline)))))


;; remove a link between two nodes
(define (node-link-remove! source target)
  (when-debug
    (display "Trying to remove link from ")
    (display (get-debug-name source))
    (display " to ")
    (display (get-debug-name target))
    (display "..."))
  (let ((targets (node-targets source)))
    (if (memq target targets)
      (begin
        (when-debug (display "done") (newline))
        (node-targets-set! source (filter (lambda (x) (not (eqv? target x))) (node-targets source)))
        (node-sources-set! target (filter (lambda (x) (not (eqv? source x))) (node-sources target))))
        
      (when-debug
        (display "already removed")
        (newline)))))



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
      (node-thunk-set! __x __thunk)
      (node-eq-set! __x ,eqv?)
      (node-value-set! __x (__thunk))
      __x))

(define-macro (!! . exprs)
  `(make-reactive-var! eqv? ,@exprs))

(define-macro (!!! . exprs)
  `(make-reactive-var! (lambda (x y) #f) ,@exprs))

(define (error msg)
  (display msg)
  (newline)
  (exit))

(define debug-vars '())
(define (get-debug-name node)
  (let ((var (assq node debug-vars)))
    (if var
      (cdr var)
      #f)))

(define-macro (!!d tag . exprs)
  `(let ((__debug (make-reactive-var! eqv? ,@exprs)))
     (set! debug-vars (cons (cons __debug ',tag) debug-vars))
     __debug))

(define-macro (!!!d tag . exprs)
  `(let ((__debug (make-reactive-var! (lambda (x y) #f) ,@exprs)))
     (set! debug-vars (cons (cons __debug ',tag) debug-vars))
     __debug))

(define debug-graph-count 0)
(define (gen-graph node)
  (define seen '())
  (define (dfs node)
    (if (not (memq node seen))
      (let ((targets (node-targets node))
            (sources (node-sources node)))
        (set! seen (cons node seen))
        (display (get-debug-name node))
        (display " [")
        (display "label=\"")
        (display (get-debug-name node))
        (display " (")
        (display (node-value node))
        (display ")\"")
        (display "];\n")


        (for-each 
          (lambda (x)
            (display (get-debug-name node))
            (display " -> ")
            (display (get-debug-name x))
            (display "[")
            (if (not (memq node (node-sources x)))
              (display " color=red"))
            (display "]")
            (display ";")
            (newline))
          targets)
        (for-each dfs sources)
        (for-each dfs targets))))

  (display "digraph G")
  (display debug-graph-count)
  (set! debug-graph-count (+ debug-graph-count 1))
  (display " {\n")
  (dfs node)
  (display "}"))

(define (debug-display x)
  (display "<")
  (display (get-debug-name x))
  (display " ")
  (display "targets=")
  (display (map get-debug-name (node-targets x)))
  (display " sources=")
  (display (map get-debug-name (node-sources x)))
  (display " value=")
  (display (node-value x))
  (display ">")
  (newline))

(define-macro (%%reactive-closure self)
  `(lambda () 
     (if %%scope (node-link-add! ,self %%scope))
     (node-value ,self)))

(define-macro (%%update-closure self . exprs)
  `(lambda () 
     (for-each 
       (lambda (__y) (node-link-remove! __y ,self))
       (node-sources ,self))
     (scope-to ,self ,@exprs)))

(define (update! x)
  (let ((node-eq? (node-eq x))
        (thunk (node-thunk x))
        (value (node-value x)))
    (when-debug (display "Updating : ") (debug-display x))
    (let ((new-value (thunk)))
      (if (not (node-eq? value new-value))
        (begin
          (node-value-set! x new-value)
          ;; not ideal beacause it can cause two updates. 
          ;;  I should do a total order on the graph and then 
          ;;  update it.
          (for-each update! (node-targets x)))))))

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

;(define x (!!d x 5))
;(define y (!!d y 10))
;(define z (!!d z (+ (x) (y))))
;
(define d display)
(define d (lambda (x) #f))
;
;(!!!d display_z
;  (d "The value of z is now :")
;  (d (z))
;  (d "\n"))
;
;
;;(debug-display x)
;;(debug-display y)
;;(debug-display z)
;(! x 7)
;;(debug-display x)
;;(debug-display y)
;;(debug-display z)
;
;;(debug-display x)
;;(! x 8) 
;(! x (y))
;(! y 20)
;;;(! y 20)
;(! x 40)
;
;(gen-graph x)
;(let loop ((i 1000))
;  (if (> i 0)
;    (begin 
;      (! x i)
;      (loop (- i 1)))))
;

(let ((pi (!!d pi 1))
      (foo (!!d foo 2)))
  (! pi (display (pi)) (newline) (+ (pi) (pi)))
  (debug-display pi))

(gen-graph pi)

;(define (pp x) #f)
;;(define (pp x) (display x) (newline))
;(define on? (!!d is_on (pp "updating on?") #f))
;(define on  (!!d on (pp "updating on")  "The light is on!"))
;(define off (!!d off (pp "updating off") "It's dark ?!?"))
;(define on/off (!!d on_or_off (pp "updating on/off") (if (on?) (on) (off))))
;
;(!!!d display_on_or_off 
;  (d (on/off))
;  (d "\n"))
;
;
;;(display (node-targets on))
;(gen-graph on/off)
;(! on "Wow! there is a lot of light here")
;(! on "Wow! here is a lot of light")
;;(display (node-targets on))
;(! on? #t)
;;(display (node-targets on))
;(! on? #f)
;;(display (node-targets on))
;(! on? #t)
;;(display (node-targets on))
;(! on? #f)








  

