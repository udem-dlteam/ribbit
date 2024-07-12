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
  (let* ((nargs-rib (##field0 node))
         (nargs (##field0 nargs-rib))
         (code (##field2 nargs-rib)))
    (##field0-set! 
     node
     (##rib ;; we need to recreate the "nargs-rib" because
            ;; sometimes ribbit reuses it in different
            ;; functions
      nargs
      (##rib 
       0 ;; value
       0 ;; thunk
       (##rib 
        eqv?  ;; eq?
        '()  ;; sources
        '())) ;; targets
      code))))

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

(define !!debug-name "a")
(define !!debug-count 0)

(define (!!debug-set! name)
  (set! !!debug-name name)
  (set! !!debug-count 0))

(define-macro (!! . exprs)
  `(!!d 
     (string-append 
       !!debug-name 
       (number->string 
         (let ((__c !!debug-count)) 
           (set! !!debug-count (+ 1 !!debug-count)) 
           __c)))
     ,@exprs))


;(define-macro (!! . exprs)
  ;`(make-reactive-var! eqv? ,@exprs))




(define-macro (!!force . exprs)
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
     (set! debug-vars (cons (cons __debug ,tag) debug-vars))
     __debug))

(define-macro (!!dforce tag . exprs)
  `(let ((__debug (make-reactive-var! (lambda (x y) #f) ,@exprs)))
     (set! debug-vars (cons (cons __debug ',tag) debug-vars))
     __debug))

(define debug-graph-count 0)
(define (gen-graph node port)
  (define seen '())
  (define disp (lambda (x) (display x port)))
  (define (dfs node)
    (if (not (memq node seen))
      (let ((targets (node-targets node))
            (sources (node-sources node)))
        (set! seen (cons node seen))
        (disp (get-debug-name node))
        (disp " [")
        (disp "label=\"")
        (disp (get-debug-name node))
        (disp " (")
        (disp (node-value node))
        (disp ")\"")
        (disp "];\n")


        (for-each 
          (lambda (x)
            (disp (get-debug-name node))
            (disp " -> ")
            (disp (get-debug-name x))
            (disp "[")
            (if (not (memq node (node-sources x)))
              (disp " color=red"))
            (disp "]")
            (disp ";\n"))
          targets)
        (for-each dfs sources)
        (for-each dfs targets))))

  (disp "digraph G")
  (disp debug-graph-count)
  (set! debug-graph-count (+ debug-graph-count 1))
  (disp " {\n")
  (dfs node)
  (disp "}"))


(define (!!debug-graph name x)
  (call-with-output-file (string-append "dot/" name)
    (lambda (port) (gen-graph x port))))

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

(define-macro (!list . args)
  (if (null? args)
    `'()
    `(!! (cons ,(car args) (!list . ,(cdr args))))))

(define (!car v)
  (car (v)))

(define (!cdr v)
  (cdr (v)))

(define (!cons a b)
  (!! (cons a b)))

(define (!dcons id a b)
  (!!d id (cons a b)))

(define (!nil)
  (!! '()))

(define (!dnil tag)
  (!!d tag '()))

(define (foo x)
  (let ((__foobar 0))
    (display __foobar)
    (set! __foobar 42)))

(define x (!cons (!! 1) 
                 (!cons (!! 2)
                        (!cons (!! 3)
                               (!nil)))))
(!!debug-set! "b")
(display "hello")

(define (display-lst name lst n)
  (if (null? (lst))
    (newline)
    (begin
      (!!
        (display name)
        (display "[")
        (display n)
        (display "]=")
        (display ((!car lst)))
        (display " ")
        (newline))
      (display-lst name (!cdr lst) (+ n 1)))))


;(define (!map func lst)
;  (if (null? (lst))
;    (!nil)
;    (!! 
;      (cons
;        (func (!car lst))
;        (!map func (!cdr lst))))))

(define (!map activate deactivate lst)
  (if (null? (lst))
    (!nil)
    (begin
      (!! (!map activate deactivate (!cdr lst)))
      (cons
        (activate (car lst))
        (!map activate deactivate (!cdr lst))))))



(define y 
  (!map 
    (lambda (x) (!! (* (x) (x))))
    (lambda (y) y)
    x))


(!!debug-set! "disp_x")
(display-lst "x" x 0)

(!! debug-set! "disp_y")
(display-lst "y" y 0)

(!!debug-graph "before" x)

;(! (!car x) 3)
;(! (!car (!cdr x)) 10)
;
;(!!debug-set! "c")
;(! (!cdr (!cdr (!cdr x))) (cons (!! 6) (!nil)))
;
;
;(!!debug-graph "after" x)

;(display (!car (!cdr (!cdr (!cdr x)))))

;(define _display display)
; (define display ##id)
; 
; (define x (!dcons 'item1 1 (!dcons 'item2 2 (!dcons 'item3 3 (!dnil 'nil)))))
; 
; (define end-x 
;   (!!d 'end 
;     (let loop ((lst x))
;       (if (null? (lst)) 
;         lst 
;         (loop (!cdr lst))))))
; 
; (!!d 'display_end
;   (display "the end is:")
;   (display ((end-x)))
;   (display "\n"))
; 
; (!!d 'display_list
;   (display "the list is : (")
;   (let loop ((lst x))
;     (if (null? (lst))
;       (begin (display ")") (newline))
;       (begin (display (!car lst)) (display " ") (loop (!cdr lst))))))
; 
; 
; (define display _display)


; (define display ##id)
; (! (end-x) (cons 30 (!nil)))

;(define (!add lst elem)
;  )



;(! (!cdr (!cdr x)) (cons 30 (!nil)))

;(display (!cdr x))


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
;(define d ##id)
;(define _display display)
;(define display ##id)
;
(define x (!! 5))
(define y (!! 10))
(define z (!! (+ (x) (y))))

(!! 
  (display "The value of z is now :")
  (display (z))
  (display "\n"))


(! x 7)
;
;(define display _display)
;(gen-graph x)
;(define display ##id)
;


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
;(let loop ((i 1000))
;  (if (> i 0)
;    (begin 
;      (! x i)
;      (loop (- i 1)))))
;

;(let ((foo (!! 1)))
;  (! foo 
;     (display (foo)) 
;     (newline) 
;     (+ (foo) (foo))))

;; Calcualting the square root of n
;; using reactive programming and 
;; newton series
;(let* ((n 1000)
;       (a (!! n)))
;  (! a 
;     (quotient 
;       (+ (a) (quotient n (a)))
;       2))
;  (display (a)))





;
;(gen-graph pi)

;(define (pp x) #f)
;;;(define (pp x) (display x) (newline))
;(define on? (!!d is_on (pp "updating on?") #f))
;(define on  (!!d on (pp "updating on")  "The light is on!"))
;(define off (!!d off (pp "updating off") "It's dark ?!?"))
;(define on/off (!!d on_or_off (pp "updating on/off") (if (on?) (on) (off))))
;
;(!!!d display_on_or_off 
;  (d (on/off))
;  (d "\n"))
;;
;;
;;;(display (node-targets on))
;
;(! on? #t)
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








  

