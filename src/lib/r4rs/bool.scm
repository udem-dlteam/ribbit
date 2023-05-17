; Booleans && Equality (R4RS sections 6.1 and 6.2).

(define (not obj) (eqv? obj #f))

;; ---------------------- EQUALITY ---------------------- ;;

(define eq? eqv?)

(define (equal? x y)
  (or (eqv? x y)
      (and (rib? x)
           (if (eqv? (field2 x) singleton-type)
               #f
               (and (rib? y)
                    (equal? (field2 x) (field2 y))
                    (equal? (field1 x) (field1 y))
                    (equal? (field0 x) (field0 y)))))))
