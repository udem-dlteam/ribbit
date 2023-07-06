(##include-once "./types.scm")
(##include-once "./pair-list.scm")

;; Vectors (R4RS section 6.8).

(define (vector-length x) (##field1 x))
(define (vector-ref vect i) (list-ref (##field0 vect) i))
(define (vector-set! vect i x) (list-set! (##field0 vect) i x))

(define (make-vector k) (list->vector (make-list k 0)))

(define (vector . args) (list->vector args))
