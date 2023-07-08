(##include-once "./types.scm")
(##include-once "./pair-list.scm")

;; Vectors (R4RS section 6.8).

(define (vector-length x) (##field1 x))

(define-signature
  vector-length
  ((x
     guard: (vector? x)
     expected: "VECTOR")))



(define (vector-ref vect i) (list-ref (##field0 vect) i))

(define-signature
  vector-ref
  ((vect 
     guard: (vector? vect)
     expected: "VECTOR")
   (i
     guard: (and (integer? i) (< -1 i (vector-length vect)))
     expected: (string-append "INTEGER between 0 and " (number->string (vector-length vect))))))



(define (vector-set! vect i x) (list-set! (##field0 vect) i x))

(define-signature
  vector-set!
  ((vect 
     guard: (vector? vect)
     expected: "VECTOR")
   (i
     guard: (and (integer? i) (< -1 i (vector-length vect)))
     expected: (string-append "INTEGER between 0 and " (number->string (vector-length vect))))
   (x)))



(define (make-vector k) (list->vector (make-list k 0)))

(define-signature
  make-vector
  ((k
     guard: (integer? k)
     expected: "INTEGER")))


(define (vector . args) (list->vector args))
