;; tree-vect.scm
;;
;; This is a library implementing vectors as trees in Ribbit, exploiting all
;; the fields in a rib to store objects. Access in this structure is
;; logarithmic in the length of the vector, better than the linear access of
;; current vectors.
;;
;; Initial implementation from Marc Feeley, adapted for Ribbit by Léonard Oest
;; O'Leary.
;;
;; Copyright (c) 2025 Marc Feeley.
;; Copyright (c) 2025 Léonard Oest O'Leary.
;;


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


;(define (rib a b c) (vector a b c))
;(define (rib? obj) (vector? obj))
(define (rib-ref r i)
  (cond
    ((##eqv? i 0) (##field0 r)) ;; first field is the tree
    ((##eqv? i 1) (##field1 r)) ;; second field is the length
    ((##eqv? i 2) (##field2 r)) ;; third field is the tag
    (else (error "rib-ref: index out of bounds"))))

(define (rib-set! r i x)
  (cond
    ((##eqv? i 0) (##field0-set! r x)) ;; first field is the tree
    ((##eqv? i 1) (##field1-set! r x)) ;; second field is the length
    ((##eqv? i 2) (##field2-set! r x)) ;; third field is the tag
    (else (error "rib-set!: index out of bounds"))))

;(define (rib-set! r i x) (vector-set! r i x))

(define vect-tag 4) ;; tag for vectors

(define (vect? obj) (and (##rib? obj) (eqv? (##field2 obj) vect-tag)))
(define (vect-length v) (##field1 v))

(define base 3) ;; ribs have 3 fields
(define powers
  (list->vector
    (list 1
          3
          9
          27
          81
          243
          729
          2187
          6561
          19683
          59049
          177147
          531441
          1594323
          4782969
          14348907
          43046721
          129140163
          387420489)))

(define (make-node) (##rib 0 0 0))
(define (node-ref node i) (rib-ref node i))
(define (node-set! node i x) (rib-set! node i x))

(define (make-vect len init)

 (define (make-full-tree index depth) ;; depth >= 0
   (if (= depth 0)
       (init index)
       (let* ((tree (make-node))
              (depth (- depth 1))
              (pow (vector-ref powers depth)))
         (let loop ((index index) (j 0))
           (if (< j base)
               (begin
                 (node-set! tree j (make-full-tree index depth))
                 (loop (+ index pow) (+ j 1)))
               tree)))))

 (define (make-tree index depth len) ;; len >= 2, depth = (ceiling (log len base)) >= 1
   (let* ((tree (make-node))
          (depth (- depth 1))
          (pow (vector-ref powers depth)))
     (let loop1 ((index index) (len len) (j 0))
       (if (>= len pow)
           (begin
             (node-set! tree j (make-full-tree index depth))
             (loop1 (+ index pow) (- len pow) (+ j 1)))
           (begin
             (if (> len 0)
                 (node-set! tree
                            j
                            (if (= len 1)
                                (init index)
                                (make-tree
                                 index
                                 ;; determine depth of leftmost node
                                 (let loop2 ((depth (- depth 1)))
                                   (if (> len (vector-ref powers depth))
                                       (+ depth 1)
                                       (loop2 (- depth 1))))
                                 len))))
             tree)))))

 (##rib
  (cond ((= len 0)
         0)
        ((= len 1)
         (init 0))
        (else
         (make-tree 0
                    ;; determine depth of leftmost node
                    (let loop ((depth 0))
                      (if (> len (vector-ref powers depth))
                          (loop (+ depth 1))
                          depth))
                    len)))
  len
  vect-tag))

(define (vect-ref vect index)

 (define (full-tree-ref tree index depth) ;; depth >= 0
   (if (= depth 0)
       tree
       (let* ((depth (- depth 1))
              (pow (vector-ref powers depth))
              (i (quotient index pow))
              (start (* i pow))
              (tree (node-ref tree i)))
         (full-tree-ref tree (- index start) depth))))

 (define (tree-ref tree index depth len) ;; len >= 1, depth = (ceiling (log len base)) >= 0
   (if (= depth 0) ;; or equivalently (= len 1)
       tree
       (let* ((depth (- depth 1))
              (pow (vector-ref powers depth))
              (i (quotient index pow))
              (start (* i pow))
              (tree (node-ref tree i)))
         (if (< i (quotient len pow)) ;; in a full tree?
             (full-tree-ref tree (- index start) depth)
             ;; determine depth of leftmost node
             (let ((len (- len start)))
               (if (= len 1)
                   tree
                   (let loop ((depth (- depth 1)))
                     (if (> len (vector-ref powers depth))
                         (tree-ref tree (- index start) (+ depth 1) len)
                         (loop (- depth 1))))))))))

 (let ((len (rib-ref vect 1)))
   (tree-ref (rib-ref vect 0)
             index
             ;; determine depth of leftmost node
             (let loop ((depth 0))
               (if (> len (vector-ref powers depth))
                   (loop (+ depth 1))
                   depth))
             len)))

(define (vect-set! vect index val)

 (define (full-tree-set! node k index depth) ;; depth >= 0
   (if (= depth 0)
       (node-set! node k val)
       (let* ((depth (- depth 1))
              (pow (vector-ref powers depth))
              (i (quotient index pow))
              (start (* i pow))
              (node (node-ref node k)))
         (full-tree-set! node i (- index start) depth))))

 (define (tree-set! node k index depth len) ;; len >= 1, depth = (ceiling (log len base)) >= 0
   (if (= depth 0) ;; or equivalently (= len 1)
       (node-set! node k val)
       (let* ((depth (- depth 1))
              (pow (vector-ref powers depth))
              (i (quotient index pow))
              (start (* i pow))
              (node (node-ref node k)))
         (if (< i (quotient len pow)) ;; in a full tree?
             (full-tree-set! node i (- index start) depth)
             ;; determine depth of leftmost node
             (let ((len (- len start)))
               (if (= len 1)
                   (node-set! node i val)
                   (let loop ((depth (- depth 1)))
                     (if (> len (vector-ref powers depth))
                         (tree-set! node i (- index start) (+ depth 1) len)
                         (loop (- depth 1))))))))))

 (let ((len (rib-ref vect 1)))
   (tree-set! vect
              0
              index
              ;; determine depth of leftmost node
              (let loop ((depth 0))
                (if (> len (vector-ref powers depth))
                    (loop (+ depth 1))
                    depth))
              len)))

(define (vect->list vect)
 (let loop ((i (- (vect-length vect) 1)) (lst '()))
   (if (>= i 0)
       (loop (- i 1)
             (cons (vect-ref vect i) lst))
       lst)))

(define (vect-reverse! vect)
 (let loop ((i 0) (j (- (vect-length vect) 1)))
   (if (< i j)
       (let ((t (vect-ref vect i)))
         (vect-set! vect i (vect-ref vect j))
         (vect-set! vect j t)
         (loop (+ i 1) (- j 1))))))

