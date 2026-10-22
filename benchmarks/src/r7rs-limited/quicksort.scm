;; This is probably from Lars Hansen's MS thesis.
;; The quick-1 benchmark.  (Figure 35, page 132.)

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (quick-1 v less?)

  (define (helper left right)
    (if (< left right)
        (let ((median (partition v left right less?)))
          (if (< (- median left) (- right median))
              (begin (helper left (- median 1))
                     (helper (+ median 1) right))
              (begin (helper (+ median 1) right)
                     (helper left (- median 1)))))
        v))

  (helper 0 (- (vector-length v) 1)))


(define (partition v left right less?)
  (let ((mid (vector-ref v right)))

    (define (uploop i)
      (let ((i (+ i 1)))
        (if (and (< i right) (less? (vector-ref v i) mid))
            (uploop i)
            i)))

    (define (downloop j)
      (let ((j (- j 1)))
        (if (and (> j left) (less? mid (vector-ref v j)))
            (downloop j)
            j)))

    (define (ploop i j)
      (let* ((i (uploop i))
             (j (downloop j)))
        (let ((tmp (vector-ref v i)))
          (vector-set! v i (vector-ref v j))
          (vector-set! v j tmp)
          (if (< i j)
              (ploop i j)
              (begin (vector-set! v j (vector-ref v i))
                     (vector-set! v i (vector-ref v right))
                     (vector-set! v right tmp)
                     i)))))

    (ploop (- left 1) right)))

;;; Benchmark was updated for ribbit. The previous list generation used floats
;;; which are not r4rs. The quicksort algorithm itself was not modified. NOTE:
;;; keep the vector size small since Ribbit uses lists internally for vectors

(define less? (hide 1 (lambda (x y) (< x y))))
(define (pseudorandom-list p)
    (define (loop i current acc)
        (if (= i p)
            (list->vector acc)
            (loop
                (+ i 1)
                (modulo (+ current (- p 1)) p)
                (cons current acc))))
    (loop 0 0 '()))

(define SIZE 499)

(run-bench
    (lambda () (quick-1 (pseudorandom-list (hide SIZE SIZE)) less?))
    (lambda (r) (equal? r (list->vector (iota (hide SIZE SIZE))))))