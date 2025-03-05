(define-macro (time . args)
  `(begin ,@args))
(define-macro (when c . body) `(if ,c (begin ,@body)))

(define-macro (unless c . body) `(if (not ,c) (begin ,@body)))

(define-macro (hide x y)
    `(car
        (make-list
            (let ((&&x&& ,x)) (if (<= &&x&& 0) 1 &&x&&))
            ,y)))

(define-macro (import . rest) #f) ;;ignore imports

(define-macro (exact-integer? x) `(integer? ,x)) ;; no floats in ribbit yet

(define make-vector (let ((make-vector make-vector))
    (lambda (size . default)
        (if (null? default)
            (make-vector size)
            (list->vector (make-list size (car default)))))))

(define (iota n) (let loop ((i 0)) (if (< i n) (cons i (loop (+ i 1))) '())))

(define (expt b n)
    (define (expt-iter b n acc)
        (cond ((= n 0) acc)
                ((= (remainder n 2) 0) ; if n is even
                (expt-iter (* b b) (/ n 2) acc))
                (else ; n is odd
                (expt-iter b (- n 1) (* acc b)))))
        (expt-iter b n 1))(define repetitions 1)
(define %_____junk-data_____ (make-list 0 0))
(define (run-bench-aux run ok?)
  (let loop ((i repetitions) (result '(undefined)))
    (if (> i 0)
        (loop (- i 1) (run))
        (if (ok? result)
            (begin (display "ok\n") result)
            (error "wrong result")))))

(define (run-bench run ok?)
  (time (run-bench-aux run ok?)));;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (my-iota n)
  (do ((n n (- n 1))
       (list '() (cons (- n 1) list)))
      ((zero? n) list)))

(define *size* 511)
(define classmax 3)
(define typemax 12)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-vector (+ classmax 1) 0))
(define *class* (make-vector (+ typemax 1) 0))
(define *piecemax* (make-vector (+ typemax 1) 0))
(define *puzzle* (make-vector (+ *size* 1)))
(define *p* (make-vector (+ typemax 1)))

(define (fit i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((or (> k end)
             (and (vector-ref (vector-ref *p* i) k)
                  (vector-ref *puzzle* (+ j k))))
         (if (> k end) #t #f)))))

(define (place i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
      (cond ((vector-ref (vector-ref *p* i) k)
             (vector-set! *puzzle* (+ j k) #t)
             #t)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (- (vector-ref *piececount* (vector-ref *class* i)) 1))
    (do ((k j (+ k 1)))
        ((or (> k *size*) (not (vector-ref *puzzle* k)))
         (if (> k *size*) 0 k)))))

(define (puzzle-remove i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
      (cond ((vector-ref (vector-ref *p* i) k)
             (vector-set! *puzzle* (+ j k) #f)
             #f)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (+ (vector-ref *piececount* (vector-ref *class* i)) 1))))

(define (trial j)
  (let ((k 0))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (+ i 1)))
           ((> i typemax) (set! *kount* (+ *kount* 1)) #f)
         (cond
          ((not
            (zero?
             (vector-ref *piececount* (vector-ref *class* i))))
           (cond
            ((fit i j)
             (set! k (place i j))
             (cond
              ((or (trial k) (zero? k))
               (set! *kount* (+ *kount* 1))
               (return #t))
              (else (puzzle-remove i j))))))))))))

(define (definePiece iclass ii jj kk)
  (let ((index 0))
    (do ((i 0 (+ i 1)))
        ((> i ii))
      (do ((j 0 (+ j 1)))
          ((> j jj))
        (do ((k 0 (+ k 1)))
            ((> k kk))
          (set! index (+ i (* *d* (+ j (* *d* k)))))
          (vector-set! (vector-ref *p* *iii*) index  #t))))
    (vector-set! *class* *iii* iclass)
    (vector-set! *piecemax* *iii* index)
    (cond ((not (= *iii* typemax))
           (set! *iii* (+ *iii* 1))))))

(define (start size)
  (set! *kount* 0)
  (do ((m 0 (+ m 1)))
      ((> m size))
    (vector-set! *puzzle* m #t))
  (do ((i 1 (+ i 1)))
      ((> i 5))
    (do ((j 1 (+ j 1)))
        ((> j 5))
      (do ((k 1 (+ k 1)))
          ((> k 5))
        (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f))))
  (do ((i 0 (+ i 1)))
      ((> i typemax))
    (do ((m 0 (+ m 1)))
        ((> m size))
      (vector-set! (vector-ref *p* i) m #f)))
  (set! *iii* 0)
  (definePiece 0 3 1 0)
  (definePiece 0 1 0 3)
  (definePiece 0 0 3 1)
  (definePiece 0 1 3 0)
  (definePiece 0 3 0 1)
  (definePiece 0 0 1 3)

  (definePiece 1 2 0 0)
  (definePiece 1 0 2 0)
  (definePiece 1 0 0 2)

  (definePiece 2 1 1 0)
  (definePiece 2 1 0 1)
  (definePiece 2 0 1 1)

  (definePiece 3 1 1 1)

  (vector-set! *piececount* 0 13)
  (vector-set! *piececount* 1 3)
  (vector-set! *piececount* 2 1)
  (vector-set! *piececount* 3 1)
  (let ((m (+ (* *d* (+ *d* 1)) 1))
        (n 0))
    (cond ((fit 0 m) (set! n (place 0 m)))
          (else (begin (newline) (display "Error."))))
    (if (trial n)
        *kount*
        #f)))

(for-each (lambda (i) (vector-set! *p* i (make-vector (+ *size* 1))))
          (my-iota (+ typemax 1)))

(run-bench
    (lambda () (start (hide 51 211)))
    (lambda (r) (equal? r 2005)))
((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
;;;bench-run: -l define-macro -l r4rs
;;;expected:
;;;ok
