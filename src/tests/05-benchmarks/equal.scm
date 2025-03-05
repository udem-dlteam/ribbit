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
  (time (run-bench-aux run ok?)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2007 William D Clinger.
;;
;; Permission to copy this software, in whole or in part, to use this
;; software for any lawful purpose, and to redistribute this software
;; is granted subject to the restriction that all copies made of this
;; software must include this copyright notice in full.
;;
;; I also request that you send me a copy of any improvements that you
;; make to this software so that they may be incorporated within it to
;; the benefit of the Scheme community.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This benchmark tests the R6RS equal? predicate on some fairly
;; large structures of various shapes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a list with n elements, all equal to x.

(define (make-test-list1 n x)
  (if (zero? n)
      '()
      (cons x (make-test-list1 (- n 1) x))))

;; Returns a list of n lists, each consisting of n x's.
;; The n elements of the outer list are actually the same list.

(define (make-test-tree1 n)
  (if (zero? n)
      '()
      (make-test-list1 n (make-test-tree1 (- n 1)))))

;; Returns a list of n elements, as returned by the thunk.

(define (make-test-list2 n thunk)
  (if (zero? n)
      '()
      (cons (thunk) (make-test-list2 (- n 1) thunk))))

;; Returns a balanced tree of height n, with the branching factor
;; at each level equal to the height of the tree at that level.
;; The subtrees do not share structure.

(define (make-test-tree2 n)
  (if (zero? n)
      '()
      (make-test-list2 n (lambda () (make-test-tree2 (- n 1))))))

;; Returns an extremely unbalanced tree of height n.

(define (make-test-tree5 n)
  (if (zero? n)
      '()
      (cons (make-test-tree5 (- n 1))
            'a)))

;; Calls the thunk n times.

(define (iterate n thunk)
  (cond ((= n 1)
         (thunk))
        ((> n 1)
         (thunk)
         (iterate (- n 1) thunk))
        (else #f)))

;; A simple circular list is a worst case for R5RS equal?.

(define (equality-benchmark0 n)
  (let ((x (vector->list (make-vector n 'a))))
    (set-cdr! (list-tail x (- n 1)) x)
    (iterate n (lambda () (equal? x (cdr x))))))

;; DAG with much sharing.
;; 10 is a good parameter for n.

(define (equality-benchmark1 n)
  (let ((x (make-test-tree1 n))
        (y (make-test-tree1 n)))
    (iterate n (lambda () (equal? x y)))))

;; Tree with no sharing.
;; 8 is a good parameter for n.

(define (equality-benchmark2 n)
  (let ((x (make-test-tree2 n))
        (y (make-test-tree2 n)))
    (iterate n (lambda () (equal? x y)))))

;; Flat vectors.
;; 1000 might be a good parameter for n.

(define (equality-benchmark3 n)
  (let* ((x (make-vector n 'a))
         (y (make-vector n 'a)))
    (iterate n (lambda () (equal? x y)))))

;; Shallow lists.
;; 300 might be a good parameter for n.

(define (equality-benchmark4 n)
  (let* ((x (vector->list (make-vector n (make-test-tree2 3))))
         (y (vector->list (make-vector n (make-test-tree2 3)))))
    (iterate n (lambda () (equal? x y)))))

;; No sharing, no proper lists,
;; and deep following car chains instead of cdr.

(define (equality-benchmark5 n . rest)
  (let* ((x (make-test-tree5 n))
         (y (make-test-tree5 n))
         (iterations (if (null? rest) n (car rest))))
    (iterate iterations (lambda () (equal? x y)))))

;; A shorter form of the benchmark above.

(define (equality-benchmark5short n)
  (equality-benchmark5 n 100))

(define (equality-benchmarks n0 n1 n2 n3 n4 n5)
  (and ; (equality-benchmark0 n0) ;; cyclic benchmark does not work on most non-R7RS schemes
       (equality-benchmark1 n1)
       (equality-benchmark2 n2)
       (equality-benchmark3 n3)
       (equality-benchmark4 n4)
       (equality-benchmark5 n5)))

(run-bench
    (lambda () (equality-benchmarks 500000 8 8 1000 400 800))
    (lambda (r) (equal? r #t)))
((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
;;;bench-run: -l define-macro -l r4rs
;;;expected:
;;;ok
