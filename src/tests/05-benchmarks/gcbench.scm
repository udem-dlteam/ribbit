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
  (time (run-bench-aux run ok?)))(define (run-benchmark2 name thunk)
(display name)
(newline)
(thunk))

(define (PrintDiagnostics)
(display " Total memory available= ???????? bytes")
(display "  Free memory= ???????? bytes")
(newline))

(define (gcbench kStretchTreeDepth)

;;  Nodes used by a tree of a given size
(define (TreeSize i)
  (- (expt 2 (+ i 1)) 1))

;;  Number of iterations to use for a given tree depth
(define (NumIters i)
  (quotient (* 2 (TreeSize kStretchTreeDepth))
            (TreeSize i)))

;;  Parameters are determined by kStretchTreeDepth.
;;  In Boehm's version the parameters were fixed as follows:
;;    public static final int kStretchTreeDepth    = 18;  // about 16Mb
;;    public static final int kLongLivedTreeDepth  = 16;  // about 4Mb
;;    public static final int kArraySize  = 500000;       // about 4Mb
;;    public static final int kMinTreeDepth = 4;
;;    public static final int kMaxTreeDepth = 16;
;;  In Larceny the storage numbers above would be 12 Mby, 3 Mby, 6 Mby.

(let* ((kLongLivedTreeDepth (- kStretchTreeDepth 2))
       (kArraySize          (* 4 (TreeSize kLongLivedTreeDepth)))
       (kMinTreeDepth       4)
       (kMaxTreeDepth       kLongLivedTreeDepth))

  ;; Elements 3 and 4 of the allocated records are useless.
  ;; They're just to take up space, so this will be comparable
  ;; to the Java original.

  ;; Constructor: creates a new classNode record.
    (define (make-node-raw left right i j)
    (vector 'classNode left right i j))

    ;; Predicate: tests whether an object is a classNode.
    (define (classNode? obj)
    (and (vector? obj)
        (eq? (vector-ref obj 0) 'classNode)))

    ;; Selectors: retrieve the fields from a classNode record.
    (define (node.left obj)
    (if (classNode? obj)
        (vector-ref obj 1)
        (error "node.left: not a classNode" obj)))

    (define (node.right obj)
    (if (classNode? obj)
        (vector-ref obj 2)
        (error "node.right: not a classNode" obj)))

    (define (node.i obj)
    (if (classNode? obj)
        (vector-ref obj 3)
        (error "node.i: not a classNode" obj)))

    (define (node.j obj)
    (if (classNode? obj)
        (vector-ref obj 4)
        (error "node.j: not a classNode" obj)))

    ;; Mutators: update the fields in a classNode record.
    (define (node.left-set! obj new)
    (if (classNode? obj)
        (vector-set! obj 1 new)
        (error "node.left-set!: not a classNode" obj)))

    (define (node.right-set! obj new)
    (if (classNode? obj)
        (vector-set! obj 2 new)
        (error "node.right-set!: not a classNode" obj)))

    (define (node.i-set! obj new)
    (if (classNode? obj)
        (vector-set! obj 3 new)
        (error "node.i-set!: not a classNode" obj)))

    (define (node.j-set! obj new)
    (if (classNode? obj)
        (vector-set! obj 4 new)
        (error "node.j-set!: not a classNode" obj)))

  (display "Don't listen to what the logs say, the benchmark was adapted for Ribbit")
  (newline)

  (let ((make-empty-node (lambda () (make-node-raw 0 0 0 0)))
    (make-node (lambda (l r) (make-node-raw l r 0 0))))

    ;;  Build tree top down, assigning to older objects.
    (define (Populate iDepth thisNode)
      (if (<= iDepth 0)
          #f
          (let ((iDepth (- iDepth 1)))
            (node.left-set! thisNode (make-empty-node))
            (node.right-set! thisNode (make-empty-node))
            (Populate iDepth (node.left thisNode))
            (Populate iDepth (node.right thisNode)))))

    ;;  Build tree bottom-up
    (define (MakeTree iDepth)
      (if (<= iDepth 0)
          (make-empty-node)
          (make-node (MakeTree (- iDepth 1))
                     (MakeTree (- iDepth 1)))))

    (define (TimeConstruction depth)
      (let ((iNumIters (NumIters depth)))
        (display (string-append "Creating "
                                (number->string iNumIters)
                                " trees of depth "
                                (number->string depth)))
        (newline)
        (run-benchmark2
         "GCBench: Top down construction"
         (lambda ()
           (do ((i 0 (+ i 1)))
               ((>= i iNumIters))
             (Populate depth (make-empty-node)))))
        (run-benchmark2
         "GCBench: Bottom up construction"
         (lambda ()
           (do ((i 0 (+ i 1)))
               ((>= i iNumIters))
             (MakeTree depth))))))

    (define (run-benchmark-x)
      (display "Garbage Collector Test")
      (newline)
      (display (string-append
                " Stretching memory with a binary tree of depth "
                (number->string kStretchTreeDepth)))
      (newline)
      (PrintDiagnostics)
      (run-benchmark2
       "GCBench: Main"
       (lambda ()
         ;;  Stretch the memory space quickly
         (MakeTree kStretchTreeDepth)

         ;;  Create a long lived object
         (display (string-append
                   " Creating a long-lived binary tree of depth "
                   (number->string kLongLivedTreeDepth)))
         (newline)
         (let ((longLivedTree (make-empty-node)))
           (Populate kLongLivedTreeDepth longLivedTree)

           ;;  Create long-lived array, filling half of it
           (display (string-append
                     " Creating a long-lived array of "
                     (number->string kArraySize)
                     " inexact reals"))
           (newline)
           (let ((array (make-vector kArraySize 0)))
             (do ((i 0 (+ i 1)))
                 ((>= i (quotient kArraySize 2)))
               (vector-set! array i (/ 100 (+ i 1))))
             (PrintDiagnostics)

             (do ((d kMinTreeDepth (+ d 2)))
                 ((> d kMaxTreeDepth))
               (TimeConstruction d))

             (if (or (eq? longLivedTree '())
                     (let ((n (min 1000
                                   (- (quotient (vector-length array)
                                                2)
                                      1))))
                       (not (= (vector-ref array n)
                               (/ 100 (+ n 1))))))
                 (begin (display "Failed") (newline)))
             ;;  fake reference to LongLivedTree
             ;;  and array
             ;;  to keep them from being optimized away
             ))))
      (PrintDiagnostics))

    (run-benchmark-x))))

(run-bench
    (lambda () (gcbench 10))
    (lambda (r) #t))
((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)
