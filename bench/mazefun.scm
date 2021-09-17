(define my-map
  (lambda (f lst)
    (if (pair? lst)
        (cons (f (car lst)) (my-map f (cdr lst)))
        '())))

(define my-length
  (lambda (lst)
    (if (pair? lst)
        (+ 1 (my-length (cdr lst)))
        0)))

(define my-member
  (lambda (x l)
    (if (not (pair? l))
        #f
        (if (eqv? x (car l))
            #t
            (my-member x (cdr l))))))

(define my-even?
  (lambda (x)
    (= x (* 2 (quotient x 2)))))

(define my-odd?
  (lambda (x)
    (not (my-even? x))))

(define my-remainder
  (lambda (x y)
    (- x (* y (quotient x y)))))

(define my-append
  (lambda (x y)
    (if (not (pair? x))
        y
        (cons (car x) (my-append (cdr x) y)))))

(define foldr
  (lambda (f base lst)
    (if (not (pair? lst))
        base
        (f (car lst) (foldr f base (cdr lst))))))

(define foldl
  (lambda (f base lst)
    (if (not (pair? lst))
        base
        (foldl f (f base (car lst)) (cdr lst)))))

(define for-aux
  (lambda (lo hi)
    (if (< lo hi)
        (cons lo (for-aux (+ lo 1) hi))
        '())))

(define for
  (lambda (lo hi func)
    (my-map func (for-aux lo hi))))

(define concat
  (lambda (lists)
    (foldr my-append '() lists)))

(define list-read
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read (cdr lst) (- i 1)))))

(define list-write
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write (cdr lst) (- i 1) val)))))

(define list-remove-pos
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos (cdr lst) (- i 1))))))

(define duplicates?
  (lambda (lst)
    (if (null? lst)
        #f
        (if (my-member (car lst) (cdr lst))
            #t
            (duplicates? (cdr lst))))))

(define make-matrix
  (lambda (n m init)
    (for 0 n (lambda (i) (for 0 m (lambda (j) (init i j)))))))

(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define matrix-size
  (lambda (mat)
    (cons (my-length mat) (my-length (car mat)))))

(define matrix-map
  (lambda (f mat)
    (my-map (lambda (lst) (my-map f lst)) mat)))

(define initial-random 0)

(define next-random
  (lambda (current-random)
    (my-remainder (+ (* current-random 3581) 12751) 131072)))

(define shuffle-aux-2
  (lambda (lst new-random i)
    (cons (list-read lst i)
          (shuffle-aux (list-remove-pos lst i)
                       new-random))))

(define shuffle-aux-1
  (lambda (lst new-random)
    (shuffle-aux-2
     lst
     new-random
     (my-remainder new-random (my-length lst)))))

(define shuffle-aux
  (lambda (lst current-random)
    (if (not (pair? lst))
        '()
        (shuffle-aux-1 lst (next-random current-random)))))

(define shuffle
  (lambda (lst)
    (shuffle-aux lst initial-random)))

(define make-maze-aux
  (lambda (cave possible-holes)
    (cave-to-maze (pierce-randomly (shuffle possible-holes) cave))))

(define make-maze
  (lambda (n m)                         ; n and m must be odd
    (if (not (if (my-odd? n) (my-odd? m) #f))
        'error
        (make-maze-aux
         (make-matrix n m (lambda (i j)
                            (if (if (my-even? i) (my-even? j) #f)
                                (cons i j)
                                #f)))
         (concat
          (for 0 n (lambda (i)
                     (concat
                      (for 0 m (lambda (j)
                                 (if (eqv? (my-even? i) (my-even? j))
                                     '()
                                     (cons (cons i j) '()))))))))
         ))))

(define cave-to-maze
  (lambda (cave)
    (matrix-map (lambda (x) (if x '_ '*)) cave)))


(define pierce-aux 
  (lambda (i j pos cave)
    (matrix-write cave i j pos)))

(define pierce
  (lambda (pos cave)
    (pierce-aux (car pos) (cdr pos) pos cave)))


(define pierce-randomly-aux
  (lambda (hole possible-holes cave)
    (pierce-randomly (cdr possible-holes)
                     (try-to-pierce hole cave))))

(define pierce-randomly
  (lambda (possible-holes cave)
    (if (not (pair? possible-holes))
        cave
        (pierce-randomly-aux (car possible-holes) possible-holes cave))))

(define try-to-pierce-aux2
  (lambda (i j pos cave ncs)
    (if (duplicates?
         (my-map (lambda (nc) (matrix-read cave (car nc) (cdr nc))) ncs))
        cave
        (pierce pos
                (foldl (lambda (c nc) (change-cavity c nc pos))
                       cave
                       ncs)))))

(define try-to-pierce-aux1
  (lambda (i j pos cave)
    (try-to-pierce-aux2 i j pos cave (neighboring-cavities pos cave))))

(define try-to-pierce
  (lambda (pos cave)
    (try-to-pierce-aux1 (car pos) (cdr pos) pos cave)))


(define change-cavity-aux-2
  (lambda (cavity-id i j cave pos new-cavity-id old-cavity-id)
    (if (eqv? cavity-id old-cavity-id)
        (foldl (lambda (c nc)
                 (change-cavity-aux c nc new-cavity-id old-cavity-id))
               (matrix-write cave i j new-cavity-id)
               (neighboring-cavities pos cave))
        cave)))

(define change-cavity-aux-1 
  (lambda (i j cave pos new-cavity-id old-cavity-id)
    (change-cavity-aux-2
     (matrix-read cave i j)
     i
     j
     cave
     pos
     new-cavity-id
     old-cavity-id)))

(define change-cavity-aux
  (lambda (cave pos new-cavity-id old-cavity-id)
    (change-cavity-aux-1 (car pos) (cdr pos) cave pos new-cavity-id old-cavity-id)))

(define change-cavity
  (lambda (cave pos new-cavity-id)
    (change-cavity-aux cave pos new-cavity-id (matrix-read cave (car pos) (cdr pos)))))

(define neighboring-cavities-aux-2
  (lambda (pos cave size n m i j)
    (my-append (if (if (< 0 i)
                       (matrix-read cave (- i 1) j)
                       #f)
                   (cons (cons (- i 1) j) '())
                   '())
               (my-append (if (if (< i (- n 1))
                                  (matrix-read cave (+ i 1) j)
                                  #f)
                              (cons (cons (+ i 1) j) '())
                              '())
                          (my-append (if (if (< 0 j)
                                             (matrix-read cave i (- j 1))
                                             #f)
                                         (cons (cons i (- j 1)) '())
                                         '())
                                     (if (if (< j (- m 1))
                                             (matrix-read cave i (+ j 1))
                                             #f)
                                         (cons (cons i (+ j 1)) '())
                                         '()))))))

(define neighboring-cavities-aux-1
  (lambda (pos cave size)
    (neighboring-cavities-aux-2
     pos
     cave
     size
     (car size)
     (cdr size)
     (car pos)
     (cdr pos))))

(define neighboring-cavities
  (lambda (pos cave)
    (neighboring-cavities-aux-1 pos cave (matrix-size cave))))

(define run-n
  (lambda (lo hi r)
    (if (< lo hi)
        (run-n (+ 1 lo) hi  (make-maze 11 11))
        r)))

(define run
  (lambda ()
    (run-n 0 150 0)))


(run)
