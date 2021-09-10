(define imap (lambda (f l r)
               (if (not (pair? l))
                 (reverse r)
                 (imap f (cdr l) (cons (f (car l)) r)))))

(define my-map (lambda (f l)
              (imap f l '())))

(define oor (lambda (a b) (if (a) #t (b))))
(define aand (lambda (a b) (if (a) (b) (= 1 2))))

(define my-> (lambda (x y)
  (not (oor
         (lambda () (< x y))
         (lambda () (= x y))))))


(define my-member
  (lambda (x l)
    (if (not (pair? l))
      #f
      (if (eq? x (car l))
        #t
        (my-member x (cdr l))))))

(define i-my-modulo (lambda ( a b acc)
                      (if (< a b)
                        a
                        (if (< (+ acc b) a)
                          (i-my-modulo a b (+ acc b))
                          (if (= (+ acc b) a)
                            0
                            (- a acc)
                            )))))

(define my-modulo
  (lambda (a b)
    (i-my-modulo a b 0)))

(define my-even?
  (lambda (x)
    (= 0 (my-modulo x 2))))

(define my-odd?
  (lambda (x)
    (not (my-even? x))))

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
      (oor (lambda () (my-member (car lst) (cdr lst)))
           (lambda () (duplicates? (cdr lst)))))))

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
    (cons (length mat) (length (car mat)))))

(define matrix-map
  (lambda (f mat)
    (my-map (lambda (lst) (my-map f lst)) mat)))

(define initial-random 0)

(define next-random
  (lambda (current-random)
    (my-modulo (+ (* current-random 3581) 12751) 131072)))

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
     (my-modulo new-random (length lst)))))

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
  (lambda (n m) ; n and m must be odd
    (if (not (aand (lambda () (my-odd? n)) (lambda () (my-odd? m))))
      'error
      (make-maze-aux
        (make-matrix n m (lambda (i j)
                           (if (aand (lambda () (my-even? i))
                                     (lambda () (my-even? j)))
                             (cons i j)
                             #f)))
        (concat
          (for 0 n (lambda (i)
                     (concat
                       (for 0 m (lambda (j)
                                  (if (eq? (my-even? i) (my-even? j))
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
    (if (eq? cavity-id old-cavity-id)
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
    (my-append (if (aand
                     (lambda () (my-> i 0))
                     (lambda () (matrix-read cave (- i 1) j)))
                 (cons (cons (- i 1) j) '())
                 '())
               (my-append (if (aand
                                (lambda () (< i (- n 1)))
                                (lambda () (matrix-read cave (+ i 1) j)))
                            (cons (cons (+ i 1) j) '())
                            '())
                          (my-append (if (aand
                                           (lambda () (my-> j 0))
                                           (lambda () (matrix-read cave i (- j 1))))
                                       (cons (cons i (- j 1)) '())
                                       '())
                                     (if (aand
                                           (lambda () (< j (- m 1)))
                                           (lambda () (matrix-read cave i (+ j 1))))
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
    (run-n 0 30 0)))


(run)
