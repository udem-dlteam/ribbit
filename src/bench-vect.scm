
(define (vector-reverse! vect)
 (let loop ((i 0) (j (- (vector-length vect) 1)))
   (if (< i j)
       (let ((t (vector-ref vect i)))
         (vector-set! vect i (vector-ref vect j))
         (vector-set! vect j t)
         (loop (+ i 1) (- j 1))))))

(define max-len 50)

(display "init")
(newline)
(define test-vect (make-vect (+ 50 max-len) (lambda (index) (+ 50 index))))
(define test-vector (make-vector (+ 50 max-len)))

(display "Testing with vectors")
(newline)
(let loop ((len 0))
 (if (<= len max-len)
     (let ((vect test-vector))

;;        (if (> len 0) ;; mutate a random location
;;            (vect-set! vect (random-integer len) 99))

       (vector-reverse! vect)

       ;(write (vector->list vect))
       ;(newline)

       (loop (+ len 1)))))


(display "Testing with tree vectors")
(newline)
(let loop ((len 0))
 (if (<= len max-len)
     (let ((vect test))

;;        (if (> len 0) ;; mutate a random location
;;            (vect-set! vect (random-integer len) 99))

       (vect-reverse! vect)

       ;(vect->list vect)
       ;(write test2)

       (loop (+ len 1)))))

