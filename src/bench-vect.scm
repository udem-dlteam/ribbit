
(cond-expand
  ((host py)

   (define-feature py/time
      ((import "import time")))

   (define-primitive (get-time)
                     (use py/time)
                     "lambda : push(time.time_ns()),")))

(define (show-time name start-time end-time)
  (display name)
  (display ": ")
  (display (quotient (- end-time start-time) 1000000))
  (display " milliseconds")
  (newline))

(define (time name f)
  (define start-time (get-time))
  (define val (f))
  (define end-time (get-time))
  (show-time name start-time end-time)
  val)


(define (vector-reverse! vect)
 (let loop ((i 0) (j (- (vector-length vect) 1)))
   (if (< i j)
       (let ((t (vector-ref vect i)))
         (vector-set! vect i (vector-ref vect j))
         (vector-set! vect j t)
         (loop (+ i 1) (- j 1))))))

(define (reverse-bench vect vreverse max-iter)
  (let loop ((len 0))
    (if (<= len max-iter)
      (begin
        (vreverse vect)
        (loop (+ len 1))))))


;(define (bench vect)
;  (let loop ((len 0))
;   (if (<= len max-len)
;       (let ((vect test-vector))
;  ;;        (if (> len 0) ;; mutate a random location
;  ;;            (vect-set! vect (random-integer len) 99))
;         (vector-reverse! vect)
;         ;(write (vector->list vect))
;         ;(newline)
;         (loop (+ len 1))))))


(let loop ((max-lens '(100 200 500 1000 2000 5000 10000 20000 50000)))

  (define max-len (car max-lens))
  
  
  (define test-vect (make-vect max-len (lambda (index) index)))
  
  (display " -- ITER ") (display max-len) (display " -- ") (newline)
  
  (define tree-vect
    (time "Tree vect creation"
          (lambda ()
            (make-vect max-len (lambda (index) index)))))
  
  (define linear-vect
    (time "Linear vect creation"
          (lambda ()
            (make-vector max-len))))
  
  ;(write "Tree vect: ") (write (vect->list tree-vect)) (newline)
  ;(write "Linear vect: ") (write (vector->list linear-vect)) (newline)
  
  ;(display " -- One reverse bench -- ") (newline)
  
  (time "Tree vect reverse one"
        (lambda ()
          (vect-reverse! tree-vect)))
  
  (time "Linear vect reverse one"
        (lambda ()
          (vector-reverse! linear-vect)))

  (if (null? (cdr max-lens))
      (begin
        (display " -- No more iterations -- ") (newline)
        (##exit 0)))

  (loop (cdr max-lens))

  )




(display " -- reverse-bench -- ") (newline)

(time "Tree vect reverse"
      (lambda ()
        (reverse-bench tree-vect vect-reverse! max-len)))

(time "Linear vect reverse"
      (lambda ()
        (reverse-bench linear-vect vector-reverse! max-len)))


;(let loop ((len 0))
; (if (<= len max-len)
;     (let ((vect test-vect))
;
;;;        (if (> len 0) ;; mutate a random location
;;;            (vect-set! vect (random-integer len) 99))
;
;       (vect-reverse! vect)
;
;       ;(write (vect->list vect))
;       ;(newline)
;
;       (loop (+ len 1)))))


