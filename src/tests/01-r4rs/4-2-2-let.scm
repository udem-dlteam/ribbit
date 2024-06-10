(display (let ((x 2) (y 3)) (* x y)))
(newline)

(display (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
(newline)

(display (letrec ((even?
                    (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
                  (odd?
                    (lambda (n) (if (zero? n) #f (even? (- n 1))))))
           (even? 88)))
(newline)

(define x 34)
(display (let ((x 3)) (define x 5) x))
(newline)

(display x)
(newline)

(display (letrec ((x 3)) (define x 10) x))
(newline)

(display x)
(newline)

(define (s x) (if x (let () (set! s x) (set! x s))))

;;;r4rs-run: -l r4rs
;;;expected:
;;;6
;;;35
;;;#t
;;;5
;;;34
;;;10
;;;34
