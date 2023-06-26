(define (rib x y z) (##rib x y z))
(define (rib? x) (##rib? x))

(define (id x) (##id x))

(define (close x) (##close x))
(define (arg1 x y) (##arg1 x y))
(define (arg2 x y) (##arg2 x y))
(define (field0 x) (##field0 x))
(define (field1 x) (##field1 x))
(define (field2 x) (##field2 x))
(define (field0-set! x y) (##field0-set! x y))
(define (field1-set! x y) (##field1-set! x y))
(define (field2-set! x y) (##field2-set! x y))

(define (exit code) (##exit code))

;; (define (eqv? x y) (##eqv? x y))
;; (define (< x y) (##< x y))
;; (define (+ x y) (##+ x y))
;; (define (- x y) (##- x y))
;; (define (* x y) (##* x y))
;; (define (quotient x y) (##quotient x y))

(define (getchar) (##getchar))
(define (putchar x) (##putchar x))

