(define-macro (six-five)
  '65)

(define-macro six-six
  (lambda ()
    66))

(##putchar (six-five))
(##putchar (six-six))

(define foo
  (lambda ()
    69))

(let ((foo (lambda () 65)))

  (define-macro (foo)
    '67)

  (##putchar (foo)))


(let ()
  (define-macro (six-eight)
    65)

  (define (six-eight)
    68)

  (##putchar (six-eight)))

(##putchar (foo)) ;; should be 69


(define-macro (add-positive x y)
   (if (> x 0)
     `(add-positive ,(- x 1) ,(+ y 1))
     y))

(##putchar (add-positive 5 65))

(##putchar 10)  


;;;variadics-run: -l define-macro -f+ arity-check
;;;run: -l define-macro
;;;expected:
;;;ABCDEF
