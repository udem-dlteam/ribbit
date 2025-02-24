(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))


(define (make-adder x)
  (let* ((z 0)
         (f (lambda (y) (##+ z y))))
    (set! z x)
    f))

(define plus1 (make-adder 1))
(define plus2 (make-adder 2))

(##putchar (plus1 41))
(##putchar (plus2 40))

(##putchar 10)


;;;flat-closure-run: -l max
;;;flat-closure-run:
;;;expected:
;;;**
