(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(define z 
  (lambda (x y)
    (lambda () (##+ x y))))

(define zz (z 40 2))

(##putchar (zz))

(##putchar 10)
 
;;;flat-closures-run: -l max
;;;flat-closures-run:
;;;expected:
;;;*
