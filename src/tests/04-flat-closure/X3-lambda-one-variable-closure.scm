(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(define z
  (lambda (x)
    (lambda () x)))

(define zz (z 42))

(%%putchar (zz))

(%%putchar 10)

;;;flat-closure-run: -l max
;;;flat-closure-run:
;;;expected:
;;;*
