(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(let ((x 40))
  (let ((y 3))
    (let ((z (lambda () (##+ x y))))
      (set! x 20)
      (set! y 22)
      (##putchar (z)))))

(##putchar 10)

;;;flat-closure-run: -l max
;;;flat-closure-run:
;;;expected:
;;;*
