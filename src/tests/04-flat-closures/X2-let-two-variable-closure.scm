(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(let ((x 41))
  (let ((y 1))
    (let ((z (lambda () (##+ x y))))
      (##putchar (z)))))

(##putchar 10)

;;;flat-closures-run: -l max
;;;flat-closures-run:
;;;expected:
;;;*
