(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(let ((x 42))
  (let ((y (lambda () x)))
    (##putchar (y))))

(##putchar 10)

;;;flat-closures-run: -l max
;;;flat-closures-run:
;;;expected:
;;;*
