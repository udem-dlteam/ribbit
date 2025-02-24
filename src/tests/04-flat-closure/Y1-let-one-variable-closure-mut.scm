(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(let ((x 43))
  (let ((y (lambda () x)))
    (set! x 42)
    (##putchar (y))))


(##putchar 10)

;;;flat-closure-run: -l max
;;;flat-closure-run:
;;;expected:
;;;*
