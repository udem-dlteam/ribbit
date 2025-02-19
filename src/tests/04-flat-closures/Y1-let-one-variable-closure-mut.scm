(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(let ((x 43))
  (let ((y (lambda () x)))
    (set! x 42)
    (##putchar (y))))


(##putchar 10)

;;;flat-closures-run: -l max
;;;flat-closures-run:
;;;expected:
;;;*
