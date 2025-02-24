(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(define x (lambda () 43))
(set! x (lambda () 42))

(##putchar (x))
(##putchar 10)

;;;flat-closure-run: -l max
;;;flat-closure-run:
;;;expected:
;;;*
