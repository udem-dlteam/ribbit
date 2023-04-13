(write (apply (lambda (x y z) (+ (+ x y) z)) (list 1 2 3)))
(putchar 10)

;;;options: -l max
;;;fancy-compiler
;;;input:
;;;expected:
;;;6
