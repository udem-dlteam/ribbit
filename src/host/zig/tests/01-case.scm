(define do-something
  (lambda (i)
    (case i
      ((allo salut)
       'a)
      ((au-revoir bye)
       'b))))

(write (do-something 'allo))
(putchar 10)

;;;options: -l max
;;;expected:
;;;a
