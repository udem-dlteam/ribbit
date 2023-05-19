;; (write (apply + (list 1 2)))

(define (foo x y z)
  (write x)
  (newline)
  (write y)
  (newline)
  (write z)
  (newline))

(apply foo (list 5 2 "3"))


(define (bar x . y)
  (write x)
  (newline)
  (write y))

(apply bar (list 4 (list "aa" "bb" 5) 9))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/control -l r4rs/io
;;;expected:
;;;5
;;;2
;;;"3"
;;;4
;;;(("aa" "bb" 5) 9)
