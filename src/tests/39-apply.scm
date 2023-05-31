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
  y)

(write (apply bar (list 4 (list "aa" "bb" 5) 9)))
(newline)

(define (##map proc lst)
  (if (pair? lst)
    (cons (proc (car lst)) (##map proc (cdr lst)))
    '()))

(define (map proc . lsts)
  (if (pair? (car lsts))
    (cons (apply proc (##map car lsts))
          (apply map (append (list proc) (##map cdr lsts))))
    '()))


(display (map + '(1 2 3) '(4 5 6) '(7 8 9)))
(newline)

(display (apply apply (list map (list + '(1 2 3) '(4 5 6) '(7 8 9)))))
(newline)

(display (call/cc (lambda (c) (apply c (list 42)))))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/control -l r4rs/io -l r4rs/number
;;;expected:
;;;5
;;;2
;;;"3"
;;;4
;;;(("aa" "bb" 5) 9)
;;;(12 15 18)
;;;(12 15 18)
;;;42
