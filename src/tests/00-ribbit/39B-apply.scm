(cond-expand
  ((host rvm)
   ;; Skip rvm host (spitting out the ribn)
   (begin))
  (else
    (##include-once (ribbit "prim-apply"))))

(define (apply f args) (##apply f args))

(define (##map proc lst)
  (if (pair? lst)
    (cons (proc (##field0 lst)) (##map proc (##field1 lst)))
    '()))

(define (map proc . lsts)
  (if (pair? (##field0 lsts))
    (cons (apply proc (##map car lsts))
          (apply map (append (list proc) (##map cdr lsts))))
    '()))

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

(define (plus . args)
  (if (pair? args)
    (##+ (car args) (apply plus (cdr args)))
    0))

(display (map plus '(1 2 3) '(4 5 6) '(7 8 9)))
(newline)

(display (apply apply (list map (list plus '(1 2 3) '(4 5 6) '(7 8 9)))))
(newline)

(display (call/cc (lambda (c) (apply c (list 42)))))
(newline)

;;;apply-run: -l max
;;;expected:
;;;5
;;;2
;;;"3"
;;;4
;;;(("aa" "bb" 5) 9)
;;;(12 15 18)
;;;(12 15 18)
;;;42
